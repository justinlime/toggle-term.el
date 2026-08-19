;;; toggle-term.el --- Quickly toggle persistent term and shell buffers  -*- lexical-binding:t -*-
;;
;; Author: justinlime
;; URL: https://github.com/justinlime/toggle-term.el
;; Version: 1.3
;; Keywords: frames convenience terminals
;; Package-Requires: ((emacs "25.1"))
;;
;;; License
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;; toggle-term.el allows you to quickly spawn persistent `term', `ghostel',
;; `vterm', `eat', `shell', `eshell', or `ielm' instances on the fly in an
;; unobstructive way.
;;
;;; Code:

(defgroup toggle-term nil
  "Toggle a `term', `vterm', `ghostel', `eat', `shell', `eshell', or `ielm' buffer."
  :prefix "toggle-term-"
  :group 'applications)

(defface toggle-term-name-face
  '((((class color) (background dark)) :foreground "SkyBlue")
    (((class color) (background light)) :foreground "Blue")
    (t :inherit default))
  "Face used for the toggle name in completion candidates.
Bright blue on dark backgrounds, dark blue on light ones."
  :group 'toggle-term)

(defface toggle-term-type-face
  '((((class color) (background dark)) :foreground "SpringGreen")
    (((class color) (background light)) :foreground "ForestGreen")
    (t :inherit default))
  "Face used for the toggle type column in the marginalia annotation.
Shows the type (term, vterm, ghostel, ...) of each toggle in green."
  :group 'toggle-term)

(defcustom toggle-term-size 40
  "Percentage of the window that the toggle-term buffer occupies."
  :type 'fixnum
  :group 'toggle-term)

(defcustom toggle-term-remember-resize t
  "Whether to remember the toggle window's size when it is hidden.
When non-nil, the size a toggle window had when it was hidden -- for
example after being resized by the user while it was open -- is
remembered and reapplied when the toggle is shown again.  The size is
remembered per toggle buffer and per side (`toggle-term-side'), so
toggles that were sized differently each restore their own.
When nil, the window is always (re)created sized
according to `toggle-term-size'."
  :type 'boolean
  :group 'toggle-term)

(defcustom toggle-term-side 'bottom
  "Side of the toggle."
  :type '(choice (const left)
                 (const right)
                 (const top)
                 (const bottom))
  :group 'toggle-term)

(defcustom toggle-term-switch-upon-toggle t
  "Whether or not to switch to the buffer upon toggle."
  :type 'boolean
  :group 'toggle-term)

(defcustom toggle-term-use-persp (when (and (boundp 'persp-mode) (eq persp-mode t)) t)
  "Whether or not to integrate with perspective.el."
  :type 'boolean
  :group 'toggle-term)

(defcustom toggle-term-spawn-hook nil
  "A hook that is run after toggle term spawns a window."
  :type 'hook
  :group 'toggle-term)

(defcustom toggle-term-close-hook nil
  "A hook that is run after toggle term closes a window."
  :type 'hook
  :group 'toggle-term)

(defvar toggle-term-init-toggle nil
  "An predefined toggle-term for startup, invoked when using `toggle-term-toggle'.
Set to a cons cell, of NAME and TYPE, such as '(\"init-toggle-name\" . \"term\")")

(defvar toggle-term--active-toggles nil
  "Nested alist of active toggles spawned by toggle-term.")

(defun toggle-term--get-last-used ()
  "Get the last used toggle term alist."
  (car (delq nil (mapcar #'(lambda (tog)
    (when (and (cdr (assoc 'last-used (cdr tog)))
               (if toggle-term-use-persp
                   (string= (persp-current-name) (cdr (assoc 'persp (cdr tog)))) t)) tog))
    toggle-term--active-toggles))))

(defun toggle-term--set-last-used (wrapped type)
  "Set the last used toggle term.
Argument WRAPPED the name, wrapped in asterisks
Argument TYPE type of toggle (term, shell, etc)."
  ;; Unset the last used toggle
  (mapcar #'(lambda (tog)
    (if toggle-term-use-persp
      (when (string= (persp-current-name) (cdr (assoc 'persp (cdr tog))))
        (setcdr (assoc 'last-used (cdr tog)) nil))
      (setcdr (assoc 'last-used (cdr tog)) nil))) toggle-term--active-toggles)
  ;; Set the new active toggle term
  (unless (car (delq nil (mapcar #'(lambda (tog)
            (when (string= (car tog) wrapped)
              (setcdr (assoc 'last-used (cdr tog)) t)
              (when toggle-term-use-persp
                (setcdr (assoc 'persp (cdr tog)) (persp-current-name))))) toggle-term--active-toggles)))
    ;; Add to or create the active `toggle-term--active-toggles' alist
    (if toggle-term--active-toggles
      ; the t's are intetionally evaluated, leaving them quoted has weird side affects
      (add-to-list 'toggle-term--active-toggles `(,wrapped . ((type . ,type)
                                                              (last-used . ,t)
                                                              (persp . ,(when toggle-term-use-persp (persp-current-name))))))
      (setq toggle-term--active-toggles `((,wrapped . ((type . ,type)
                                                       (last-used . ,t)
                                                       (persp . ,(when toggle-term-use-persp (persp-current-name))))))))))

(defun toggle-term--remember-window-size (window)
  "Remember WINDOW's size before it is hidden.
Stores the size in the toggle's `toggle-term--active-toggles' entry
under a `saved-size' key, as (SIDE . SIZE).  SIZE is in lines for
top/bottom sides and columns for left/right sides, and SIDE is the
`toggle-term-side' in effect at hide time, so a changed side on
re-open falls back to `toggle-term-size'.

Only acts when `toggle-term-remember-resize' is non-nil."
  (when toggle-term-remember-resize
    (let* ((buffer-name (buffer-name (window-buffer window)))
           (entry (assoc buffer-name toggle-term--active-toggles)))
      (when entry
        (let* ((props (cdr entry))
               (old (assq 'saved-size props))
               (size (if (memq toggle-term-side '(top bottom))
                         (window-total-height window)
                       (window-total-width window))))
          (if old
              (setcdr old (cons toggle-term-side size))
            (setcdr entry (cons (cons 'saved-size (cons toggle-term-side size))
                                props))))))))

(defun toggle-term--saved-window-size (name)
  "Return the remembered size for toggle NAME, or nil.
Only returns a size when `toggle-term-remember-resize' is non-nil and
the size was saved for the current `toggle-term-side'."
  (when toggle-term-remember-resize
    (let* ((entry (assoc name toggle-term--active-toggles))
           (saved (cdr (assq 'saved-size (cdr entry)))))
      (and saved
           (eq (car saved) toggle-term-side)
           (cdr saved)))))

(defun toggle-term--display-buffer (buffer &optional size)
  "Display a given BUFFER.
SIZE, when non-nil, is an absolute size for the window: a number of
lines for top/bottom sides, or a number of columns for left/right
sides.  When nil, the window is sized from `toggle-term-size' (a
fraction of the frame)."
  (let* ((size (or size (/ toggle-term-size 100.0)))
         (size-spec
          (if (integerp size)
              `((side . ,toggle-term-side)
                (preserve-size . (t . nil))
                (,(if (memq toggle-term-side '(top bottom))
                      'window-height 'window-width)
                 . ,size))
            `((side . ,toggle-term-side)
              (window-height . ,size)
              (window-width . ,size)
              (preserve-size . (t . nil))))))
    (display-buffer-in-side-window buffer size-spec)
    (set-window-dedicated-p (select-window (get-buffer-window buffer)) nil)))

(defun toggle-term--cleanup-active-toggle ()
  "Remove this buffer's entry from `toggle-term--active-toggles' when killed,
so a dead toggle buffer can't be mistaken for a live one."
  (let ((entry (assoc (buffer-name) toggle-term--active-toggles)))
    (when entry
      (setq toggle-term--active-toggles (delq entry toggle-term--active-toggles)))))


(defun toggle-term--spawn (wrapped type)
  "Handles the spawning of a toggle.
Argument WRAPPED the name, wrapped with asterisks.
Argument TYPE type of toggle (term, shell, etc)."
  (let* ((height (window-total-height))
         (temp-buffer (get-buffer-create " temp-toggle-term-buffer"))
         (current (selected-window))
         (size (toggle-term--saved-window-size wrapped)))
    (toggle-term--display-buffer temp-buffer size)
    (if (member wrapped (mapcar #'car toggle-term--active-toggles))
    ;; (if (buffer-live-p (get-buffer wrapped))
      (progn
        (switch-to-buffer wrapped)
        (when toggle-term-use-persp
          (persp-set-buffer wrapped))
        (run-hooks 'toggle-term-spawn-hook))
      (pcase type
        ("term" (switch-to-buffer (make-term wrapped (getenv "SHELL"))))
        ("vterm" (vterm))
        ("ghostel" (ghostel t))
        ("eat" (set-buffer (eat)))
        ("shell" (shell wrapped))
        ("ielm" (ielm wrapped))
        ("eshell" (eshell) (setq-local eshell-buffer-name wrapped))))
    (toggle-term--set-last-used wrapped type)
    ;; Ensure the buffer is renamed properly
    (unless (eq (buffer-name) wrapped)
      (rename-buffer wrapped))
    (add-hook 'kill-buffer-hook #'toggle-term--cleanup-active-toggle nil t)
    (when toggle-term-use-persp
      (persp-set-buffer wrapped))
    (unless toggle-term-switch-upon-toggle (select-window current))
    (run-hooks 'toggle-term-spawn-hook)))

(defun toggle-term-find (&optional name type)
  "Toggle a toggle-term buffer, or create a new one.

If NAME is provided, set the buffer's
name, otherwise prompt for one.

If TYPE is provided, set the buffer's type (term, shell, etc),
otherwise prompt for one."
  (interactive)
  (let* ((name (substring-no-properties
                (or name
                    (completing-read
                     "Name of toggle: "
                     (delq nil (mapcar #'(lambda (tog)
                        (let ((cand (car tog)))
                          (when (if toggle-term-use-persp
                                    (member (get-buffer cand) (persp-buffers (persp-curr)))
                                  t)
                            (propertize cand 'face 'toggle-term-name-face))))
                              toggle-term--active-toggles))))))
         (wrapped (if (member name (mapcar #'car toggle-term--active-toggles))
                      name
                      (if toggle-term-use-persp
                        (format " *%s-%s*" name (persp-current-name))
                        (format " *%s*" name))))
         (last-used (toggle-term--get-last-used))
         (windows (delete-dups (delq nil (mapcar #'(lambda (tog) (get-buffer-window (car tog))) toggle-term--active-toggles))))
         (type (or type (or (cdr (assoc wrapped toggle-term--active-toggles))
                            (completing-read "Type of toggle: "
                              (delq nil (mapcar #'(lambda (type)
                               (when (fboundp type) type)) '(term vterm ghostel eat eshell ielm shell))) nil t)))))
    (if (or (not last-used)
            (not windows))
        (toggle-term--spawn wrapped type)
        (when windows
          (when toggle-term-remember-resize
            (mapc #'toggle-term--remember-window-size windows))
          (mapcar #'delete-window windows)
          (run-hooks 'toggle-term-close-hook))
        (unless (string= (car last-used) wrapped)
          (toggle-term--spawn wrapped type)))))

(defun toggle-term-toggle ()
  "Toggle the last used buffer spawned by toggle-term.
Invokes `toggle-term-find', and provides it with necessary arguments unless
`toggle-term--last-used' is nil, in which case `toggle-term-find' will prompt
the user to choose a name and type."
  (interactive)
  (let* ((last-used (toggle-term--get-last-used))
         (name (car last-used))
         (type (alist-get 'type (cdr last-used)))
         (persp (alist-get 'persp (cdr last-used)))
         (init (when (and toggle-term-init-toggle (not toggle-term--active-toggles))
                 #'(lambda () (toggle-term-find (car toggle-term-init-toggle) (cdr toggle-term-init-toggle))))))
    (if last-used
      (progn
        (if toggle-term-use-persp
          (if (string= (persp-current-name) persp)
            (toggle-term-find name type)
            (if init (funcall init) (toggle-term-find)))
          (toggle-term-find name type)))
      (if init (funcall init) (toggle-term-find)))))

;; Helpers
(defun toggle-term-term ()
  "Spawn a toggle-term term."
  (interactive)
  (toggle-term-find "toggle-term-term" "term"))

(with-eval-after-load 'vterm
  (defun toggle-term-vterm ()
    "Spawn a toggle-term vterm."
    (interactive)
    (toggle-term-find "toggle-term-vterm" "vterm")))

(with-eval-after-load 'ghostel
  (defun toggle-term-ghostel ()
    "Spawn a toggle-term ghostel."
    (interactive)
    (toggle-term-find "toggle-term-ghostel" "ghostel")))

(with-eval-after-load 'eat
  (defun toggle-term-eat ()
    "Spawn a toggle-term eat."
    (interactive)
    (toggle-term-find "toggle-term-eat" "eat")))

(defun toggle-term-shell ()
  "Spawn a toggle-term shell."
  (interactive)
  (toggle-term-find "toggle-term-shell" "shell"))

(defun toggle-term-eshell ()
  "Spawn a toggle-term eshell."
  (interactive)
  (toggle-term-find "toggle-term-eshell" "eshell"))

(defun toggle-term-ielm ()
  "Spawn a toggle-term ielm."
  (interactive)
  (toggle-term-find "toggle-term-ielm" "ielm"))

(defun toggle-term--marginalia-annotate (cand)
  "Annotate toggle-term candidate CAND with its toggle type.
Returns a string with a space-aligned column showing the toggle's
type (term, vterm, ghostel, ...) in `toggle-term-type-face'."
  (let* ((entry (assoc cand toggle-term--active-toggles))
         (type (alist-get 'type (cdr entry))))
    (when type
      ;; Leading space marked for marginalia's column alignment, the
      ;; separator, then the green type.
      (concat (propertize " " 'marginalia--align t)
              (or (bound-and-true-p marginalia-separator) " ")
              (propertize type 'face 'toggle-term-type-face)))))

(with-eval-after-load 'marginalia
  ;; Let marginalia's prompt classifier map the toggle-term-find
  ;; prompt to the `toggle-term' category.
  (add-to-list 'marginalia-prompt-categories
               '("\\<Name of toggle\\>" . toggle-term))
  ;; Register the annotator for that category, preserving marginalia's
  ;; entry shape ((CATEGORY ANNOTATORS... builtin)) and keeping the
  ;; registration idempotent across re-evaluations.
  (let ((entry (assq 'toggle-term marginalia-annotators)))
    (if entry
        (unless (memq #'toggle-term--marginalia-annotate (cdr entry))
          (setcdr entry (cons #'toggle-term--marginalia-annotate (cdr entry))))
      (push (list 'toggle-term #'toggle-term--marginalia-annotate 'builtin)
            marginalia-annotators))))

(provide 'toggle-term)

;;; toggle-term.el ends here.
