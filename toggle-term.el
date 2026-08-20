;;; toggle-term.el --- Quickly toggle persistent term and shell buffers  -*- lexical-binding:t -*-
;;
;; Author: justinlime
;; URL: https://github.com/justinlime/toggle-term.el
;; Version: 2.1
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

(defface toggle-term-side-face
  '((((class color) (background dark)) :foreground "gray60")
    (((class color) (background light)) :foreground "gray40")
    (t :inherit default))
  "Face used for the side of each toggle in the name completion
candidates.  A light, unobtrusive gray shown right after the toggle
name, making the side visible and searchable in the minibuffer
without competing with the name or the marginalia type column."
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
  "Side used for newly spawned toggle-term buffers.
Once a toggle is spawned it remembers its own side, so this option
only applies to toggles that have not recorded a side yet -- toggles
created by `toggle-term-term' and friends, or by `toggle-term-toggle'
when no side-specific command like `toggle-term-toggle-left' is used.
`toggle-term-find' prompts for the side when creating a new toggle
interactively, so this option only sets the default for spawns that
were not started from a name prompt."
  :type '(choice (const left)
                 (const right)
                 (const top)
                 (const bottom))
  :group 'toggle-term)

(defcustom toggle-term-types '(term vterm ghostel eat shell eshell ielm)
  "List of toggle types available when `toggle-term-find' creates a new toggle.
The type prompt only offers types in this list, and only those whose
function is actually defined (so, for example, `vterm' only appears
when vterm is installed; if none of the configured types are defined
they are offered anyway).  When the list holds exactly one type,
`toggle-term-find' skips the type prompt entirely and uses that type
directly.

Possible values are `term', `vterm', `ghostel', `eat', `shell',
`eshell', and `ielm'."
  :type '(repeat (choice (const term)
                         (const vterm)
                         (const ghostel)
                         (const eat)
                         (const shell)
                         (const eshell)
                         (const ielm)))
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
  "A predefined toggle-term for startup, invoked when using `toggle-term-toggle'.
May be a cons cell of NAME and TYPE, such as
'(\"init-toggle-name\" . \"term\"), or may also include a side as a
three-element list (NAME TYPE SIDE), where SIDE is `left', `right',
`top', or `bottom', e.g. '(\"init-toggle-name\" \"term\" bottom).
When SIDE is omitted, the toggle is spawned on `bottom'.")

(defvar toggle-term--active-toggles nil
  "Nested alist of active toggles spawned by toggle-term.")

(defvar toggle-term--last-used-tick 0
  "Monotonic counter tracking which toggle was last used.
Incremented on every `toggle-term--set-last-used' call so
`toggle-term--get-last-used' can find the most recently used toggle
both overall and per side.")

(defun toggle-term--get-last-used (&optional side)
  "Return the most recently used toggle, optionally on SIDE.
SIDE is `left', `right', `top', `bottom' or nil for any side.
Entries from other perspectives are ignored when
`toggle-term-use-persp' is non-nil."
  (let ((best nil)
        (best-tick -1))
    (dolist (tog toggle-term--active-toggles)
      (let* ((props (cdr tog))
             (tick (cdr (assoc 'last-used props)))
             (persp-ok (or (not toggle-term-use-persp)
                           (string= (persp-current-name)
                                    (cdr (assoc 'persp props)))))
             (side-ok (or (null side)
                          (eq side (cdr (assoc 'side props))))))
        (when (and (numberp tick)
                   (> tick best-tick)
                   persp-ok side-ok)
          (setq best tog best-tick tick))))
    best))

(defun toggle-term--set-last-used (wrapped type)
  "Mark toggle WRAPPED of TYPE as the most recently used.
WRAPPED is the name, wrapped in asterisks; TYPE is the toggle type
(term, shell, etc).  Creates the entry if it does not exist yet and
records the current perspective when `toggle-term-use-persp' is on."
  (let ((entry (assoc wrapped toggle-term--active-toggles)))
    (unless entry
      (setq entry (cons wrapped `((type . ,type)))
            toggle-term--active-toggles (cons entry toggle-term--active-toggles)))
    (setq toggle-term--last-used-tick (1+ toggle-term--last-used-tick))
    (let ((props (cdr entry))
          (old (assq 'last-used (cdr entry))))
      (if old
          (setcdr old toggle-term--last-used-tick)
        (setcdr entry (cons (cons 'last-used toggle-term--last-used-tick)
                            props))))
    (when toggle-term-use-persp
      (let* ((props (cdr entry))
             (old (assq 'persp props)))
        (if old
            (setcdr old (persp-current-name))
          (setcdr entry (cons (cons 'persp (persp-current-name)) props)))))))

(defun toggle-term--toggle-side (name)
  "Return the side toggle NAME is displayed on.
Falls back to `toggle-term-side' when the toggle has not recorded a
side yet."
  (or (cdr (assq 'side (cdr (assoc name toggle-term--active-toggles))))
      toggle-term-side))

(defun toggle-term--remember-window-size (window)
  "Remember WINDOW's size before it is hidden.
Stores the size in the toggle's `toggle-term--active-toggles' entry
under a `saved-size' key, as (SIDE . SIZE).  SIZE is in lines for
top/bottom sides and columns for left/right sides, and SIDE is the
side the toggle is displayed on, so a changed side on re-open falls
back to `toggle-term-size'.

Only acts when `toggle-term-remember-resize' is non-nil."
  (when toggle-term-remember-resize
    (let* ((buffer-name (buffer-name (window-buffer window)))
           (entry (assoc buffer-name toggle-term--active-toggles)))
      (when entry
        (let* ((side (toggle-term--toggle-side buffer-name))
               (props (cdr entry))
               (old (assq 'saved-size props))
               (size (if (memq side '(top bottom))
                         (window-total-height window)
                       (window-total-width window))))
          (if old
              (setcdr old (cons side size))
            (setcdr entry (cons (cons 'saved-size (cons side size))
                                props))))))))

(defun toggle-term--saved-window-size (name)
  "Return the remembered size for toggle NAME, or nil.
Only returns a size when `toggle-term-remember-resize' is non-nil and
the size was saved for the side the toggle is currently displayed on."
  (when toggle-term-remember-resize
    (let* ((side (toggle-term--toggle-side name))
           (saved (cdr (assq 'saved-size
                             (cdr (assoc name toggle-term--active-toggles))))))
      (and saved
           (eq (car saved) side)
           (cdr saved)))))

(defun toggle-term--display-buffer (buffer &optional size side)
  "Display a given BUFFER.
SIZE, when non-nil, is an absolute size for the window: a number of
lines for top/bottom sides, or a number of columns for left/right
sides.  When nil, the window is sized from `toggle-term-size' (a
fraction of the frame).  SIDE is the side of the frame to display on
and defaults to `toggle-term-side'."
  (let* ((side (or side toggle-term-side))
         (height-p (memq side '(top bottom)))
         (size (or size (/ toggle-term-size 100.0)))
         (size-spec
          (if (integerp size)
              `((side . ,side)
                (preserve-size . (t . nil))
                (,(if height-p 'window-height 'window-width)
                 . ,size))
            `((side . ,side)
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


(defun toggle-term--spawn (wrapped type &optional side)
  "Handles the spawning of a toggle.
Argument WRAPPED the name, wrapped with asterisks.
Argument TYPE type of toggle (term, shell, etc).
SIDE, when non-nil, is the side for a newly created toggle; existing
toggles keep the side they recorded when first spawned, falling back
to `toggle-term-side' when they have none."
  (let* ((height (window-total-height))
         (temp-buffer (get-buffer-create " temp-toggle-term-buffer"))
         (current (selected-window))
         (tog-side (or side (toggle-term--toggle-side wrapped)))
         (size (toggle-term--saved-window-size wrapped)))
    (toggle-term--display-buffer temp-buffer size tog-side)
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
    ;; Record the side this toggle is displayed on so future toggles
    ;; reopen it on the same side, independent of `toggle-term-side'.
    (let* ((entry (assoc wrapped toggle-term--active-toggles))
           (props (cdr entry))
           (old (assq 'side props)))
      (if old
          (setcdr old tog-side)
        (setcdr entry (cons (cons 'side tog-side) props))))
    ;; Ensure the buffer is renamed properly
    (unless (eq (buffer-name) wrapped)
      (rename-buffer wrapped))
    (add-hook 'kill-buffer-hook #'toggle-term--cleanup-active-toggle nil t)
    (when toggle-term-use-persp
      (persp-set-buffer wrapped))
    (unless toggle-term-switch-upon-toggle (select-window current))
    (run-hooks 'toggle-term-spawn-hook)))

(defun toggle-term--strip-side-suffix (str)
  "Return STR without a trailing side suffix such as (left)
appended to candidates by `toggle-term--name-candidates'."
  (let ((side (catch 'toggle-term--side
                (dolist (s '("left" "right" "top" "bottom"))
                  (when (string-suffix-p (format " (%s)" s) str)
                    (throw 'toggle-term--side (format " (%s)" s)))))))
    (if side
        (substring str 0 (- (length str) (length side)))
      str)))

(defun toggle-term--name-candidates ()
  "Return the completion candidates for the name prompt.
Each candidate is an active toggle's name (restricted to the current
perspective when `toggle-term-use-persp' is on) with its side shown
in `toggle-term-side-face' right after the name.  Because the side is
part of the candidate text, typing the name of a side narrows the
candidates to toggles displayed on that side, while name matching
still works as before.  The bare name is stored in the
`toggle-term-name' text property so the selection can be resolved
back to the real toggle."
  (delq nil (mapcar #'(lambda (tog)
                        (let* ((cand (car tog))
                               (side (or (cdr (assq 'side (cdr tog)))
                                         toggle-term-side)))
                          (when (if toggle-term-use-persp
                                    (member (get-buffer cand) (persp-buffers (persp-curr)))
                                  t)
                            (let ((name (propertize cand 'face 'toggle-term-name-face)))
                              (propertize (concat name (propertize (format " (%s)" side)
                                                                    'face 'toggle-term-side-face))
                                          'toggle-term-name cand)))))
                      toggle-term--active-toggles)))

(defun toggle-term-find (&optional name type side)
  "Toggle a toggle-term buffer, or create a new one.

If NAME is provided, set the buffer's
name, otherwise prompt for one.  In the name prompt each toggle's
side is shown next to its name, so the side can also be used to
narrow the candidates (typing a side, e.g. left, matches toggles displayed on
the left).

When creating a new toggle interactively -- that is, when both NAME
and SIDE are nil and the name is read from the user -- prompt for the
side to spawn it on (left, right, top, or bottom) right after the
name prompt.

If TYPE is provided, set the buffer's type (term, shell, etc),
otherwise prompt for one.  The type prompt only offers the types in
`toggle-term-types', and when that list contains exactly one type the
prompt is skipped and that type is used directly.

If SIDE is provided (left/right/top/bottom), a newly created toggle is
spawned on that side; existing toggles keep the side they were spawned
on.  Toggling a visible toggle off hides only that toggle's window, so
toggles on different sides can be shown at the same time."
  (interactive)
  (let* ((name-given name)
         (chosen (or name
                     (completing-read "Name of toggle: "
                                       (toggle-term--name-candidates))))
         (name (substring-no-properties
                (toggle-term--strip-side-suffix
                 (or (get-text-property 0 'toggle-term-name chosen)
                     chosen))))
         (wrapped (if (member name (mapcar #'car toggle-term--active-toggles))
                      name
                      (if toggle-term-use-persp
                        (format " *%s-%s*" name (persp-current-name))
                        (format " *%s*" name))))
         (side (or side
                   (unless (or name-given (assoc wrapped toggle-term--active-toggles))
                     (intern (completing-read
                              "Side of toggle: "
                              '("left" "right" "top" "bottom") nil t)))))
         (type (or type
                   (alist-get 'type (cdr (assoc wrapped toggle-term--active-toggles)))
                   (let* ((types (mapcar #'(lambda (type)
                                             (if (symbolp type)
                                                 (symbol-name type)
                                               type))
                                         toggle-term-types))
                          ;; Only offer types whose function is defined,
                          ;; so uninstalled packages don't show up; when
                          ;; that leaves nothing, offer the configured
                          ;; types anyway rather than an empty prompt.
                          (usable (delq nil (mapcar #'(lambda (type)
                                                        (when (fboundp (intern type))
                                                          type))
                                                    types))))
                     (if (= (length types) 1)
                         (car types)
                       (completing-read "Type of toggle: "
                                        (or usable types) nil t)))))
         (win (get-buffer-window wrapped)))
    (if win
        ;; Toggle off: hide only this toggle's window, so other toggles
        ;; on different sides stay visible.
        (progn
          (when toggle-term-remember-resize
            (toggle-term--remember-window-size win))
          (delete-window win)
          (run-hooks 'toggle-term-close-hook))
      ;; Toggle on: (re)display on the toggle's own side.  An existing
      ;; side window on that side is reused, so two toggles cannot
      ;; occupy the same side at once.
      (toggle-term--spawn wrapped type side))))

(defun toggle-term--toggle (side)
  "Toggle the most recently used toggle on SIDE.
SIDE is `left', `right', `top' or `bottom', or nil for the most
recently used toggle overall.  If no matching toggle exists yet, fall
back to `toggle-term-init-toggle' or prompt for a name and type,
spawning the new toggle on SIDE (or `toggle-term-side' when SIDE is
nil).  When `toggle-term-init-toggle' specifies its own side, that
side is used; otherwise the toggle is spawned on `bottom'."
  (let* ((last-used (toggle-term--get-last-used side))
         (name (car last-used))
         (type (alist-get 'type (cdr last-used)))
         (init (when (and toggle-term-init-toggle (not toggle-term--active-toggles))
                 (let* ((rest (cdr toggle-term-init-toggle))
                        ;; `rest' is TYPE for the (NAME . TYPE) form, or
                        ;; (TYPE SIDE) for the (NAME TYPE SIDE) form.  An
                        ;; init toggle never prompts: it uses its own side
                        ;; if given, otherwise defaults to `bottom'.
                        (init-type (if (consp rest) (car rest) rest))
                        (init-side (or (and (consp rest) (cadr rest)) 'bottom)))
                   #'(lambda () (toggle-term-find (car toggle-term-init-toggle)
                                                  init-type init-side))))))
    (if last-used
        (toggle-term-find name type side)
      (if init (funcall init) (toggle-term-find nil nil side)))))

(defun toggle-term-toggle ()
  "Toggle the most recently used buffer spawned by toggle-term.
Invokes `toggle-term-find' with the last used toggle, whatever side
it is on; if no toggle has been used yet, prompts for a name and
type.

When point is inside a toggle-term window, this is overridden: that
toggle is marked as the most recently used and its window is hidden,
so a toggle closes the toggle the user is actually in rather than
whatever was used last."
  (interactive)
  (let ((entry (assoc (buffer-name) toggle-term--active-toggles)))
    (if entry
        (let ((name (car entry))
              (type (alist-get 'type (cdr entry))))
          (toggle-term--set-last-used name type)
          (toggle-term-find name type))
      (toggle-term--toggle nil))))

(defun toggle-term-toggle-left ()
  "Toggle the most recently used left-side toggle, or create one."
  (interactive)
  (toggle-term--toggle 'left))

(defun toggle-term-toggle-right ()
  "Toggle the most recently used right-side toggle, or create one."
  (interactive)
  (toggle-term--toggle 'right))

(defun toggle-term-toggle-top ()
  "Toggle the most recently used top-side toggle, or create one."
  (interactive)
  (toggle-term--toggle 'top))

(defun toggle-term-toggle-bottom ()
  "Toggle the most recently used bottom-side toggle, or create one."
  (interactive)
  (toggle-term--toggle 'bottom))

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
  "Annotate toggle-term candidate CAND with its type.
Appends an aligned column with the toggle's type in
`toggle-term-type-face'.  The side is already shown inline next to
the name in the candidate itself, so it is not repeated here."
  (let* ((entry (assoc (toggle-term--strip-side-suffix cand)
                       toggle-term--active-toggles))
         (type (alist-get 'type (cdr entry))))
    (when type
      ;; Alignment space, then the green type padded to a fixed width.
      (concat (propertize " " 'marginalia--align t)
              (or (bound-and-true-p marginalia-separator) " ")
              (propertize (format "%-8s" type) 'face 'toggle-term-type-face)))))

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
