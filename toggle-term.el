;;; toggle-term.el --- Quickly toggle persistent term and shell buffers  -*- lexical-binding:t -*-
;;
;; Author: justinlime
;; URL: https://github.com/justinlime/toggle-term.el
;; Version: 0.0.1
;; Keywords: toggle terminal term shell eshell ielm
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
;; toggle-term.el allows you to quickly spawn persistent `term',
;; `shell', `eshell', or `ielm' instances on the fly in an unobstructive way.
;;
;;; Code:

(defgroup toggle-term nil
  "Toggle a `term', `shell', `eshell', or `ielm' buffer"
  :prefix "toggle-term-"
  :group 'applications)

(defcustom toggle-term-size 22
  "Percentage of the window that the toggle-term buffer occupies."
  :type 'fixnum
  :group 'toggle-term)

(defvar toggle-term-active-toggles nil
  "Active toggles spawned by toggle-term.")

(defvar toggle-term-last-used nil
  "The current active toggle to be targeted by `toggle-term-toggle'.")

(defun toggle-term--spawn (name wrapped type)
  (let* ((height (window-total-height))
         (size (round (* height (- 1 (/ toggle-term-size 100.0))))))
    (select-window (split-root-window-below size))
    (dolist (buf (buffer-list))
      (if (string= (buffer-name buf) wrapped)
        (switch-to-buffer wrapped)
        (progn
          (cond
            ((string= type 'term)
               (progn 
                 (make-term name (getenv "SHELL"))
                 (switch-to-buffer wrapped)))
            ((string= type 'shell)
               (shell wrapped))
            ((string= type 'ielm)
               (ielm wrapped))
            ((string= type 'eshell)
               (eshell)
               (setq-local eshell-buffer-name wrapped)))
          (if toggle-term-active-toggles
            (add-to-list 'toggle-term-active-toggles `(,wrapped . ,type))
            (setq toggle-term-active-toggles `((,wrapped . ,type)))))))
    (setq toggle-term-last-used `(,wrapped . ,type))
    (unless (eq (buffer-name (current-buffer)) wrapped)
      (rename-buffer wrapped))))

(defun toggle-term-find (&optional name type)
  "Toggle a buffer spawned by toggle-term, or create a new one.

If NAME is provided, set the buffer's name to NAME, otherwise prompt for one.
If TYPE is provided, set the buffer's type (term, shell, eshell, ielm) to TYPE, otherwise prompt for one."
  (interactive)
  (let* ((name (if name name (completing-read "Name of toggle: " (mapcar 'car toggle-term-active-toggles))))
         (unwrapped-name (replace-regexp-in-string "\\*" "" name))
         (wrapped (format "*%s*" unwrapped-name))
         (type (if type type (if (assoc wrapped toggle-term-active-toggles)
                               (cdr (assoc wrapped toggle-term-active-toggles))
                               (completing-read "Type of toggle: " '(term eshell ielm shell))))))

    (let* ((last (car toggle-term-last-used))
           (win (get-buffer-window last)))
      (if (not last)
        (toggle-term--spawn name wrapped type)
        (if win
          (progn
            (delete-window win)
            (unless (string= last wrapped)
             (toggle-term--spawn name wrapped type)))
          (toggle-term--spawn name wrapped type))))))

(defun toggle-term-toggle ()
  "Toggle the last used buffer spawned by toggle-term.
Invokes `toggle-term-find', and provides it with necessary arguments unless
`toggle-term-last-used' is nil, in which case `toggle-term-find' will prompt
the user to choose a name and type."
  (interactive)
  (if toggle-term-last-used
    (toggle-term-find (car toggle-term-last-used) (cdr toggle-term-last-used))
    (toggle-term-find)))

(provide 'toggle-term)

;;; toggle-term.el ends here
