;;; nav-mod.el ---                                   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  John Honaker

;; Author: John Honaker <john@pop-os>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(straight-use-package 'avy)

(with-eval-after-load 'avy
  (custom-set-variables
   '(avy-style 'at-full)
   '(avy-dispatch-alist ((?m . avy-action-mark)
		        (?  . avy-action-mark-to-char)
		        (?I . avy-action-ispell)
		        (?E . avy-action-embark)
		        (?H . avy-action-helpful)
		        (?z . avy-action-zap-to-char)

		        (?w . avy-action-copy)
		        (?k . avy-action-kill-stay)
		        (?y . avy-action-yank)

		        (?W . avy-action-copy-whole-line)
		        (?K . avy-action-kill-whole-line)
		        (?Y . avy-action-yank-whole-line)))
   '(avy-timeout-seconds 0.4)
   '(avy-background t)))

(defun my/avy-pop-window ()
  "Return to the window that was active before last avy command."
  (select-window
   (cdr (ring-ref avy-ring 0))))

(defun avy-action-embark (pt)
  (cl-letf (((symbol-function 'keyboard-quit)
	     #'abort-recursive-edit))
    (save-excursion
      (goto-char pt)
      (embark-act))
    (my/avy-pop-window))
  t)

(defun avy-action-mark-to-char (pt)
  (activate-mark)
  (goto-char pt))

(defun avy-action-helpful (pt)
  (save-excursion
    (goto-char pt)
    (helpful-at-point))
  (my/avy-pop-window)
  t)

(defun avy-action-kill-line (pt)
  (save-excursion
    (goto-char pt)
    (kill-line))
  (my/avy-pop-window)
  t)

(defun avy-action-kill-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (kill-whole-line))
  (my/avy-pop-window)
  t)

(defun avy-action-copy-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (cl-destructuring-bind (start . end)
	(bounds-of-thing-at-point 'line)
      (copy-region-as-kill start end)))
  (my/avy-pop-window)
  t)

(defun avy-yank-whole-line (pt)
  (avy-action-copy-whole-line pt)
  (save-excursion (yank))
  t)

(defun my/avy-isearch-this-window (&optional arg)
  "Go to isearch candidate in this window only with avy."
  (interactive "P")
  (let ((avy-all-windows nil)
	(current-prefix-arg (if arg 4)))
    (call-interactively 'avy-isearch)))

;;;; Keybindings

(global-set-key (kbd "C-t") 'avy-goto-char-timer)
(define-key isearch-mode-map (kbd "C-t") #'my/avy-search-this-window)

(provide 'nav-mod)
;;; nav-mod.el ends here
