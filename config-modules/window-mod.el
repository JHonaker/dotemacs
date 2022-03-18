;;; window-mod.el ---                                -*- lexical-binding: t; -*-

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

(straight-use-package 'ace-window)
(straight-use-package 'popper)

(winner-mode)

(defun my/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(setq aw-keys '(?a ?s ?d ?f ?j ?k ?l))

;;;;; Popper

(popper-mode)
(popper-echo-mode)

(custom-set-variables
 '(popper-reference-buffers '("\\*Messages\\*"
			      "Output\\*$"
			      "\\*Async Shell Command\\*"	
			      help-mode
			      helpful-mode
			      compilation-mode
			      "\\*.*REPL\\*"))
 '(popper-group-function #'popper-group-by-directory))

(defcustom window-group-prefix-key "C-c w"
  "Configure the prefix key for window management.")


;;;; Keybindings

(define-prefix-command 'window-group-key-map)

(let ((km window-group-key-map))
  (define-key km (kbd "u") 'winner-undo)
  (define-key km (kbd "n") 'windmove-down)
  (define-key km (kbd "p") 'windmove-up)
  (define-key km (kbd "f") 'windmove-right)
  (define-key km (kbd "b") 'windmove-left))

(global-set-key (kbd window-group-prefix-key) 'window-group-key-map)

(define-key ctl-x-map "o" 'ace-window)
(define-key ctl-x-map "1" 'my/toggle-delete-other-windows)

(global-set-key (kbd "C-`") 'popper-toggle-latest)
(global-set-key (kbd "M-`") 'popper-cycle)
(global-set-key (kbd "C-M-`") 'popper-toggle-type)

(provide 'window-mod)
;;; window-mod.el ends here
