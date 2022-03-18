;;; helpful-mod.el --- Helpful is a better Help buffer system  -*- lexical-binding: t; -*-

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

(straight-use-package 'helpful)
(straight-use-package 'elisp-demos)

(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

;; Keybindings

(global-set-key [remap describe-command] #'helpful-command)
(global-set-key [remap describe-function] #'helpful-callable)
(global-set-key [remap describe-key] #'helpful-key)
(global-set-key [remap describe-symbol] #'helpful-symbol)
(global-set-key [remap describe-variable] #'helpful-variable)
(global-set-key (kbd "C-h F") #'helpful-function)

(global-set-key (kbd "C-h K") #'describe-keymap)

(define-key mode-specific-map (kbd "C-d") #'helpful-at-point)

(with-eval-after-load 'embark
  (let ((keymap embark-become-help-map))
    (define-key keymap (kbd "f") #'helpful-callable)
    (define-key keymap (kbd "v") #'helpful-variable)
    (define-key keymap (kbd "C") #'helpful-command)))



(provide 'helpful-mod)
;;; helpful-mod.el ends here
