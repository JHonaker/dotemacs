;;; paredit-mod.el ---                               -*- lexical-binding: t; -*-

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

(dolist (hook '(emacs-list-mode-hook
		eval-expression-minibuffer-setup-hook
		ielm-mode-hook
		lisp-mode-hook
		lisp-interaction-mode-hook
		scheme-hook
		geiser-mode-hook
		racket-mode-hook))
  (add-hook hook #'enable-paredit-mode))

;;; Keybindings

(with-eval-after-load 'paredit
  (let ((km paredit-mode-map))
    ;; Switch C-backspace to use paredit-backward-kill to prevent
    ;; unbalancing parentheses
    (define-key (kbd "<C-backspace>") 'paredit-backward-kill-word)
    (define-key (kbd "<M-backspace>") 'backward-kill-word)
    ;; Remap the splice and split to not conflict with `search-map'.
    (define-key (kbd "M-s") nil)
    (define-key (kbd "M-S") nil)
    (define-key (kbd "M-s s") 'paredit-splice-sexp)
    (define-key (kbd "M-S S") 'paredit-split-sexp)))

(provide 'paredit-mod)
;;; paredit-mod.el ends here
