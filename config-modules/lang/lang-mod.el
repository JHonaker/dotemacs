;;; lang-mod.el ---                                  -*- lexical-binding: t; -*-

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

(require 'lsp-eglot-mod)
(require 'lang-elisp-mod)
(require 'lang-lisp-mod)
(require 'lang-latex-mod)
(require 'lang-markdown-mod)

(require 'lang-julia-mod)
(require 'lang-python-mod)


;;;; R

(straight-use-package 'ess)
(customize-set-variable 'ess-use-flymake t)

;;;; Haskell

(straight-use-package 'haskell-mode)

;;;; Nix

(straight-use-package 'nix-mode)

;;;; KMonad

(straight-use-package '(kbd-mode :type git
				 :host github
				 :repo "kmonad/kbd-mode"))

;;;; Godot Script

(straight-use-package 'gdscript-mode)
(customize-set-variable 'gdscript-use-tab-indents nil)

(provide 'lang-mod)
;;; lang-mod.el ends here
