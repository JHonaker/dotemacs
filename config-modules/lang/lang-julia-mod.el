;;; lang-julia-mod.el ---                            -*- lexical-binding: t; -*-

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

(straight-use-package 'julia-mode)
(straight-use-package 'eglot-jl)
(straight-use-package 'julia-repl)

(with-eval-after-load 'lsp-eglot-mod
  (eglot-jl-init))

(setenv "JULIA_NUM_THREADS" "4")

(add-hook 'julia-mode-hook #'julia-repl-mode)

(provide 'lang-julia-mod)
;;; lang-julia-mod.el ends here
