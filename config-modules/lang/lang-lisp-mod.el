;;; lang-lisp-mod.el ---                             -*- lexical-binding: t; -*-

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


(straight-use-package 'sly)


;;;; Racket
(straight-use-package 'racket-mode)
(add-hook 'racket-mode-hook #'racket-xp-mode)

;;;; Scheme
(straight-use-package 'geiser)
(straight-use-package 'geiser-chez)
(straight-use-package 'geiser-gambit)
(straight-use-package 'geiser-guile)
(straight-use-package 'geiser-mit)

(straight-use-package 'macrostep-geiser)

(add-hook 'geiser-repl-mode-hook #'macrostep-geiser-setup)
(add-hook 'geiser-mode-hook #'macrostep-geiser-setup)

;;;; Common Lisp
(straight-use-package 'sly)
(setq inferior-lisp-program "sbcl")


(provide 'lang-lisp-mod)
;;; lang-lisp-mod.el ends here
