;;; lang-latex-mod.el ---                            -*- lexical-binding: t; -*-

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

(straight-use-package 'auctex)
(straight-use-package 'cdlatex)


;; AUCTeX

(custom-set-variables
 '(TeX-auto-save t)
 '(TeX-parse-self t)
 '(reftex-plug-into-AUCTeX t))

(add-to-list 'auto-mode-alist '("\\.tex\\'" . TeX-latex-mode))

(add-hook 'LaTeX-mode-hook #'cdlatex-mode)
(add-hook 'LaTeX-mode-hook #'prettify-symbols-mode)
(add-hook 'LaTeX-mode-hook #'TeX-fold-mode)
(add-hook 'LaTeX-mode-hook #'reftex-mode)

(with-eval-after-load 'latex
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  (defvar mentat/latex-preview-scale 1.0)

  (add-hook 'LaTeX-mode-hook
            (defun preview-larger-previews ()
              (setq preview-scale-function
                    (lambda () (* mentat/latex-preview-scale
				  (funcall (preview-scale-from-face))))))))

(with-eval-after-load 'preview
  (add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))

(provide 'lang-latex-mod)
;;; lang-latex-mod.el ends here
