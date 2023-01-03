;;; lang-ocaml-mod.el    -*- lexical-binding: t; -*-

(straight-use-package 'tuareg)
(straight-use-package 'dune)
(straight-use-package 'merlin)
(straight-use-package 'merlin-eldoc)
;; (straight-use-package 'utop)

;; Merlin hooks
(add-hook 'tuareg-mode-hook #'merlin-mode)
;; (add-hook 'merlin-mode-hook #'company-mode)

;; Merlin-eldoc hooks
(add-hook 'tuareg-mode-hook #'merlin-eldoc-setup)

;; utop hooks
;; (add-hook 'tuareg-mode-hook #'utop-minor-mode)

(provide 'lang-ocaml-mod)
