
(require 'util-functions)

(require 'use-package-mod)
(require 'defaults-mod)
(require 'ui-mod)
(require 'editor-mod)
(require 'completion-mod)
(require 'window-mod)
(require 'nav-mod)
(require 'buffers-mod)
(require 'project-mod)
(require 'lang-mod)
(require 'org-mod)
(require 'app-mod)

;; (straight-use-package 'general)

(with-eval-after-load 'bookmark
  (setq bookmark-fontify nil)
  (setq org-capture-bookmark nil))

(load-theme 'leuven-dark)


(straight-use-package 'hydra)
(straight-use-package 'transient)
(straight-use-package 'which-key)
(which-key-mode 1)


(straight-use-package 'olivetti)
(custom-set-variables
 '(olivetti-body-width 0.85)
 '(olivetti-minimum-body-width 80))

(defvar symex-over-paredit-p t)

(if (not symex-over-paredit-p)
    (require 'paredit-mod)
  (straight-use-package 'symex)
  (require 'symex)

  (symex-initialize)
  (customize-set-variable 'symex-modal-backend 'hydra)
  (global-set-key (kbd "C-'") 'symex-mode-interface))

;;; In-flux

(straight-use-package 'dumb-jump)

;;; config.el ends here
