;; -*- lexical-binding: t -*-

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

(straight-use-package '(zk :files (:defaults "zk-consult.el")))
(straight-use-package 'zk-index)

(setq zk-directory "~/org/zk/")
(setq zk-file-extension "md")

(require 'zk)
(zk-setup-embark)

(require 'zk-consult)
(setq zk-grep-function 'zk-consult-grep)
(setq zk-tag-grep-function 'zk-consult-grep-tag-search)

(require 'zk-index)
(zk-index-setup-embark)
(setq zk-index-desktop-directory zk-directory)

;; Luhmann style ids

(straight-use-package '(zk-luhmann :type git
                                   :host github
                                   :repo "localauthor/zk-luhmann"))

(let ((km zk-index-map))
  (define-key km (kbd "L") #'zk-luhmann-index-sort)
  (define-key km (kbd "l") #'zk-luhmann-index)
  (define-key km (kbd "C-f") #'zk-luhmann-index-forward)
  (define-key km (kbd "C-b") #'zk-luhmann-index-back)
  (define-key km (kbd "C-t") #'zk-luhmann-index-unfold)
  (define-key km (kbd "t") #'zk-luhmann-index-top)
  (dolist (n (mapcar #'number-to-string '(1 2 3 4 5 6 7 8 9)))
    (define-key km (kbd n) #'zk-luhmmann-index-level)))

;;; config.el ends here
