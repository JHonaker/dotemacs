;;; init.el --- My person configuration.
;;
;; Copyright (C) 2020 John Honaker
;;
;; Author: John Honaker <http://github/john>
;; Maintainer: John Honaker <honaker215@gmail.com>
;; Created: July 25, 2020
;; Modified: July 25, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/john/init
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(provide 'init)
;;; init.el ends here


;; (require 'package)
;; (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
;;                     (not (gnutls-available-p))))
;;        (proto (if no-ssl "http" "https")))
;;   (when no-ssl
;;     (warn "\
;; Your version of Emacs does not support SSL connections,
;; which is unsafe because it allows man-in-the-middle attacks.
;; There are two things you can do about this warning:
;; 1. Install an Emacs version that does support SSL and be safe.
;; 2. Remove this warning from your init file so you won't see it again."))
;;   ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
;;   (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
;;   ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
;;   (when (< emacs-major-version 24)
;;     ;; For important compatibility libraries like cl-lib
;;     (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
;; (package-initialize)


;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; ;; This is only needed once, near the top of the file
;; (eval-when-compile
;;   (require 'use-package))

;; Straight.el bootstrap
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

(setq straight-repository-branch "develop")
(setq straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Themes
;; (let ((themes '( leuven-theme brutalist-theme
;;        challenger-deep-theme darcula-theme dracula-theme
;;        eink-theme espresso-theme flatland-theme gandalf-theme
;;        naysayer-theme northcode-theme tao-theme afternoon-theme
;;        modus-themes humanoid-themes))
;;        (value))
;;   (dolist (theme-name themes value)
;;     (use-package theme-name
;;       :straight t)))

(use-package doom-themes
  :straight t)

(use-package curry-on-theme
  :straight t)

(use-package nano-theme
  :straight (nano-theme :type git
			:host github
			:repo "404cn/nano-theme.el"))

(tool-bar-mode -1)
(scroll-bar-mode -1)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;(load-theme 'doom-old-hope t)
(load-theme 'doom-challenger-deep t)

(global-unset-key [(control z)])
(global-unset-key [(control x) (control z)])

(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

(use-package emacs
  :bind (("C-c e i" . jh/find-user-init-file))
  :init

  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  ;; (defun crm-indicator (args)
  ;;   (cons (concat "[CRM] " (car args)) (cdr args)))
  ;; (advice-add #'completing-read-multiple :filter #'crm-indicator)
  
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28+ - Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
	#'command-completion-default-include-p)
  
  
  (setq enable-recursive-minibuffers t)
  :config

  (server-start)

  (show-paren-mode 1)
  
  (setq backup-directory-alist
	`(("." . ,(concat user-emacs-directory "backups")))
	comp-deferred-compilation t)

  (defun jh/find-user-init-file (&optional open-current-window-p)
    "Edit the `user-init-file', in another window. C-u to open in current window."
    (interactive "P")
    (if open-current-window-p
	(find-file user-init-file)
      (find-file-other-window user-init-file)))

  ;; Some performance things
  (setq gc-cons-threshold 80000000) ;; ~80mb
  (setq read-process-output-max (* 1024 1024)) ;; 1 mb
  
  ;;(set-face-attribute 'default nil :font "Source Code Pro-10")
  (set-face-attribute 'default nil :font "JuliaMono-10")
  (set-face-attribute 'fixed-pitch nil :font "JuliaMono-10")
  (set-face-attribute 'variable-pitch nil :font "Source Code Variable-10"))

(use-package helpful
  :straight t
  :bind (("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)
	 ("C-c C-d" . helpful-at-point)
	 ("C-h F" . helpful-function))
  )

(use-package rainbow-delimiters
  :straight t)

(use-package company
  :straight t
  :hook (after-init . global-company-mode)
  :commands global-company-mode
  :init
  (setq company-idle-delay 0.05
        company-minimum-prefix-length 2)
  
  ;; Specify a global default
  ;;company-backends '(company-capf))
  :config

  (defun john/company-to-yasnippet ()
    "Avoid company completion when using yasnippet. The code is from https://code.lexarcana.com/posts/make-company-yasnippet-play-nice.html"
    (interactive)
    (company-abort)
    (call-interactively 'company-yasnippet))

  (bind-key "<backtab>" 'john/company-to-yasnippet company-active-map)
  (bind-key "<backtab>" 'company-yasnippet)
  (add-to-list 'company-backends 'company-math-symbols-unicode))

(use-package company-math
  :straight t)

(use-package company-auctex
  :straight t
  :after company
  :config
  (company-auctex-init))


(use-package marginalia
  :straight t
  :init
  (marginalia-mode)

  ;; Can switch from light to heavy annotation using this
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)))

(use-package hl-todo
  :straight t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(;; For things that need to be done, just not today.
          ("TODO" warning bold)
          ;; For problems that will become bigger problems later if not
          ;; fixed ASAP.
          ("FIXME" error bold)
          ;; For tidbits that are unconventional and not intended uses of the
          ;; constituent parts, and may break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For things that were done hastily and/or hasn't been thoroughly
          ;; tested. It may not even be necessary!
          ("REVIEW" font-lock-keyword-face bold)
          ;; For especially important gotchas with a given implementation,
          ;; directed at another user other than the author.
          ("NOTE" success bold)
          ;; For things that just gotta go and will soon be gone.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; For a known bug that needs a workaround
          ("BUG" error bold)
          ;; For warning about a problematic or misguiding code
          ("XXX" font-lock-constant-face bold))))

;; Refer to https://github.com/hlissner/doom-emacs/tree/develop/modules/tools/eval
;; Refer to https://github.com/hlissner/doom-emacs/tree/develop/modules/tools/lookup
;;


;; (use-package eglot
;;   :config
;;   (setq eglot-connect-timeout 300))

;; (use-package eglot-jl
;;   :after (eglot julia-mode)
;;   :hook (julia-mode . eglot-jl-init))

(use-package julia-mode
  :straight t
  :config
  (setenv "JULIA_NUM_THREADS" "4"))


(use-package julia-repl
  :straight t
  :after julia-mode
  :hook (julia-mode . julia-repl-mode))

;; (use-package ace-window
;;   :straight t
;;   :bind ("C-x o" . ace-window)
;;   :config
;;   (setq aw-background nil
;; 	aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; (use-package avy
;;   :straight t
;;   :bind (("C-;" . avy-goto-char-timer))
;;   :config
;;   (setq avy-all-windows 'all-frames))

(use-package lsp-mode
  :straight t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-x l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (julia-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(use-package lsp-julia
  :straight t
  :config
  (setq lsp-julia-default-environment "~/.julia/environments/v1.6"))


;; (use-package lsp-mode
;;   :hook ((python-mode . lsp-deferred)
;;          (rust-mode . lsp-deferred))
;;   :commands lsp-deferred
;;   :config
;;   (setq lsp-prefer-flymake nil))

;; (use-package lsp-ui
;;   :commands lsp-ui-mode
;;   :hook (lsp-mode . lsp-ui-mode))


(use-package magit
  :straight t
  :bind (("C-x g" . magit-status)))


(use-package ess
  :straight t
  :init
  ;;(add-to-list 'auto-mode-alist '("\\.jl\\'" . ess-julia-mode))
  :config
  (setq ess-use-flymake nil) ;; Use flycheck instead

  (setq ess-r--no-company-meta t) ;; Hacky fix to be removed from ESS shortly
  )

(use-package markdown-mode
  :straight t
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :init
  (setq markdown-enable-math t ; syntax highlighting for latex fragments
        markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-fontify-code-blocks-natively t
        markdown-gfm-uppercase-checkbox t ; for compat with org-mode
        markdown-gfm-additional-languages '("sh")
        markdown-make-gfm-checkboxes-buttons t

        ;; `+markdown-compile' offers support for many transpilers (see
        ;; `+markdown-compile-functions'), which it tries until one succeeds.
        markdown-command #'+markdown-compile
        ;; This is set to `nil' by default, which causes a wrong-type-arg error
        ;; when you use `markdown-open'. These are more sensible defaults.
        markdown-open-command "xdg-open"

        ;; A sensible and simple default preamble for markdown exports that
        ;; takes after the github asthetic (plus highlightjs syntax coloring).
        markdown-content-type "application/xhtml+xml"
        markdown-css-paths
        '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
          "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css")
        markdown-xhtml-header-content
        (concat "<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>"
                "<style> body { box-sizing: border-box; max-width: 740px; width: 100%; margin: 40px auto; padding: 0 10px; } </style>"
                "<script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>"
                "<script>document.addEventListener('DOMContentLoaded', () => { document.body.classList.add('markdown-body'); document.querySelectorAll('pre[lang] > code').forEach((code) => { code.classList.add(code.parentElement.lang); }); document.querySelectorAll('pre > code').forEach((code) => { hljs.highlightBlock(code); }); });</script>"))
  )


(use-package auctex
  :straight t
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook ((LaTeX-mode . TeX-fold-mode)
	 (LaTeX-mode . LaTeX-math-mode))

  :config
  (setq TeX-auto-save t
	TeX-parse-self t
	TeX-PDF-mode t)

  ;; NOTE I should probably make Emacs detect DPI
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

  (setq-default TeX-master nil))

(use-package org
  :straight t
  :hook (org-mode . org-indent-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-switchb))
  :config
  (setq org-agenda-files '("~/org/")
	org-default-notes-file "~/org/todo.org")

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (R .t)
     (emacs-lisp . t)
     (calc . t)))
  (setq org-confirm-babel-evaluate nil))


;; (use-package org-roam
;;   :hook ((org-load . org-roam-mode)
;;          (org-roam-backlinks-mode . turn-on-visual-line-mode))
;;   :commands (org-roam-buffer-toggle-display
;;              org-roam-dailies-date
;;              org-roam-dailies-today
;;              org-roam-dailies-tomorrow
;;              org-roam-dailies-yesterday)
;;   :init
;;   (setq org-roam-directory (expand-file-name "~/org/roam/")
;;         ;; org-roam-verbose nil
;;         org-roam-buffer-window-parameters '((no-delete-other-windows . t))
;;         org-roam-completion-system 'ivy))

;; (use-package company-org-roam
;;   :after org-roam
;;   :config
;;   (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))


(use-package python
  :mode ("\\.py" . python-mode)
  :config
  (setq python-indent-offset 4))

(use-package pyvenv
  :straight t
  :init
  (setenv "WORKON_HOME" "~/.envs/"))


(use-package elfeed
  :straight t)

(use-package elfeed-org
  :straight t
  :after elfeed
  :config
  (elfeed-org))


(use-package which-key
  :straight t
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package deft
  :straight t
  :commands deft
  :bind ("C-c d d" . deft)
  :config (setq deft-directory "~/org/zk/"
		deft-extensions '("org" "md" "markdown" "txt")
		deft-default-extension "org")

  (add-hook 'deft-mode-hook (lambda () (display-line-numbers-mode -1))))


(use-package gdscript-mode
  :straight t
  :config (setq gdscript-use-tab-indents nil))

(use-package olivetti
  :straight t)

(use-package rainbow-mode
  :straight t)

(use-package autothemer
  :straight t)

;;(use-package vterm
;;  :straight t)

(use-package racket-mode
  :straight t
  :hook (racket-mode . racket-xp-mode))

(use-package hydra
  :straight t)

(use-package ein
  :straight t)

(use-package yasnippet
  :straight t)

(use-package emacsql-sqlite3
  :straight t)

(use-package bufler
  :straight t)

(use-package sly
  :straight t
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package paredit
  :straight t
  :hook ((emacs-lisp-mode . enable-paredit-mode)
	 (eval-expression-minibuffer-setup . enable-paredit-mode)
	 (ielm-mode . enable-paredit-mode)
	 (lisp-mode . enable-paredit-mode)
	 (lisp-interaction-mode . enable-paredit-mode)
	 (scheme-mode . enable-paredit-mode)
	 (racket-mode . enable-paredit-mode))
  :bind (([C-backspace] . paredit-backward-kill-word)
	 ([M-backspace] . backward-kill-word))

  :config

  (define-key paredit-mode-map (kbd "M-s") nil)
  (define-key paredit-mode-map (kbd "M-S") nil)
  (define-key paredit-mode-map (kbd "M-S s") 'paredit-splice-sexp)
  (define-key paredit-mode-map (kbd "M-S S") 'paredit-split-sexp))



(use-package vertico
  :straight t
  :init
  (vertico-mode))

(use-package embark
  :straight t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

   :init
   ;; Optionally replace the key help with a completing-read interface
   (setq prefix-help-command #'embark-prefix-help-command) ;; by default describe-prefix-bindings

   :config
   (add-to-list 'display-buffer-alist
		'("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		  nil
		  (window-parameters (mode-line-format . none)))))

(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
	 ("C-c h" . consult-history)
	 ("C-c m" . consult-mode-command)
	 ("C-c b" . consult-bookmark)
	 ("C-c k" . consult-kmacro)
	 ;; C-x bindings (ctl-x-map)
	 ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
	 ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
	 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	 ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
	 ;; Custom M-# bindings for fast register access
	 ("M-#" . consult-register-load)
	 ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
	 ("C-M-#" . consult-register)
	 ;; Other custom bindings
	 ("M-y" . consult-yank-pop) ;; orig. yank-pop
	 ("<help> a" . consult-apropos) ;; orig. apropos-command
	 ;; M-g bindings (goto-map)
	 ("M-g e" . consult-compile-error)
	 ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
	 ("M-g g" . consult-goto-line) ;; orig. goto-line
	 ("M-g M-g" . consult-goto-line) ;; orig. goto-line
	 ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-project-imenu)
	 ;; M-s bindings (search-map)
	 ("M-s f" . consult-find)
	 ("M-s L" . consult-locate)
	 ("M-s g" . consult-grep)
	 ("M-s G" . consult-git-grep)
	 ("M-s r" . consult-ripgrep)
	 ("M-s l" . consult-line)
	 ("M-s m" . consult-multi-occur)
	 ("M-s k" . consult-keep-lines)
	 ("M-s u" . consult-focus-lines)
	 ;; Isearch integration
	 ("M-s e" . consult-isearch)
	 :map isearch-mode-map
	 ("M-e" . consult-isearch) ;; orig. isearch-edit-string
	 ("M-s e" . consult-isearch) ;; orig. isearch-edit-string
	 ("M-s l" . consult-line))

  
  :straight t
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-narrow-key "<") 
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :init
  (savehist-mode))

(use-package prism
  :straight t)


;;(use-p ackage flycheck
;; :commands flycheck-list-errors flycheck-buffer
;; :hook (after-init . global-flycheck-mode)
;; :config
;; (setq flycheck-emacs-lisp-load-path 'inherit)

;; ;; Check only when saving or opening files. Newline & idle checks are a mote
;; ;; excessive and can catch code in an incomplete state, producing false
;; ;; positives, so we removed them.
;; (setq flycheck-check-syntax-automatically '(save mode-enabled idle-buffer-switch))

;; ;; For the above functionality, check syntax in a buffer that you switched to
;; ;; only briefly. This allows "refreshing" the syntax check state for several
;; ;; buffers quickly after e.g. changing a config file.
;; (setq flycheck-buffer-switch-check-intermediate-buffers t)

;; ;; Display errors a little quicker (default is 0.9s)
;; (setq flycheck-display-errors-delay 0.25)

;; ;; Don't commandeer input focus if the error message pops up (happens when
;; ;; tooltips and childframes are disabled).
;; ;;(set-popup-rule! "^\\*Flycheck error messages\\*" :select nil)
;; )

(custom-set-variables
 ;; custom-set-variables was added by Custom.

 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline success warning error])
 '(awesome-tray-mode-line-active-color "#2fafff")
 '(awesome-tray-mode-line-inactive-color "#323232")
 '(custom-safe-themes
   '("cbdf8c2e1b2b5c15b34ddb5063f1b21514c7169ff20e081d39cf57ffee89bc1e" "a0be7a38e2de974d1598cf247f607d5c1841dbcef1ccd97cded8bea95a7c7639" "03c4056491cec9272f3db90a7f7750f741a73b8c888c161b49c48f09b74dba51" "c0d819a0d134a7ddd5f1462370f5d270b16d9fd33dd9a61dfee8726af950bf09" "e53bed8363dc3219664bfc37dd4d0fb7e8912f6f803b10ff04e3f3d8cf0063b2" "2679db166117d5b26b22a8f12a940f5ac415d76b004de03fcd34483505705f62" "8ca8fbaeaeff06ac803d7c42de1430b9765d22a439efc45b5ac572c2d9d09b16" "fdaa2acf1ef1bbb8094029bb007e958f6d120914369f0a82f9143456d4e58866" "2186e7e823967ae910dc00cd0deb7f50be901c58512927e2d24a08cbcfe159de" "d28a6aca98c9c3c154c49f1a4f5526308b4847abff28d9a2f254695f05fab2d1" "3e4c5e34452c3259a63f3e102b90be765d9183c6bf83409ecf955ec4656b6625" "5c35e6f5d604c356c9f06e3754e4427faeae3156c166d802772e63b60ffd1311" "c5f94c84efcc240c90e3cfdd353c123f38df657cae113480caff94819cb6b636" "5065b433264e54e1153635b8b85c984c47f40de97323433c5f9958aea9058f34" "eeab6976572150d2235d51c7522141c37d6bc73bc08a8a1a43e357c611d6c13c" "23a611b4aa91decc36f58761624e6ec100714ed68b1f036f583812eda0266ec7" "75821230c28609ef75717b5117866c909a6a3ec645db05f7bf33a86386805560" "494c111df9d33f6ab4f6cadc8a4def672d00089a63eef651b0e77bb8d6739306" "efd1195a7520817916e5a630fda709c4ef8663e589442d2237ac4b67f96336ea" "b53c7409d33403262d3521cd37a3325472046df8b69d6f42da5c130b1fd4c063" "961aa640caca3646443cc0d2d711af68f5b936ec18cdcb24aff0670ffd97b4ff" "b88bb97c7ae3cdf24ae01e13d83f733ddda5ccd0d506f24814f2f4d4004c29e6" "bdc9ae7edbf5a684a49f2a372737558bd85df83c9a6cc075641deb9e9fb727b6" "58435c5f916dcc0f109bf1db1fd4f3c7679399df33bdb168af08661e70423cf0" "0fd880b004cd0eca5ca73745db613716d24f7a68258d6ac514c3cf0925045fc0" "8950d48a5ab9b329d5d4a7e885f303bea7bf9aa106ada09288862f21942bb51b" "442989ca4ffe78eb501ad959395f73a402702359aa94728d67beafb9926d17cb" "9fcc37e34ac8603a7ebef47aac016bf9158145d3b8035204c650038bee310450" "6a9e48e3ff2170edd584d2e9655d3b5c44a8d5a36ac5e52e9db98671a606e345" "11cca5e3710fc21ba140738b9bec90104ccf983c34e24a69c7c753e6f944d7f4" "378d52c38b53af751b50c0eba301718a479d7feea5f5ba912d66d7fe9ed64c8f" "890a1a44aff08a726439b03c69ff210fe929f0eff846ccb85f78ee0e27c7b2ea" "bdc756066a7be9c1baa8014c443db73355f82006f88eb3d4df55b04947396ca0" "00664002472a541e3df8a699c2ea4a5474ea30518b6f9711fdf5fe3fe8d6d34f" "dbade2e946597b9cda3e61978b5fcc14fa3afa2d3c4391d477bdaeff8f5638c5" "801a567c87755fe65d0484cb2bded31a4c5bb24fd1fe0ed11e6c02254017acb2" "d4131a682c4436bb5a61103d9a850bf788cbf793f3fd8897de520d20583aeb58" "0754c176c3f850e1f90e177e283247749a1ac688faebf05b6016343cb3f00064" "5d59bd44c5a875566348fa44ee01c98c1d72369dc531c1c5458b0864841f887c" "5078e1845735a69b21b5effe083998dc368853320f449530c2616cf70bc3c47b" "eb122e1df607ee9364c2dfb118ae4715a49f1a9e070b9d2eb033f1cefd50a908" "df01ad8d956b9ea15ca75adbb012f99d2470f33c7b383a8be65697239086672e" "021321ae56a45794f43b41de09fb2bfca184e196666b7d7ff59ea97ec2114559" "dcdd1471fde79899ae47152d090e3551b889edf4b46f00df36d653adc2bf550d" "79586dc4eb374231af28bbc36ba0880ed8e270249b07f814b0e6555bdcb71fab" "5eb1bcc5b0b0a88fbcbe31ac45169ac7aa54b724f8753db0f3fe26c3504e734c" "de1f10725856538a8c373b3a314d41b450b8eba21d653c4a4498d52bb801ecd2" "5ed25f51c2ed06fc63ada02d3af8ed860d62707e96efc826f4a88fd511f45a1d" "9442c8082c5a380277af6ba7af157f70fa238a48b9f1e955387010b82134237f" "9fe5f7bf18a73ed5a1f5e469b21ff49b3366b5322d29c27e981353a3816bf94a" "57e3f215bef8784157991c4957965aa31bac935aca011b29d7d8e113a652b693" default))
 '(ensime-sem-high-faces
   '((var :foreground "#9876aa" :underline
	  (:style wave :color "yellow"))
     (val :foreground "#9876aa")
     (varField :slant italic)
     (valField :foreground "#9876aa" :slant italic)
     (functionCall :foreground "#a9b7c6")
     (implicitConversion :underline
			 (:color "#808080"))
     (implicitParams :underline
		     (:color "#808080"))
     (operator :foreground "#cc7832")
     (param :foreground "#a9b7c6")
     p
     (class :foreground "#4e807d")
     (trait :foreground "#4e807d" :slant italic)
     (object :foreground "#6897bb" :slant italic)
     (package :foreground "#cc7832")
     (deprecated :strike-through "#a9b7c6")))

 '(flymake-note-bitmap '(exclamation-mark modus-theme-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-theme-fringe-yellow))
 '(frame-background-mode nil)
 '(highlight-tail-colors '(("#2f4a00" . 0) ("#00415e" . 20)))
 '(hl-sexp-background-color "#efebe9")
 '(hl-todo-keyword-faces
   '(("HOLD" . "#cfdf30")
     ("TODO" . "#feacd0")
     ("NEXT" . "#b6a0ff")
     ("THEM" . "#f78fe7")
     ("PROG" . "#00d3d0")
     ("OKAY" . "#4ae8fc")
     ("DONT" . "#80d200")
     ("FAIL" . "#ff8059")
     ("BUG" . "#ff8059")
     ("DONE" . "#44bc44")
     ("NOTE" . "#f0ce43")
     ("KLUDGE" . "#eecc00")
     ("HACK" . "#eecc00")
     ("TEMP" . "#ffcccc")
     ("FIXME" . "#ff9977")
     ("XXX+" . "#f4923b")
     ("REVIEW" . "#6ae4b9")
     ("DEPRECATED" . "#bfd9ff")))
 '(ibuffer-deletion-face 'modus-theme-mark-del)
 '(ibuffer-filter-group-name-face 'modus-theme-mark-symbol)
 '(ibuffer-marked-face 'modus-theme-mark-sel)
 '(ibuffer-title-face 'modus-theme-header)
 '(inhibit-startup-screen t)
 '(linum-format " %5i ")
 '(vc-annotate-background-mode nil)
 '(xterm-color-names
   ["#000000" "#ff8059" "#44bc44" "#eecc00" "#29aeff" "#feacd0" "#00d3d0" "#a8a8a8"])
 '(xterm-color-names-bright
   ["#181a20" "#f4923b" "#80d200" "#cfdf30" "#72a4ff" "#f78fe7" "#4ae8fc" "#ffffff"]))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
