;; -*- lexical-binding: t -*-

;;; Bootstrapping and Setup

;; Make garbage collector threshold larger during startup

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
	    (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(load "~/.config/emacs/local-lisp/straight-bootstrap.el")

(straight-use-package 'use-package)
(straight-use-package 'general)

(setq use-package-hook-name-suffix nil)

(require 'use-package)
(require 'general)

(straight-use-package-mode 1)

;;; OS Predicates

(defconst emacs-28+-p (>= (string-to-number (substring emacs-version 0 2))
                          28)
  "Is t if `emacs-version' is at least version 28.")
(defconst on-windows (string-equal system-type "windows-nt")
  "Is the OS Windows?")
(defconst on-linux (string-equal system-type "gnu/linux")
  "Is the OS Linux?")

;;; Core Emacs Behavior

;;;; Load Path

(add-to-list 'load-path (locate-user-emacs-file "local-lisp"))

;;;; Customize File

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;;; Native Compilation

(when emacs-28+-p
  ;; Compile loaded .elc files asynchronously
  (setq native-comp-deferred-compilation t))

;;;; Backup and Autosave

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups")))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 10
      kept-old-versions 5)

(use-package savehist
  :init (savehist-mode 1))

(use-package recentf
  :hook (after-init-hook . recentf-mode)
  :init
  (setq-default recentf-max-saved-items 100))

;;;; Miscellaneous

(general-unbind
  "<insert>"
  "C-z"
  "C-x C-z")

(setq delete-by-moving-to-trash t)
(setq read-process-output-max (* 1024 1024)) ;; 1 mb

;; Save those two letters!
(fset 'yes-or-no-p 'y-or-n-p)

;; Confirm when killing Emacs.
(setq confirm-kill-emacs
      (lambda (prompt)
	(let ((seconds 2))
	  (y-or-n-p-with-timeout prompt seconds nil))))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(setq sentence-end-double-space nil)

;;; Function Library

(require 'util-functions)

;;; Visual Tweaks

;;;; Font

;; (set-face-attribute 'default nil :font "Source Code Pro-10")
;; (set-face-attribute 'default nil :font "JuliaMono-10")
;; (set-face-attribute 'fixed-pitch nil :font "JuliaMono-10")
;; (set-face-attribute 'variable-pitch nil :font "Source Code Variable-10")

(custom-set-faces
 '(default ((t (:family "Iosevka" :height 120))))
 '(fixed-pitch ((t (:family "Iosevka" :height 120))))
 '(variable-pitch ((t (:family "Iosevka Etoile" :height 120)))))

;;;; Themes

;; For personal themes
(setq custom-theme-directory "~/.config/emacs/themes")

(use-package autothemer)

;;;;; Theme Utilily

(defun my/install-themes (themes)
  "Installs the individual theme pacakges in the list THEMES.

Themes are provided as symbols or lists that represent
straight.el recipes as an example:

(install-themes '(doom-themes
                  (nano-theme :type git
                              :host github
                              :repo \"rougier/nano-theme.el\")))

Uses `straight.el' for installation."
  (if (null themes)
      nil
    (let ((theme (car themes))
	  (rest (cdr themes)))
      (condition-case err
	  (straight-use-package theme)
	(error (let ((name (if (atom theme)
			       theme
			     (car theme))))
		  
		 (display-warning 'mentat-theme-installer
				  (format "Installing `%s' failed." name)
				  :warning))))
      (my/install-themes rest))))

;;;;; Theme Installation

(let ((theme-list '(doom-themes
		    leuven-theme
		    brutalist-theme
		    challenger-deep-theme
		    darcula-theme
		    dracula-theme
		    eink-theme
		    espresso-theme
		    flatland-theme
		    gandalf-theme
		    naysayer-theme
		    northcode-theme
		    tao-theme
		    afternoon-theme
		    humanoid-themes
		    curry-on-theme
		    nano-theme
		    plan9-theme)))
  (my/install-themes theme-list))

;;;;; Modus Themes

(use-package modus-themes
  :disabled
  :init
  (setq modus-themes-bold-constructs t
	modus-themes-syntax '(alt-syntax))
  (modus-themes-load-themes)
  :config
  (modus-themes-load-operandi))

;;;;; Active Theme

(load-theme 'leuven-dark)

;;;; Olivetti

;;;; Frame Formatting

(setq frame-title-format
      '(""
        "%b"
        (:eval (if (project-current)
		   (let ((project-name (or (project-root (project-current)) "")))
		     (unless (string= "-" project-name)
		       ;;(format (if (buffer-modified-p) " ◉ %s" "  ●  %s - Emacs") project-name)
		       (format (if (buffer-modified-p) " * %s - Emacs" " - %s - Emacs") project-name)
		       ))))))

;;;; Parentheses

(use-package paren
  :defer 2
  :config
  (show-paren-mode 1)
  (setq show-paren-when-point-in-periphery t
	show-paren-when-point-inside-paren t))

;;;; Colorize Colors

(use-package rainbow-mode
  :defer
  :config
  (setq rainbow-ansi-colors nil
	rainbow-x-colors nil))

;;; Outline Mode

(use-package outline
  :general (outline-minor-mode-map
	    "<tab>" 'outline-cycle
	    "C-c C-n" 'outline-next-visible-heading
	    "C-c C-p" 'outline-previous-visible-heading
	    "<backtab>" (lambda ()
			  (interactive)
			  (outline-back-to-heading)
			  (outline-cycle))) )

;;; Emacs Server

(add-hook 'after-init-hook
	  (lambda ()
	    (require 'server)
	    (unless (server-running-p)
	      (server-start))))

;;; Major Interaction Packages

;;;; Navigation

(use-package symex
  :custom (symex-modal-backend 'hydra)
  :config
  (symex-initialize)
  (global-set-key (kbd "C-'") 'symex-mode-interface))

(use-package avy
  :custom
  (avy-style 'at-full)
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (avy-dispatch-alist '((?m . avy-action-mark)
			(?  . avy-action-mark-to-char)
			(?I . avy-action-ispell)
			(?E . avy-action-embark)
			(?H . avy-action-helpful)
			(?z . avy-action-zap-to-char)
			   
			(?w . avy-action-copy)
			(?k . avy-action-kill-stay)
			(?y . avy-action-yank)

			(?W . avy-action-copy-whole-line)
			(?K . avy-action-kill-whole-line)
			(?Y . avy-action-yank-whole-line)))
  (avy-timeout-seconds 0.4)
  (avy-background t)
  :config
  (defun my/avy-pop-window ()
    "Return to the window that was active before last avy command."
    (select-window
     (cdr (ring-ref avy-ring 0))))

  (defun avy-action-embark (pt)
    (cl-letf (((symbol-function 'keyboard-quit)
	       #'abort-recursive-edit))
      (save-excursion
	(goto-char pt)
	(embark-act))
      (my/avy-pop-window))
    t)
  
  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))
  
  (defun avy-action-helpful (pt)
    (save-excursion
      (goto-char pt)
      (helpful-at-point))
    (my/avy-pop-window)
    t)
  
  (defun avy-action-kill-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-line))
    (my/avy-pop-window)
    t)

  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (my/avy-pop-window)
    t)

  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
	  (bounds-of-thing-at-point 'line)
	(copy-region-as-kill start end)))
    (my/avy-pop-window)
    t)

  (defun avy-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (defun my/avy-isearch-this-window (&optional arg)
    "Go to isearch candidate in this window only with avy."
    (interactive "P")
    (let ((avy-all-windows nil)
	  (current-prefix-arg (if arg 4)))
      (call-interactively 'avy-isearch)))
  :general
  (global-map
   "C-t" 'avy-goto-char-timer)
  (isearch-mode-map
   "C-t" #'my/avy-search-this-window))

(use-package dumb-jump)

;;;; Minibuffer customizations

(setq completion-cycle-threshold 3
      enable-recursive-minibuffers t
      ;; Do not allow the cursor in the minibuffer prompt
      minibuffer-prompt-properties '(read-only t
				     cursor-intangible t
				     face minibuffer-prompt))

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq completion-category-overrides '((file (styles . (partial-completion)))))

(when emacs-28+-p
  ;; Hide commands in M-x which do not work in the current mode.
  (setq read-extended-command-predicate
	#'command-completion-default-include-p))

(use-package marginalia
  :init (marginalia-mode)
  :general (minibuffer-local-map
	    "M-A" 'marginalia-cycle))

;;;; Completion framework

(use-package orderless
  :config
  (setq orderless-component-separator #'split-string-and-unquote)
  (setq completion-styles '(orderless partial-completion))
  (setq orderless-matching-styles
	'(
	  orderless-regexp
	  orderless-strict-leading-initialism))
  
  (defun my/flex-if-twiddle (pattern _index _total)
    "If the orderless component ends in ~, then use the flex style."
    (when (or (string-suffix-p "`" pattern)
	      (string-suffix-p "~" pattern))
      `(orderless-flex . ,(substring pattern 0 -1))))

  (defun my/first-initialism (pattern index _total)
    "Override the first component to always try to match as an initialism."
  (if (= index 0) 'orderless-initialism))

  (defun my/without-if-bang (pattern _index _total)
    "The literal orderless component should not be contained in the candidate."
  (cond
   ((equal "!" pattern)
    '(orderless-literal . ""))
   ((string-prefix-p "!" pattern)
    `(orderless-without-literal . ,(substring pattern 1)))))

  (defun my/regexp-if-dollar (pattern _index _total)
    "If the orderless component ends in $ then it should match regexp."
    (when (string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1)
                                    "[\x100000-\x10FFFD]*$"))))

  (defun my/literal-if-eqsign (pattern _index _total)
    "If the orderless component ends in = it should match literally."
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))

  (defun my/initialism-if-comma (pattern _index _total)
    "If the orderless component ends in , it should match a strict full initialism."
    (when (string-suffix-p "," pattern)
      `(orderless-initialism . ,(substring pattern 0 -1))))

(setq orderless-matching-styles '(orderless-regexp)
      orderless-style-dispatchers '(my/flex-if-twiddle
                                    my/without-if-bang
				    my/regexp-if-dollar
				    my/literal-if-eqsign
				    my/initialism-if-comma)))

(use-package vertico
  :init (vertico-mode)
  :config
  (setq vertico-count 15)
  :general (vertico-map
	    "C-o" 'embark-export
	    "C->" 'embark-become))

;;;; Embark and Consult

(use-package embark
  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  (setq embark-verbose-indicator-display-action '(display-buffer-reuse-window))

  (defun embark-target-this-buffer-file ()
    "Allow Embark to act on the current buffer file."
    (cons 'this-buffer-file (buffer-name)))

  (add-to-list 'embark-target-finders #'embark-target-this-buffer-file 'append)

  (embark-define-keymap this-buffer-file-map
    "Commands to act on current file or buffer."
    ("l" load-file)
    ("b" byte-compile-file)
    ("S" sudo-find-file)
    ("r" rename-file-and-buffer)
    ;; ("d" my/diff-buffer-dwim)
    ("=" ediff-buffers)
    ("C-=" ediff-files)
    ("&" async-shell-command)
    ("x" consult-file-externally)
    ("C-a" mml-attach-file)
    ("c" copy-file)
    ("k" kill-buffer)
    ("#" recover-this-file)
    ("z" bury-this-buffer)
    ("|" embark-shell-command-on-buffer)
    ;; ("l" org-store-link)
    ("g" revert-buffer))

  (add-to-list 'embark-keymap-alist '(this-buffer-file . this-buffer-file-map))
  ;; TODO Do I need cl-pushnew? or will add-to-list suffice?
  (cl-pushnew 'revert-buffer embark-allow-edit-commands)
  (cl-pushnew 'rename-file-and-buffer embark-allow-edit-commands)
  
  :general ("C-." 'embark-act
	       "C-;" 'embark-dwim
	       "C-h B" 'embark-bindings))

(use-package consult
  :config
  (setq consult-narrow-key "<")
  (setq consult-line-numbers-widen t)
  (setq consult-preview-key (list :debounce 0.2 'any))

  ;; ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; ;; Otherwise use the default `completion--in-region' function.
  ;; (setq completion-in-region-function
  ;; 	(lambda (&rest args)
  ;;         (apply (if vertico-mode
  ;;                    #'consult-completion-in-region
  ;;                  #'completion--in-region)
  ;; 		 args)))

  ;; ;; Optionally replace `completing-read-multiple' with an enhanced version.
  ;; (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
	register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)

  ;; For some reason project-root wasn't introduced until 28+
  (unless emacs-28+-p 
    (defun project-root (project)
      (car (project-roots project))))

  (setq consult-project-root-function
	(lambda ()
	  (when-let (project (project-current))
	    (project-root project))))
  :general
  ('help-map
   "a" 'consult-apropros)
  (mode-specific-map
   "h" 'consult-history
   "m" 'consult-mode-command
   "b" 'consult-bookmark
   )
  (ctl-x-map
   "r b" 'consult-bookmark
   "M-:" 'consult-complex-command
   "b" 'consult-buffer
   "4 b" 'consult-buffer-other-window
   "5 b" 'consult-buffer-other-frame)
  (goto-map ;; M-g
   "e" 'consult-compile-error
   "f" 'consult-flymake
   "g" 'consult-goto-line
   "M-g" 'consult-goto-line
   "o" 'consult-outline
   "m" 'consult-mark
   "k" 'consult-global-mark
   "i" 'consult-imenu
   "I" 'consult-project-imenu)
  (search-map ;; M-s
   "f" 'consult-find
   "L" 'consult-locate
   "g" 'consult-grep
   "G" 'consult-git-grep
   "r" 'consult-ripgrep
   "l" 'consult-line
   "m" 'consult-multi-occur
   "k" 'consult-keep-lines
   "u" 'consult-focus-lines
   "e" 'consult-isearch)
  (isearch-mode-map
   "M-e" 'consult-isearch
   "M-s e" 'consult-isearch
   "M-s l" 'consult-line)
  (global-map
   ;; Register commands
   "M-#" 'consult-register-load
   "M-'" 'consult-register-store
   "C-M-#" 'consult-register
   ;; Yank commands
   "M-y" 'consult-yank-pop))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode)
  :general
  (embark-become-file+buffer-map
   "m" 'consult-bookark
   "b" 'consult-buffer
   "f" 'consult-find))



;;;; Completion and Capfs

(use-package corfu
  :hook (after-init-hook . corfu-global-mode)
  :config
  (setq tab-always-indent 'complete))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file-capf t))

;;; Better Help

(use-package helpful
  :general
  (help-map
   "f" 'helpful-callable
   "v" 'helpful-variable
   "k" 'helpful-key
   "F" 'helpful-function)
  (mode-specific-map
   "C-d" 'helpful-at-point)
  (embark-become-help-map
   "f" 'helpful-callable
   "v" 'helpful-variable
   "C" 'helpful-command))

;;; Window Management

(use-package winner
  :straight nil
  :init (winner-mode)

  (defun my/toggle-delete-other-windows ()
    "Delete other windows in frame if any, or restore previous window config."
    (interactive)
    (if (and winner-mode
             (equal (selected-window) (next-window)))
	(winner-undo)
      (delete-other-windows)))
  :general
  (ctl-x-map
   "1" 'my/toggle-delete-other-windows
   ))

(use-package ace-window
  :disabled
  :config
  (setq aw-keys '(?a ?r ?s ?t ?n ?e ?i ?o)))
  ;; :general
  ;; (clt-x-map
  ;;  "s-o" 'ace-window)

;;;; Popper

(use-package popper
  :hook ((after-init-hook . popper-mode)
	 (after-init-hook . popper-echo-mode))
  :custom
  (popper-reference-buffers '("\\*Messages\\*"
			      "Output\\*$"
			      "\\*Async Shell Command\\*"	
			      help-mode
			      helpful-mode
			      compilation-mode
			      "\\*.*REPL\\*"))
  (popper-group-function #'popper-group-by-directory)
  :general
  (global-map
   "C-`" 'popper-toggle-latest
   "M-`" 'popper-cycle
   "C-M-`" 'popper-toggle-type))

;;; Buffer Management

(use-package bufler)

;;; Visual Tweaks

;;;; Dashboard

(use-package dashboard
  :disabled
  :custom
  (dashboard-projects-backend 'project-el)
  (dashboard-items '((recents . 5)
		     (projects . 5)
		     (bookmarks . 5)
		     (agenda . 5)
		     (registers . 5)))
  (dashboard-set-footer nil)
  :config
  (dashboard-setup-startup-hook))

;;;; Mode-line customization

(defvar mode-line-alarm-flash-color "#F2804F")

(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line mode-line-alarm-flash-color)
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

;;;; Pulse

(use-package pulse)
(use-package mentat-pulse
  :straight nil
  :config
  (setq mentat-pulse-command-list
      '(recenter-top-bottom
        move-to-window-line-top-bottom
        reposition-window
        bookmark-jump
        other-window))

  (mentat-pulse-advice-commands-mode 1)
  )

;;;; Colorization

(use-package rainbow-delimiters)
(use-package prism)
(use-package hl-todo
  :hook (prog-mode-hook . hl-todo-mode)
  :custom
  (hl-todo-highlight-punctuation ":")
  (hl-todo-keyword-faces
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

;;;; Olivetti

(use-package olivetti
  :custom
  (olivetti-body-width 0.85)
  (olivetti-minimum-body-width 80))


;;; Keymap Helpers

(straight-use-package 'hydra)
(straight-use-package 'transient)
(use-package which-key
  :init (which-key-mode))

;;; Project Management

(use-package project)

;;; Bookmarks

(use-package bookmark
  :config
  (setq bookmark-fontify nil)
  (setq org-capture-bookmark nil))

;;; Scratch Buffers

(use-package scratch
  :hook (scratch-create-buffer-hook . my/scratch-buffer-setup)
  :config
  (defun my/scratch-buffer-setup ()
  "Add contents to `scratch' buffer and name it accordingly.
If region is active, add its contents t o the new buffer."
  (let* ((mode major-mode)
	 (string (format "Scratch buffer for: %s\n\n" mode))
	 (region (with-current-buffer (current-buffer)
		   (if (region-active-p)
		       (buffer-substring-no-properties
			(region-beginning)
			(region-end)))
		   ""))
	 (text (concat string region)))
    (when scratch-buffer
      (save-excursion
	(insert text)
	(goto-char (point-min))
	(comment-region (point-at-bol) (point-at-eol)))
      (forward-line 2))
    (rename-buffer (format "*Scratch for %s*" mode) t)))
  
  :general
  (global-map
   "C-c s" 'scratch))


;;; Org-mode

(use-package org
  :config
  (require 'mentat-org-setup))

(use-package org-ql)
(use-package org-super-agenda)

;;; Language Server

;; (use-package lsp-mode
;;   :init
;;   (setq lsp-tex-server 'digestif))

(use-package eglot
  :custom
  (eglot-connect-timeout 90))

(use-package eglot-jl
  :init (eglot-jl-init))

;;; Programming Languages

;;;; Julia

(use-package julia-mode
  :config
  (setenv "JULIA_NUM_THREADS" "4"))

(use-package julia-repl
  :hook (julia-mode-hook . julia-repl-mode))

;;;; Lisps

(use-package paredit
  :hook ((emacs-lisp-mode-hook
	  eval-exprerrion-minibuffer-hook
	  ielm-mode-hook
	  lisp-mode-hook
	  lisp-interaction-mode-hook
	  scheme-hook
	  racket-mode-hook)
	 . enable-paredit-mode)
  :general
  (paredit-mode-map
   ;; Switch C-backspace to use paredit-backward-kill to prevent
   ;; unbalancing parentheses
   "<C-backspace>" 'paredit-backward-kill-word
   "<M-backspace>" 'backward-kill-word
   ;; Remap the splice and split to not conflict with `search-map'.
   "M-s" nil
   "M-S" nil
   "M-s s" 'paredit-splice-sexp
   "M-S S" 'paredit-split-sexp))

;;;;; Common Lisp

(use-package sly
  :init
  (setq inferior-lisp-program "sbcl"))

;;;;; Racket

(use-package racket-mode
  :hook (racket-mode-hook . racket-xp-mode))

;;;;; Emacs Lisp

;;;;;; Libraries

(straight-use-package 'dash)
(straight-use-package 's)

;;;; R

(use-package ess
  :custom
  (ess-use-flymake t))


;;;; Haskell

(use-package haskell-mode)

;;;; Latex

(use-package auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (reftex-plug-into-AUCTeX t))

(use-package cdlatex
  :hook (LaTeX-mode-hook . turn-on-cdlatex))

(use-package latex
  :straight nil
  :hook ((LaTeX-mode-hook . prettify-symbols-mode)
	 (LaTeX-mode-hook . reftex-mode)
	 (LaTeX-mode-hook . TeX-fold-mode))
  :defer t
  :config
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (defvar mentat/latex-preview-scale 1.0)

  (add-hook 'LaTeX-mode-hook
            (defun preview-larger-previews ()
              (setq preview-scale-function
                    (lambda () (* mentat/latex-preview-scale
				  (funcall (preview-scale-from-face))))))))

(use-package preview
  :straight nil
  :defer t
  :config
  (add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))

;;;; Python

(use-package python
  :custom
  (python-indent-offset 4))

(use-package pyenv
  :init
  (setenv "WORKON_HOME" "~/.envs/"))

;;;; Markdown

(use-package markdown-mode
  :mode ("/README\\(|:\\.md\\)?\\'" . gfm-mode)
  :config

  (defun markdown-compile-pandoc (beg end output-buffer)
    "Compiles markdown with the pandoc program, if available.
Returns its exit code."
    (when (executable-find "pandoc")
      (call-process-region beg end "pandoc" nil output-buffer nil
                           "-f" "markdown"
                           "-t" "html"
                           "--mathjax"
                           "--highlight-style=pygments")))

  (setq markdown-enable-math t ; syntax highlighting for latex fragments
	markdown-enable-wiki-links t
	markdown-italic-underscore t
	markdown-asymmetric-header t
	markdown-fontify-code-blocks-natively t
	markdown-gfm-uppercase-checkbox t ; for compat with org-mode
	markdown-gfm-additional-languages '("sh")
	markdown-make-gfm-checkboxes-buttons t

	markdown-command #'markdown-compile-pandoc
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
		"<script>document.addEventListener('DOMContentLoaded', () => { document.body.classList.add('markdown-body'); document.querySelectorAll('pre[lang] > code').forEach((code) => { code.classList.add(code.parentElement.lang); }); document.querySelectorAll('pre > code').forEach((code) => { hljs.highlightBlock(code); }); });</script>")))

;;;; Nix

(use-package nix-mode)

;;;; Kmonad

(use-package kbd-mode
  :straight '(kbd-mode :type git
		       :host github
		       :repo "kmonad/kbd-mode"))

;;;; Godot Script

(use-package gdscript-mode
  :custom
  (gdscript-use-tab-indents nil))

;;; Applications

;;;; PDF

(use-package pdf-tools)

;;;; Email

(use-package mu4e
  :straight nil
  :custom
  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (mu4e-change-filenames-when-moving t)
  ;; Refresh mail using isync every 10 minutes
  (mu4e-update-interval (* 10 60))
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-maildir "~/Mail")

  (mu4e-drafts-folder "/[Gmail]/Drafts")
  (mu4e-sent-folder "/[Gmail]/Sent Mail")
  (mu4e-refile-folder "/[Gmail]/All Mail")
  (mu4e-trash-folder "/[Gmail]/Trash")
  (mu4e-maildir-shortcuts '((:maildir "/Inbox" :key ?i)
			    (:maildir "/[Gmail]/Sent Mail" :key ?s)
			    (:maildir "/[Gmail]/All Mail" :key ?a)
			    (:maildir "/[Gmail]/Trash" :key ?t)
			    (:maildir "/[Gmail]/Drafts" :key ?d))))

;;;; Elfeed

(use-package elfeed)
(use-package elfeed-org
  :init (elfeed-org))

;;;; Magit

(use-package magit
  :general
  (ctl-x-map
   "g" 'magit-status))

;;;; Deft

(use-package deft
  :custom
  (deft-directory "~/org/zk/")
  (deft-extensions '("org" "md" "markdown" "txt"))
  (deft-default-extension "org")
  :general
  (global-map
   "C-c d" 'deft))

;;;; Emacs Jupyter Notebook

(use-package ein)

;;;; SQL

(use-package emacsql-sqlite3)

;;; OS Specific

;;;; Linux 

(when on-linux
  (use-package envrc
    :hook (after-init-hook . envrc-global-mode))
  (use-package vterm ;; installed via guix
    :straight nil))

;;;; Windows

(when on-windows
  (require 'init-msft))

