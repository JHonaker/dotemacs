;;; init.el --- My Emacs configuration               -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <john@coeus>
;; Keywords: lisp

;; For debugging the config, we can use:
;; (load (locate-user-emacs-file "simple-init.el"))

;;; OS Predicates

(defconst emacs-28+-p
  (>= (string-to-number (substring emacs-version 0 2))
      28))

(defconst on-linux-p (string-equal system-type "gnu/linux")
  "Is the OS Linux?")

(defconst on-windows-p (string-equal system-type "windows-nt")
  "Is the OS Windows?")

;;; Packages and PATH

(defun recursive-add-folder-to-load-path (folder-name)
  "Adds the folder `folder-name' and its sub-folders to `load-path'."
  (let ((default-directory (directory-file-name folder-name)))
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path)))

;;;; Package.el setup

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))

;;;; Setup load-path

(defvar personal-packages-directory (locate-user-emacs-file "site-lisp")
  "The location of locally developed packages.")

(recursive-add-folder-to-load-path personal-packages-directory)

;;; Use-package

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq use-package-hook-name-suffix nil)

;;; Behavior

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s."
                     (emacs-init-time))))

(fset 'yes-or-no-p 'y-or-n-p)
(setopt echo-keystrokes 0.01)

;; Confirm when killing Emacs.
(setopt confirm-kill-emacs
	(lambda (prompt)
	  (let ((seconds 2))
	    (y-or-n-p-with-timeout prompt seconds nil))))

;; Use spaces!
(setq-default indent-tabs-mode nil)

;; Do not save duplicates in kill-ring
(setopt kill-do-not-save-duplicates t)

(global-so-long-mode 1)

;; Make #! files executable when saved
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(savehist-mode 1)

(setopt delete-by-moving-to-trash t)

(setopt read-process-output-max (* 1024 1024))

;; Unbind these awful keys
(dolist (key '("<insert>"
	       "C-z"
	       "C-x C-z"))
  (global-unset-key (kbd key)))

;;;; Recentf mode

(use-package recentf
  :init
  (setq-default recentf-max-saved-items 100)
  :custom
  (backup-by-copying t)
  (version-control t)
  (kept-new-versions 10)
  (kept-old-versions 5))


;;;; No-littering mode

(use-package no-littering
  :after (recentf)
  :config
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (add-to-list 'recentf-exclude no-littering-var-directory)

  ;; Customize file
  ;; (setopt custom-file (no-littering-expand-etc-file-name "custom.el"))
  ;; (when (file-exists-p custom-file)
  ;;  (load custom-file))
  
  ;; Autosave
  (setopt auto-save-file-name-transforms
	  `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;;;; Minibuffer

(setq enable-recursive-minibuffers t
      ;; Do not allow the cursor in the minibuffer prompt
      minibuffer-prompt-properties '(read-only t
				     cursor-intangible t
				     face minibuffer-prompt))

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(when emacs-28+-p
  ;; Hide commands in M-x which do not work in the current mode.
  (setq read-extended-command-predicate
	#'command-completion-default-include-p))


;;;; Whitespace

(use-package ws-butler
  :hook ((text-mode-hook prog-mode-hook) . ws-butler-mode))

;;; UI

(scroll-bar-mode 0)
(tool-bar-mode 0)
(show-paren-mode 1)
(setopt show-paren-when-point-in-periphery t)

;; Line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Visual column indicator
(setopt fill-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;;;; Font

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code" :height 110))))
 '(fixed-pitch ((t (:family "Fira Code" :height 110))))
 '(variable-pitch ((t (:family "Fira Code" :height 110)))))

(when (boundp 'text-scale-remap-header-line)
  (setq-default text-scale-remap-header-line t))

;;;; Theme

(use-package ef-themes
  :custom (ef-themes-to-toggle '(ef-deuteranopia-light ef-deuteranopia-dark))
  :config
  
  (ef-themes-select 'ef-deuteranopia-light))

;;;; Mode-line customization

(defcustom mode-line-alarm-flash-color "#F2804F"
  "The color to flash the mode-line when the visual alarm bell goes off.")

(setq ring-bell-function
      (lambda ()
	(let ((orig-fg (face-foreground 'mode-line)))
	  (set-face-foreground 'mode-line mode-line-alarm-flash-color)
	  (run-with-idle-timer 0.1 nil
			       (lambda (fg) (set-face-foreground 'mode-line fg))
			       orig-fg))))

;;;; Tab Bar

(use-package tab-bar
  :config
  (defun jh/tab-bar-format-menu-bar ()
    "Propertize the menu bar to show a custom character instead of the text `Menu'."
    `((menu-bar menu-item (propertize " λ " 'face 'tab-bar-tab-inactive)
		tab-bar-menu-bar :help "Menu Bar")))
  
  (add-to-list 'tab-bar-format #'jh/tab-bar-format-menu-bar)
  (menu-bar-mode -1)
  (tab-bar-mode 1))

;;;; Popup window management

(use-package popper
  :bind (("C-`" . popper-toggle-latest)
	 ("M-`" . popper-cycle)
	 ("C-M-`" . popper-toggle-type))
  :custom
  (popper-reference-buffers '("\\*Messages\\*"
			      "Output\\*$"
			      "\\*Async Shell Command\\*"	
			      help-mode
			      helpful-mode
			      compilation-mode
			      "\\*.*REPL\\*"))
  (popper-group-function #'popper-group-by-directory)
  :init
  (popper-mode)
  (popper-echo-mode))

;;;; Frame formatting

(setq frame-title-format
      '(""
        "%b"
        (:eval (if (project-current)
		   (let ((project-name (or (project-root (project-current)) "")))
		     (unless (string= "-" project-name)
		       (cond
			(on-linux-p
			 (format (if (buffer-modified-p)
				     " ◉ %s - Emacs"
				   "  ●  %s - Emacs")
				 project-name))
			(on-windows-p
			 (format (if (buffer-modified-p)
				     " * %s - Emacs"
				   " - %s - Emacs")
				 project-name)))))))))


;;;; Pretty writing mode

(use-package olivetti
  :commands (olivetti-mode)
  :custom ((olivetti-body-width 0.85)
	   (olivetti-minimum-body-width 80)))

;;;; Hightlight TODO keywords

(use-package hl-todo
  :hook (prog-mode-hook . hl-todo-mode)
  :custom
  (hl-todo-hightlight-punctuation ":")
  (hl-todo-keyword-faces
   '(;; For things that need to be done, just not today.
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

;;;; Rainbow mode

(use-package rainbow-mode
  :defer t
  :config
  (setq rainbow-ansi-colors nil)
  (setq rainbow-x-colors nil))

;;; Utility functions

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
	  (message "A buffer named '%s' already exists!" new-name)
	(if (vc-registered filename)
	    (vc-rename-file name new-name)
	  (rename-file name new-name 1))
	(rename-buffer new-name)
	(set-visited-file-name new-name)
	(set-buffer-modified-p nil)))))

(defun jh/open-directory-in-named-tab (directory tab-name)
  "Opens the DIRECTORY in a tab called TAB-NAME.

Switches to or creates the tab if it does not exist."
  (tab-switch tab-name)
  (find-file directory))

(defun jh/open-emacs-config-dired ()
  "Opens the Emacs configuration directory in a tab called \"Emacs Config\"."
  (interactive)
  (jh/open-directory-in-named-tab
   user-emacs-directory
   "Emacs Config"))

(defun jh/open-nix-config-dired ()
  "Opens the NixOS configuration directory in a tab called \"Nix Config\"."
  (interactive)
  (jh/open-directory-in-named-tab
   (expand-file-name "~/dots/nixos/")
   "Nix Config"))

;;; Editing

(electric-pair-mode 1)
(setopt sentence-end-double-space nil)

(defun open-line-above (&optional arg)
  (interactive)
  (beginning-of-line)
  (open-line (or arg 1))
  (indent-according-to-mode))

(defun open-line-below (&optional arg)
  (interactive)
  (end-of-line)
  (open-line (or arg 1))
  (forward-line)
  (indent-according-to-mode))

(global-set-key (kbd "C-o") #'open-line-below)
(global-set-key (kbd "M-o") #'open-line-above)

(defun move-to-indentation-or-beginning ()
  (interactive)
  (let* ((starting-point (point))
         ;; Side effect: moves the point to start of indentation
         (indentation-point (progn
                              (back-to-indentation)
                              (point)))
         (started-at-indentation-p (= starting-point indentation-point)))
    (when started-at-indentation-p
      (beginning-of-line))))

(global-set-key (kbd "C-a") #'move-to-indentation-or-beginning)

(global-set-key (kbd "C-S-o") #'other-window)

;;;;; Puni

(use-package puni
  :bind ("C-M-SPC" . puni-expand-region)
  :hook (term-mode-hook . puni-disable-puni-mode)
  :init (puni-global-mode))

;;;; Completion

(setq tab-always-indent 'complete)

;;;;; Vertico

(use-package vertico
  :custom
  (vertico-cycle t)
  (vertico-count 15)
  :init
  (vertico-mode))

;;;;; Corfu

(use-package corfu
  :bind (:map corfu-map
	 ("SPC" . corfu-insert-separator)
	 ("M-m" . corfu-move-to-minibuffer))
  :init
  (global-corfu-mode)

  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
	  completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data))))

;;;;; CAPE - Completion at Point Extensions

(use-package cape
  :bind (:map mode-specific-map
	 ("p p" . completion-at-point)
	 ("p d" . cape-dabbrev)
	 ("p f" . cape-file)
	 ("p i" . cape-ispell)
	 ("p w" . cape-dict)
	 ("p \\" . cape-tex))
	 
  :init
  (dolist (cape-fn (list #'cape-file
			 #'cape-tex
			 #'cape-dabbrev
			 #'cape-keyword))
    (add-to-list 'completion-at-point-functions cape-fn)))

;;;;; Orderless

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-overrides '((file (styles . (partial-completion)))))
  (orderless-component-separator #'split-string-and-unquote)
  :config

  ;; Orderless style dispatchers
  
   (defun my/flex-if-twiddle (pattern _index _total)
    "If the orderless component ends in ~ or `, then use the flex style."
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

  (setq orderless-style-dispatchers '(my/flex-if-twiddle
                                      my/without-if-bang
				      my/regexp-if-dollar
				      my/literal-if-eqsign
				      my/initialism-if-comma)))

;;;;; Marginalia

(use-package marginalia
  :bind (:map minibuffer-local-map
	 ("M-A" . marginalia-cycle))
  :init
  (setq marginalia-annotators '(marginalia-annotators-heavy
				marginalia-annotators-light
				nil))
  :config
  (marginalia-mode 1))

;;;;; Consult

;; Optionally configure the register formatting. This improves the register
;; preview for `consult-register', `consult-register-load',
;; `consult-register-store' and the Emacs built-ins.
(setq register-preview-delay 0.5
      register-preview-function #'consult-register-format)

;; Optionally tweak the register preview window.
;; This adds thin lines, sorting and hides the mode line of the window.
(advice-add #'register-preview :override #'consult-register-window)

;; Use Consult to select xref locations with preview
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

(use-package consult
  :bind (("M-y" . consult-yank-pop)
	 ("M-#" . consult-register-load)
	 ("M-'" . consult-register-store)
	 ("C-M-#" . consult-register)
	 :map help-map
	 ("a" . consult-apropos)
	 :map minibuffer-local-map
	 ("C-r" . consult-history)
	 :map mode-specific-map
	 ("h" . consult-history)
	 ("m" . consult-mode-command)
	 ("b" . consult-bookmark)
	 :map ctl-x-map
	 ("r b" . consult-bookmark)
	 ("M-:" . consult-complex-command)
	 ("b" . consult-buffer)
	 ("4 b" . consult-buffer-other-window)
	 ("5 b" . consult-buffer-other-frame)
	 :map goto-map
	 ("e" . consult-compile-error)
	 ("f" . consult-flymake)
	 ("g" . consult-goto-line)
	 ("M-g" . consult-goto-line)
	 ("o" . consult-outline)
	 ("m" . consult-mark)
	 ("k" . consult-global-mark)
	 ("i" . consult-imenu)
	 ("I" . consult-project-imenu)
	 :map isearch-mode-map
	 ("M-e" . consult-isearch)
	 ("M-s e" . consult-isearch)
	 ("M-s l" . consult-line))
  :init
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)
  ;; Replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))

  (setq consult-narrow-key (kbd "C-+")))

;;;; Embark
  
(use-package embark
  :bind (("C-." . embark-act)
	 ("C-;" . embark-dwim)
	 ([remap describe-bindings] . embark-bindings)
	 :map embark-become-file+buffer-map
	 ("m" . consult-bookmark)
	 ("b" . consult-buffer)
	 ("f" . consult-find)
	 :map vertico-map
	 ("C-o" . embark-export)
	 ("C->" . embark-become))
  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  
  (setq embark-verbose-indicator-display-action '(display-buffer-reuse-window))

  (defun embark-target-this-buffer-file ()
    "Allow Embark to act on the current buffer's file."
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

  (push 'embark--allow-edit (alist-get 'revert-buffer embark-target-injection-hooks))
  (push 'embark--allow-edit (alist-get 'rename-file-and-buffer embark-target-injection-hooks)))

(use-package embark-consult
  :after (embark consult))

;;;; Outline minor mode

(use-package outline
  :bind (:map outline-minor-mode-map
	 ("C-c C-SPC" . outline-cycle)
	 ("C-c C-n" . outline-next-visible-heading)
	 ("C-c C-p" . outline-previous-visible-heading)))

;;;; Scratch buffers don't get any respect

(use-package scratch
  :bind (:map mode-specific-map
	 ("s" . scratch))
  :config
  (defun jh/scratch-buffer-setup ()
    "Add contents to `scratch buffer and name it accordingly.
If region is active, add its contents to the new buffer."
    (let* ((mode major-mode)
	   (string (format "Scratch buffer for: %s\n\n" mode))
	   (region (with-current-buffer (current-buffer)
		     (if (region-active-p)
			 (buffer-substring-no-properties
			  (region-beginning)
			  (region-end))
		       "")))
	   (text (concat string region)))
      (when scratch-buffer
	(save-excursion
	  (insert text)
	  (goto-char (point-min))
	  (comment-region (point-at-bol) (point-at-eol)))
	(forward-line 2))
      (rename-buffer (format "*Scratch for %s*" mode) t)))

  (add-hook 'scratch-create-buffer-hook #'jh/scratch-buffer-setup))

;;; Navigation

;; To try: dumb-jump

;;;; Ace-window

(use-package ace-window
  :disabled
  :bind ("C-x o" . ace-window)
  :custom (aw-keys '(?a ?s ?d ?f ?j ?k ?l)))

;;;; Window movement

(winner-mode 1)

(defcustom window-group-prefix-key "C-c w"
  "Configure the prefix key for window management.")

(define-prefix-command 'window-group-key-map)

(let ((km window-group-key-map))
  (define-key km (kbd "u") 'winner-under)
  (define-key km (kbd "n") 'windmove-down)
  (define-key km (kbd "p") 'windmove-up)
  (define-key km (kbd "f") 'windmove-right)
  (define-key km (kbd "b") 'windmove-left))

(global-set-key (kbd window-group-prefix-key) 'window-group-key-map)
(define-key ctl-x-map "1" 'jh/toggle-delete-other-windows)
  
(defun my/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))


;;; Templates and Snippets

(use-package tempel
  :hook ((prog-mode-hook text-mode-hook) . tempel-setup-capf)
  :custom (tempel-path (no-littering-expand-etc-file-name "templates"))
  :init
  (defun tempel-setup-capf ()
    "Add the Tempel Capf to
`completion-at-point-functions'. `tempel-expand' only triggers on
exact matches. Alternatively use `tempel-complete' if you want to
see all matches, but then Tempel will probably trigger too often
when you don't expect it.

NOTE: `tempel-expand' is added *before* the main programming mode
Capf, so that it will be tried first."
    (setq-local completion-at-point-functions
		(cons #'tempel-expand
		      completion-at-point-functions))))

;;; Help Enhancements

(use-package helpful
  :bind (([remap describe-command] . helpful-command)
	 ([remap describe-function] . helpful-callable)
	 ([remap describe-key] . helpful-key)
	 ([remap describe-symbol] . helpful-symbol)
	 ([remap describe-variable] . helpful-variable)
	 ("C-h F" . helpful-function)
	 ("C-h K" . describe-keymap)
	 :map mode-specific-map
	 ("C-d" . helpful-at-point)))

(use-package helpful
  :after (embark)
  :bind (:map embark-become-help-map
	 ("f" . helpful-callable)
	 ("v" . helpful-variable)
	 ("C" . helpful-command)))

(use-package elisp-demos
  :after (helpful)
  :init
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))
  
;;; Bookmark

(use-package bookmark
  :defer t
  :config
  (setq bookmark-fontify nil)
  (setq org-capture-bookmark nil))

;;; Programming Major Modes

;;;; Eglot

(use-package eglot
  :custom
  (eglot-connect-timeout 90)
  :config
  (setf (alist-get 'eglot completion-category-overrides) '((styles orderless))))

;;;;; Emacs Lisp

(use-package dash)
(use-package s)
(use-package macrostep
  :bind (:map emacs-lisp-mode-map
	 ("C-c e" . macrostep-expand)))

;;;;; Common Lisp

(use-package sly
  :init
  (setq inferior-lisp-program "sbcl"))

;;;;; Racket

(use-package racket-mode
  :hook (racket-mode-hook . racket-xp-mode))

;;;;; Scheme

(use-package geiser)

(use-package geiser-chez
  :after (geiser))

(use-package geiser-gambit
  :after (geiser))

(use-package geiser-guile
  :after (geiser))

(use-package geiser-mit
  :after (geiser))

(use-package macrostep-geiser
  :after (geiser)
  :hook ((geiser-repl-mode-hook geiser-mode-hook) . macrostep-geiser-setup))

;;;;; Writing

;;;;;; LaTeX

(use-package auctex
  :mode ("\\.tex\\'" . TeX-mode)
  :hook ((LaTeX-mode-hook . prettify-symbols-mode)
	 (LaTeX-mode-hook . reftex-mode)
	 (LaTeX-mode-hook . outline-minor-mode)
	 (LaTeX-mode-hook . TeX-source-correlate-mode))
  :bind ((:map TeX-source-correlate-map
	  ([C-down-mouse-1] . TeX-view-mouse)))
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (reftex-plug-into-AUCTeX t)
  ;; (TeX-electric-math-mode '("\\(" . "\\)"))
  (TeX-electric-math nil)
  (TeX-engine 'luatex)
  (TeX-source-correlate-start-server nil)
  (TeX-view-program-list
   '(("Sioyek"
      ("sioyek %o --reuse-instance"
       (mode-io-correlate
	" --forward-search-file %b --forward-search-line %n --inverse-search \"emacsclient --no-wait +%2:%3 %1\""))
      "sioyek")))
  (TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Sioyek")
     (output-html "xdg-open")))
  )

;;;;;; Markdown

(use-package markdown-mode

  :config
  (defun markdown-compile-pandoc (beg end output-buffer)
    "Compiles markdown with pandoc, if available.
Returns its exit code."
    (when (executable-find "pandoc")
      (call-process-region beg end "pandoc" nil output-buffer nil
                           "-f" "markdown"
                           "-t" "html"
                           "--mathjax"
                           "--highlight-style=pygments")))
  (setq markdown-enable-math t		; syntax highlighting for latex fragments
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

;;;;; Python

(use-package python
  :custom
  (python-indent-offset 4))

;;;;; R

(use-package ess
  :custom
  (ess-use-flymake t))

;;;;; Julia

(use-package julia-mode)

(use-package julia-repl
  :after (julia-mode)
  :hook (julia-mode-hook . julia-repl-mode))

(use-package eglot-jl
  :init
  (setenv "JULIA_NUM_THREADS" "4")
  :after (eglot julia-mode)
  :config
  (eglot-jl-init))
  

;;;;; Haskell

(use-package haskell-mode)

;;;;; OCaml

(use-package tuareg)
(use-package dune)
(use-package merlin
  :hook ((tuareg-mode-hook . merlin-mode)
	 (tuareg-mode-hook . merlin-eldoc-setup)))
(use-package merlin-eldoc)
(use-package utop
  :disabled t
  :hook (tuareg-mode-hook . utop-minor-mode))

;;;;; Nix

(use-package nix-mode)

;;;;; Godot Script

(use-package gdscript-mode
  :disabled t
  :custom
  (gdscript-use-tab-indents nil))

;;;; Org-mode

(use-package org
  :custom
  (org-agenda-files '("~/org/refile.org"
		      "~/org/projects.org"))
  (org-default-notes-file "~/org/refile.org")
  :config
  ;; NOTE I should probably make Emacs detect DPI instead of this hack.
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (R . t)
     (lisp . t)
     (emacs-lisp . t)
     ;;(racket . t)
     (calc . t)))

  (setq org-confirm-babel-evaluate nil)

  (setq org-babel-lisp-eval-fn #'sly-eval)

  (autoload #'org-indent-mode "org" nil t)
  (add-hook 'org-mode-hook #'org-indent-mode)

  (setq org-log-done 'time)

  (setq org-use-fast-todo-selection 'expert)
  (setq org-fast-tag-selection-include-todo t)

  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "PROJ(p)" "|" "DONE(d)")
	  (sequence "WAITING(w@/!)" "INACTIVE(i)" "|" "CANCELLED(c@)")
	  (sequence "AMOTIVATOR(MA)" "TMOTIVATOR(MT)" "CMOTIVATOR(MC)")))

  (setq org-refile-targets '((nil :maxlevel . 9)
			     (org-agenda-files :maxlevel . 9)))

  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  (setq org-archive-location "archive/%s_archive::"))

(use-package org-ql)

(use-package org-super-agenda)

;;;; Agenda

;;;;; Agenda Views

(defmacro def-ql (name query &optional header)
  "Defines an `org-ql-block' called NAME for the QUERY with an optional HEADER."
  (declare (indent 1))
  (let ((query-name (intern (concat (symbol-name name) "-query")))
	(block-name (intern (concat (symbol-name name) "-block")))
	(header-stmt (if header
			  `(((org-ql-block-header ,header)))
		       nil)))
    `(progn
      (setq ,query-name ,query)
      (setq ,block-name '((org-ql-block ,query
					,@header-stmt))))))

(def-ql mentat/all-projects
  '(todo "PROJ")
  "Projects")

(def-ql mentat/superprojects
  '(and (todo "PROJ")
	(descendants (todo "PROJ")))
  "Root Projects")

(def-ql mentat/subprojects
  '(and (todo "PROJ")
	(ancestors (todo "PROJ")))
  "Subprojects")

(setq mentat/all-project-tasks-query
      '(and (todo "TODO" "NEXT")
	(ancestors (todo "PROJ"))
	(or (not (scheduled))
	    (scheduled :to 0))))

(setq mentat/all-project-tasks-block
      '((org-ql-block mentat/all-project-tasks-query
		      ((org-ql-block-header "All Project Tasks")
		       (org-super-agenda-groups '((:auto-parent t)))))))

(def-ql mentat/scheduled-tasks
  '(and (todo "TODO" "NEXT")
	(scheduled :to 0))
  "Scheduled Tasks")

(def-ql mentat/due-soon
  '(and (todo "TODO" "NEXT")
	(deadline :to 14))
  "Tasks Due Soon")

(def-ql mentat/stuck-projects
  '(and (todo "PROJ")
	(not (descendants (todo "NEXT"))))
  "Stuck Projects")

(def-ql mentat/refile-tasks
  '(category "refile")
  "Tasks to Refile")

(def-ql mentat/closed-today
  '(closed :from 0 :to 0)
  "Completed Today")

(def-ql mentat/recently-closed
  '(closed :from -14 :to 0)
  "Recently Completed Tasks")

(def-ql mentat/waiting-tasks
  '(todo "WAITING")
  "Waiting Tasks")

(def-ql mentat/next-tasks
  '(and (todo "NEXT")
	(or (not (scheduled))
	    (scheduled :to 0)))
  "Next Tasks")

(setq org-agenda-custom-commands

      `(("d" "Day to Day View"
	 (,@mentat/next-tasks-block
	  ,@mentat/scheduled-tasks-block
	  ,@mentat/due-soon-block
	  ,@mentat/closed-today-block
	  ,@mentat/refile-tasks-block
	  ,@mentat/all-projects-block
	  ,@mentat/all-project-tasks-block))
	("w" "Weekly Review"
	 (,@mentat/refile-tasks-block
	  ,@mentat/waiting-tasks-block
	  ,@mentat/stuck-projects-block
	  ,@mentat/next-tasks-block
	  ,@mentat/all-projects-block))
	("r" "Review Report View"
	 (,@mentat/recently-closed-block
	  ,@mentat/waiting-tasks-block
	  ,@mentat/all-projects-block
	  ,@mentat/stuck-projects-block))))

;;;; Capture Templates


(setq org-capture-templates
  '(("t" "Task" entry (file org-default-notes-file)
     "* TODO %?
%t
%i
%a")))


;;;; Functions

;; Functions

(defun mentat/is-project-p ()
  "A task with a 'PROJ' keyword is a project."
  (member (nth 2 (org-heading-components)) '("PROJ")))

(defun mentat/find-project-task ()
  "Any task with a todo keyword that is in a project subtree"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
	(when (mentat/is-project-p)
	  (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun mentat/is-project-subtree-p ()
  "Any task wtih a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
			      (point))))
    (save-excursion
      (mentat/find-project-task)
      (if (equal (point) task)
	  nil t))))

;;;; Review Functions

(defvar weekly-review-template "~/org/templates/weeklyreview.org")

(defun mentat/weekly-review ()
  "Open the Weekly Review checklist and agenda view in side-by-side windows.

Clears the current window setup."
  (interactive)
  (let ((buf-name "*Weekly Review*")
	(review-template-key "rr")))
  (delete-other-windows)
  (with-current-buffer (get-buffer-create buf-name)
    (insert-file-contents weekly-review-template)
    (org-mode))
  (switch-to-buffer buf-name)
  (org-agenda nil review-template-key))

(defun mentat/weekly-report ()
  (interactive)
  (let ((buf-name "*Weekly Report*"))
    (with-current-buffer (get-buffer-create buf-name)
      
      )
    )
  )

(defun ap/org-tree-to-indirect-buffer (&optional arg)
    "Create indirect buffer and narrow it to current subtree.
The buffer is named after the subtree heading, with the filename
appended.  If a buffer by that name already exists, it is
selected instead of creating a new buffer."
    (interactive "P")
    (let* ((new-buffer-p)
           (pos (point))
           (buffer-name (concat (org-link-display-format (nth 4 (org-heading-components)))
                                "::" (file-name-nondirectory (buffer-file-name (buffer-base-buffer)))))
           (new-buffer (or (get-buffer buffer-name)
                           (prog1 (condition-case nil
                                      (make-indirect-buffer (current-buffer) buffer-name 'clone)
                                    (error (make-indirect-buffer (current-buffer) buffer-name)))
                             (setq new-buffer-p t)))))
      (if arg
          (pop-to-buffer new-buffer)
        (switch-to-buffer new-buffer))
      (when new-buffer-p
        ;; I don't understand why setting the point again is necessary, but it is.
        (goto-char pos)
        (org-narrow-to-subtree))))

(advice-add 'org-tree-to-indirect-buffer :override 'ap/org-tree-to-indirect-buffer)


;;;; Keybindings

(let ((km mode-specific-map))
  (define-key km (kbd "l") 'org-store-link)
  (define-key km (kbd "a") 'org-agenda)
  (define-key km (kbd "c") 'org-capture)
  (define-key km (kbd "b") 'org-switchb))

;;;; Magit

(use-package magit
  :bind (:map ctl-x-map
	 ("g" . magit-status)))

;;;; PDF Tools

(use-package pdf-tools)

;;;; Elfeed

(use-package elfeed
  :hook (elfeed-show-mode-hook . my/enhance-elfeed-readability)
  :init
  (defun my/enhance-elfeed-redability ()
    "Clean up elfeed show buffer"
    (setq-local shr-width 80)
    (buffer-face-set 'fixed-pitch)))

(use-package elfeed-org
  :after (elfeed)
  :init (elfeed-org))

;;; Customize

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(puni no-littering ef-themes use-package)))
