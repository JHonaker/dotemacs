;;; Straight.el Bootstrap

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

;;; System specific configuration

(defconst on-windows (string-equal system-type "windows-nt"))
(defconst on-linux (string-equal system-type "gnu/linux"))

(add-to-list 'load-path (locate-user-emacs-file "local-lisp"))

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file)

(require 'mentat-core)

(when on-windows
  (setq ispell-program-name  "~/.emacs.d/hunspell/bin/hunspell.exe"))

;;; Global Emacs config

(unless (and (fboundp 'server-running-p)
             (server-running-p))
  (server-start))

(recentf-mode 1)

(setq gc-cons-threshold 80000000) ;; ~80mb
(setq read-process-output-max (* 1024 1024)) ;; 1 mb

;;; Setup macro

(defmacro config-with-package (package &rest body)
  "Requires PACKAGE and evaluates the forms in BODY. If there is an error, warn that loading failed.

PACKAGE is a quoted symbol. BODY is a series of lisp forms."
  (declare (indent 1))
  `(if (require ,package nil 'noerror)
       (progn ,@body)
     (display-warning 'mentat-config
                      (format "Loading `%s' failed." ,package)
                      :warning)))

;;; Helpful - the better `describe'

(straight-use-package 'helpful)
(config-with-package 'helpful
  (let ((map help-map))
    (define-key map (kbd "f") 'helpful-callable)
    (define-key map (kbd "v") 'helpful-variable)
    (define-key map (kbd "k") 'helpful-key)
    (define-key map (kbd "F") 'helpful-function))

  (define-key mode-specific-map (kbd "C-d") 'helpful-at-point))

(straight-use-package 'which-key)
;; (config-with-package 'which-key)
;;   (which-key-mode)

;;; Visual bell

(defvar mode-line-alarm-flash-color "#F2804F")

(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line mode-line-alarm-flash-color)
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

;;; Minibuffer

;; I believe this section comes from the Embark or Consult docs.

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t
        cursor-intangible t
        face minibuffer-prompt))

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq enable-recursive-minibuffers t)



;; Emacs 28+

(defconst emacs-28+-p (>= (string-to-number (substring emacs-version 0 2))
                          28)
  "Is t if `emacs-version' is at least version 28.")

(when emacs-28+-p
    (progn
      ;; Hide commands in M-x which do not work in the current mode.
      (setq read-extended-command-predicate
            #'command-completion-default-include-p)

      ;; Compile loaded .elc files asynchronously
      (setq native-comp-deferred-compilation t)))

(show-paren-mode 1)

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

(global-unset-key (kbd "<insert>"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

;;(set-face-attribute 'default nil :font "Source Code Pro-10")
;; (set-face-attribute 'default nil :font "JuliaMono-10")
;; (set-face-attribute 'fixed-pitch nil :font "JuliaMono-10")
;; (set-face-attribute 'variable-pitch nil :font "Source Code Variable-10")
(set-face-attribute 'default nil :font "Iosevka-10")
(set-face-attribute 'fixed-pitch nil :font "Iosevka-10")
(set-face-attribute 'variable-pitch nil :font "IosevkaEtoile-10")

;;; Dashboard

(straight-use-package 'dashboard)
(config-with-package 'dashboard

  (dashboard-setup-startup-hook)

  (setq dashboard-projects-backend 'project-el
        dashboard-items '((recents . 5)
                          (projects . 5)
                          (bookmarks . 5)
                          (agenda . 5)
                          (registers . 5))
        dashboard-set-footer nil))

;;; Themes

(require 'mentat-theme-install)

(setq custom-theme-directory "~/.config/emacs/themes")

(defvar theme-list '(doom-themes
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
                     modus-themes
                     humanoid-themes
                     curry-on-theme
                     (nano-theme :type git
                                 :host github
                                 :repo "404cn/nano-theme.el")
                     plan9-theme))

(mentat/install-themes theme-list)

(require 'modus-themes)

(setq modus-themes-bold-constructs t
      modus-themes-syntax '(alt-syntax))

(modus-themes-load-themes)
(modus-themes-load-operandi)

;;; Rainbow Mode
(straight-use-package 'rainbow-mode)
(setq rainbow-ansi-colors nil)
(setq rainbow-x-colors nil)

;;(fringe-mode (cons 15 15))

;;; Pulse

(config-with-package 'pulse
(require 'mentat-pulse)
(setq mentat-pulse-command-list
      '(recenter-top-bottom
        move-to-window-line-top-bottom
        reposition-window
        bookmark-jump
        other-window))
(mentat-pulse-advice-commands-mode 1))

;;; Scratch buffers

(straight-use-package 'scratch)
(config-with-package 'scratch
  (defun mentat/scratch-buffer-setup ()
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
      (rename-buffer (format "*Scratch for %s*" mode) t))))

(add-hook 'scratch-create-buffer-hook #'mentat/scratch-buffer-setup)
(define-key global-map (kbd "C-c s") #'scratch)

;;; Orderless

(straight-use-package 'orderless)
(config-with-package 'orderless
  (require 'mentat-orderless)

  (savehist-mode 1)

  (setq completion-styles '(orderless))

  (setq orderless-matching-styles mentat-orderless-default-styles)
  (setq orderless-style-dispatchers
        '(mentat-orderless-literal-dispatcher
          mentat-orderless-initialism-dispatcher
          mentat-orderless-flex-dispatcher)))

;;; Completion - Vertico

(straight-use-package 'vertico)
(config-with-package 'vertico
  (vertico-mode 1))

;;; Marginalia

(straight-use-package 'marginalia)
(config-with-package 'marginalia
  (marginalia-mode 1)

  (define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle))

;;; Completion - Corfu

(straight-use-package 'corfu)
(setq tab-always-indent 'complete)
(corfu-global-mode 1)

;;; Consult

(straight-use-package 'consult)
(config-with-package 'consult

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

  ;; For some reason project-root wasn't introduced until 28+
  (unless emacs-28+-p 
    (defun project-root (project)
      (car (project-roots project))))

  (setq consult-narrow-key "<") 
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (project-root project))))

  ;;;; Keybindings

  (define-key help-map (kbd "a") 'consult-apropos)

  (let ((map mode-specific-map))
    (define-key map (kbd "h") 'consult-history)
    (define-key map (kbd "m") 'consult-mode-command)
    (define-key map (kbd "b") 'consult-bookmark)
    (define-key map (kbd "k") 'consult-kmacro))

  (let ((map ctl-x-map))
    (define-key map (kbd "M-:") 'consult-complex-command) ;; orig. repeat-complex-command
    (define-key map (kbd "b") 'consult-buffer) ;; orig. switch-to-buffer
    (define-key map (kbd "4 b") 'consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
    (define-key map (kbd "5 b") 'consult-buffer-other-frame)) ;; orig. switch-to-buffer-other-frame

  (let ((map goto-map)) ;; M-g
    (define-key map (kbd "e") 'consult-compile-error) 
    (define-key map (kbd "f") 'consult-flymake) ;; Alternative consult-flycheck
    (define-key map (kbd "g") 'consult-goto-line) ;; orig. goto-line
    (define-key map (kbd "M-g") 'consult-goto-line) ;; orig. goto-line
    (define-key map (kbd "o") 'consult-outline) ;; Alt: consult-org-heading
    (define-key map (kbd "m") 'consult-mark)
    (define-key map (kbd "k") 'consult-global-mark)
    (define-key map (kbd "i") 'consult-imenu)
    (define-key map (kbd "I") 'consult-preoject-imenu))

  (let ((map search-map))
    (define-key map (kbd "f") 'consult-find)
    (define-key map (kbd "L") 'consult-locate)
    (define-key map (kbd "g") 'consult-grep)
    (define-key map (kbd "G") 'consult-git-grep)
    (define-key map (kbd "r") 'consult-ripgrep)
    (define-key map (kbd "l") 'consult-line)
    (define-key map (kbd "m") 'consult-multi-occur)
    (define-key map (kbd "k") 'consult-keep-lines)
    (define-key map (kbd "u") 'consult-focus-lines)
    ;; Isearch integration
    (define-key map (kbd "e") 'consult-isearch))

  (let ((map isearch-mode-map))
    (define-key map (kbd "M-e") 'consult-isearch)
    (define-key map (kbd "M-s e") 'consult-isearch)
    (define-key map (kbd "M-s l") 'consult-line))

  (let ((map global-map))
    ;; Register commands
    (define-key map (kbd "M-#") 'consult-register-load)
    (define-key map (kbd "M-'") 'consult-register-store) ; orig. abbrev-prefix-mark (unrelated)
    (define-key map (kbd "C-M-#") 'consult-register)
    ;; Yank commands
    (define-key map (kbd "M-y") 'consult-yank-pop)))

;;; Embark

(straight-use-package 'embark)
(config-with-package 'embark
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command) ;; by default describe-prefix-bindings

  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (let ((map global-map))
    (define-key map (kbd "C-.") 'embark-act)
    (define-key map (kbd "C-;") 'embark-dwim)
    (define-key map (kbd "C-h B") 'embark-bindings)))

(straight-use-package 'embark-consult)
(config-with-package 'embark-consult
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

;;; Wgrep

(straight-use-package 'wgrep)
(require 'wgrep)

;;; Dumb-jump

(straight-use-package 'dumb-jump)


;; (if emacs-28+-p
;;     ;; Repeatable key chords (Emacs 28+)
;;     (config-with-package 'repeat
;;       (defvar resize-window-repeat-map
;;         (let ((map (make-sparse-keymap)))
;;           (define-key map "^" 'enlarge-window)
;;           (define-key map "}" 'enlarge-window-horizontally)
;;           (define-key map "{" 'shrink-window-horizontally)
;;           (define-key map "v" 'shrink-window)
;;           map)
;;         "Keymap to repeat window resizing commands. Used in `repeat-mode'.")
;;       (put 'enlarge-window 'repeat-map 'resize-window-repeat-map)
;;       (put 'enlarge-window-horizontally 'repeat-map 'resize-window-repeat-map)
;;       (put 'shrink-window-horizontally 'repeat-map 'resize-window-repeat-map)
;;       (put 'shrink-window 'repeat-map 'resize-window-repeat-map)

;;       (repeat-mode 1)))

;;; LSP Mode

(straight-use-package 'lsp-mode)
(config-with-package 'lsp-mode
  (setq lsp-keymap-prefix "C-x l"))

;;; Lisp

(straight-use-package 'paredit)
(config-with-package 'paredit

  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-hook #'enable-paredit-mode)
  (add-hook 'racket-mode-hook #'enable-paredit-mode)

  (let ((map paredit-mode-map))
    ;; Switch C-backspace to use paredit-backward-kill to prevent
    ;; unbalancing parentheses
    (define-key map (kbd "<C-backspace>") 'paredit-backward-kill-word)
    (define-key map (kbd "<M-backspace>") 'backward-kill-word)
    ;; Remap the splice and split to not conflict with `search-map'.
    (define-key map (kbd "M-s") nil)
    (define-key map (kbd "M-S") nil)
    (define-key map (kbd "M-S s") 'paredit-splice-sexp)
    (define-key map (kbd "M-S S") 'paredit-split-sexp)))

;;;; Common Lisp

(straight-use-package 'sly)
(config-with-package 'sly
  (setq inferior-lisp-program "sbcl"))

;;;; Racket

(straight-use-package 'racket-mode)
(config-with-package 'racket-mode
  (add-hook 'racket-mode-hook #'racket-xp-mode))

;;; R - ESS

(straight-use-package 'ess)
(config-with-package 'ess
  (setq ess-use-flymake t))

;;; Julia

(straight-use-package 'julia-mode)
(straight-use-package 'julia-repl)
;;(straight-use-package 'julia-snail)
;;(add-hook 'julia-mode-hook #'julia-snail-mode)

(config-with-package 'julia-mode
  (setenv "JULIA_NUM_THREADS" "4")
  (add-hook 'julia-mode-hook #'julia-repl-mode))

(straight-use-package 'lsp-julia)
(config-with-package 'lsp-julia
  (add-hook 'julia-mode-hook #'lsp-deferred)
  (setq lsp-julia-default-environment "~/.julia/environments/v1.6"))

;;; Python

(config-with-package 'python
  (setq python-indent-offset 4))

(straight-use-package 'pyvenv)
(config-with-package 'pyvenv
  (setenv "WORKON_HOME" "~/.envs/"))

;;; AucTeX

(straight-use-package 'auctex)
(add-hook 'LaTeX-mode-hook 'outline-minor-mode)

(load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)

(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))

(add-hook 'LaTeX-mode-hook #'TeX-fold-mode)

(autoload #'LaTeX-math-mode "latex")
(add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)

(setq TeX-auto-save t
      TeX-parse-self t
      TeX-PDF-mode t)

(setq-default TeX-master nil)

(setq lsp-tex-server 'digestif)

;;; Nix

(straight-use-package 'nix-mode)

;;; Godot script

(straight-use-package 'gdscript-mode)
(config-with-package 'gdscript-mode
  (setq gdscript-use-tab-indents nil))

;;; Markdown

(straight-use-package 'markdown-mode)

;;;###autoload
(defun markdown-compile-pandoc (beg end output-buffer)
  "Compiles markdown with the pandoc program, if available.
  Returns its exit code."
  (when (executable-find "pandoc")
    (call-process-region beg end "pandoc" nil output-buffer nil
                         "-f" "markdown"
                         "-t" "html"
                         "--mathjax"
                         "--highlight-style=pygments")))

(config-with-package 'markdown-mode
    (add-to-list 'auto-mode-alist '("/README\\(|:\\.md\\)?\\'" . gfm-mode))

    (with-eval-after-load 'markdown-mode

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
                    "<script>document.addEventListener('DOMContentLoaded', () => { document.body.classList.add('markdown-body'); document.querySelectorAll('pre[lang] > code').forEach((code) => { code.classList.add(code.parentElement.lang); }); document.querySelectorAll('pre > code').forEach((code) => { hljs.highlightBlock(code); }); });</script>"))))

;;; Hightlight Todos

(straight-use-package 'hl-todo)
(config-with-package 'hl-todo
  (add-hook 'prog-mode-hook #'hl-todo-mode)
  (setq
   hl-todo-highlight-punctuation ":"
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

;;; Misc. Packages

(straight-use-package 'emacsql-sqlite3)
(straight-use-package 'autothemer)
(straight-use-package 'olivetti)
(straight-use-package 'hydra)
(straight-use-package 'ein)
(straight-use-package 'yasnippet)
(straight-use-package 'bufler)
(straight-use-package 'rainbow-mode)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'prism)

;;; Org

(straight-use-package 'org)
(straight-use-package 'org-ql)
(straight-use-package 'org-super-agenda)

(require 'org)
(require 'mentat-org-setup)

;;; Bookmark

(setq bookmark-fontify nil)
(setq org-capture-bookmark nil)

;;; Vterm

(when on-linux
  (straight-use-package 'vterm))

;;; Magit

(straight-use-package 'magit)
(config-with-package 'magitt
  (define-key ctl-x-map (kbd "g") 'magit-status))

;;; Deft

(straight-use-package 'deft)
(config-with-package 'deft

  (setq deft-directory "~/org/zk/"
	deft-extensions '("org" "md" "markdown" "txt")
	deft-default-extension "org")
  
  (define-key global-map (kbd "C-c d") 'deft))

;;; Elfeed

(straight-use-package 'elfeed)
(straight-use-package 'elfeed-org)
(require 'elfeed)
(require 'elfeed-org)
(elfeed-org)

;;; Kmonad

(straight-use-package '(kbd-mode :type git
                                 :host github
                                 :repo "kmonad/kbd-mode"))

;;; Envrc

(when on-linux
  (straight-use-package 'envrc)
  (config-with-package 'envrc
    (envrc-global-mode)))

;;; Minions

(straight-use-package 'minions)
(minions-mode 1)

;;; Objed - modal editing
(straight-use-package 'objed)
(objed-mode 1)
(global-set-key (kbd "s-t") 'objed-activate)

;;; Avy

(straight-use-package 'avy)
(setq avy-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o))


;;(straight-use-package 'boon)
;;(require 'boon-qwerty)
;;(boon-mode)

;;(straight-use-package 'god-mode)
;;(require 'god-mode)
;;(global-set-key (kbd "<escape>") #'god-local-mode)

;; (defun my-god-mode-update-cursor-type ()
;;   (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

;; (add-hook 'post-command-hook #'my-god-mode-update-cursor-type)

;; (defun my-god-mode-update-mode-line ()
;;   (cond
;;    (god-local-mode
;;     (set-face-attribute 'mode-line nil
;;                         :foreground "#604000"
;;                         :background "#fff29a")
;;     (set-face-attribute 'mode-line-inactive nil
;;                         :foreground "#3f3000"
;;                         :background "#fff3da"))
;;    (t
;;     (set-face-attribute 'mode-line nil
;; 			:foreground "#0a0a0a"
;; 			:background "#d7d7d7")
;;     (set-face-attribute 'mode-line-inactive nil
;; 			:foreground "#404148"
;; 			:background "#efefef"))))
;; ;; Or try 'god-mode-enabled-hook and 'god-mode-disabled-hook or `window hooks'.
;; (add-hook 'post-command-hook 'my-god-mode-update-mode-line)

;;(define-key god-local-mode-map (kbd "z") #'repeat)
;;(define-key god-local-mode-map (kbd "i") #'god-local-mode)
