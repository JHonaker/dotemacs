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

;;(load-theme 'leuven-dark)
(straight-use-package 'kaolin-themes)
;;(load-theme 'kaolin-ocean)


(straight-use-package 'hydra)
(straight-use-package 'transient)
(straight-use-package 'which-key)
(which-key-mode 1)


(straight-use-package 'olivetti)
(custom-set-variables
 '(olivetti-body-width 0.85)
 '(olivetti-minimum-body-width 80))

(defvar symex-over-paredit-p nil)

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
(setq zk-file-extension "org")

(require 'zk)
(zk-setup-embark)

(require 'zk-consult)
(setq zk-grep-function 'zk-consult-grep)
(setq zk-tag-grep-function 'zk-consult-grep-tag-search)

(require 'zk-index)
(zk-index-setup-embark)
(setq zk-index-desktop-directory zk-directory)

(defun zk-org-try-to-follow-link (fn &optional arg)
  "When 'org-open-at-point' FN fails, try 'zk-follow-link-at-point'.
Optional ARG."
  (let ((org-link-search-must-match-exact-headline t))
    (condition-case nil
	(apply fn arg)
      (error (zk-follow-link-at-point)))))

(with-eval-after-load 'org
  (advice-add 'org-open-at-point :around #'zk-org-try-to-follow-link))

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

;; Tempel - Templates by minad using Tempo.el

(straight-use-package 'tempel)

(defun tempel-setup-capf ()
  ;; Add the Tempel Capf to `completion-at-point-functions'. `tempel-expand'
  ;; only triggers on exact matches. Alternatively use `tempel-complete' if
  ;; you want to see all matches, but then Tempel wil probably trigger too
  ;; often when you don't expect it.
  ;; NOTE: We add `temple-expand' *before* the main programming mode Capf,
  ;; such that it will be tried first.
  (setq-local completion-at-point-functions
              (cons #'tempel-expand
                    completion-at-point-functions)))

(add-hook 'prog-mode-hook 'tempel-setup-capf)
(add-hook 'text-mode-hook 'tempel-setup-capf)

(customize-set-variable 'tempel-path (no-littering-expand-etc-file-name "templates"))

;; Proof General - Proof Assistant for Coq

(straight-use-package 'proof-general)

;; Lean 4

(straight-use-package '(lean4-mode
                        :type git
                        :host github
                        :repo "leanprover/lean4-mode"))

(autoload #'lean4-mode "lean4-mode")

(defun my/lsp-mode-setup-completion ()
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless)))

(with-eval-after-load 'lsp-mode
  (customize-set-variable 'lsp-completion-provider :none)
  (add-hook #'lsp-completion-mode-hook 'my/lsp-mode-setup-completion))

(straight-use-package 'lean-mode)

;; Guix

;; Trying out themes

(straight-use-package 'stimmung-themes)
(straight-use-package '(alabaster-theme
                        :type git
                        :host github
                        :repo "uzhne/alabaster-emacs"
                        ))

;; (fringe-mode 10) ; set a 10 unit fringe
;; (setq frame-resize-pixelwise t
;;       default-frame-alist (append default-frame-alist
;;                                   (list
;;                                    '(internal-border-width . 24)
;;                                    '(right-fringe . 0))))

(straight-use-package 'hyperbole)

;; Agda mode
;; (load-file (let ((coding-system-for-read 'utf-8))
;;                 (shell-command-to-string "agda-mode locate")))

(straight-use-package 'meow)

;; (defun meow-setup ()
;;   (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
;;   (meow-motion-overwrite-define-key
;;    '("j" . meow-next)
;;    '("k" . meow-prev)
;;    '("<escape>" . ignore))
;;   (meow-leader-define-key
;;    ;; SPC j/k will run the original command in MOTION state.
;;    '("j" . "H-j")
;;    '("k" . "H-k")
;;    ;; Use SPC (0-9) for digit arguments.
;;    '("1" . meow-digit-argument)
;;    '("2" . meow-digit-argument)
;;    '("3" . meow-digit-argument)
;;    '("4" . meow-digit-argument)
;;    '("5" . meow-digit-argument)
;;    '("6" . meow-digit-argument)
;;    '("7" . meow-digit-argument)
;;    '("8" . meow-digit-argument)
;;    '("9" . meow-digit-argument)
;;    '("0" . meow-digit-argument)
;;    '("/" . meow-keypad-describe-key)
;;    '("?" . meow-cheatsheet))
;;   (meow-normal-define-key
;;    '("0" . meow-expand-0)
;;    '("9" . meow-expand-9)
;;    '("8" . meow-expand-8)
;;    '("7" . meow-expand-7)
;;    '("6" . meow-expand-6)
;;    '("5" . meow-expand-5)
;;    '("4" . meow-expand-4)
;;    '("3" . meow-expand-3)
;;    '("2" . meow-expand-2)
;;    '("1" . meow-expand-1)
;;    '("-" . negative-argument)
;;    '(";" . meow-reverse)
;;    '("," . meow-inner-of-thing)
;;    '("." . meow-bounds-of-thing)
;;    '("[" . meow-beginning-of-thing)
;;    '("]" . meow-end-of-thing)
;;    '("a" . meow-append)
;;    '("A" . meow-open-below)
;;    '("b" . meow-back-word)
;;    '("B" . meow-back-symbol)
;;    '("c" . meow-change)
;;    '("d" . meow-delete)
;;    '("D" . meow-backward-delete)
;;    '("e" . meow-next-word)
;;    '("E" . meow-next-symbol)
;;    '("f" . meow-find)
;;    '("g" . meow-cancel-selection)
;;    '("G" . meow-grab)
;;    '("h" . meow-left)
;;    '("H" . meow-left-expand)
;;    '("i" . meow-insert)
;;    '("I" . meow-open-above)
;;    '("j" . meow-next)
;;    '("J" . meow-next-expand)
;;    '("k" . meow-prev)
;;    '("K" . meow-prev-expand)
;;    '("l" . meow-right)
;;    '("L" . meow-right-expand)
;;    '("m" . meow-join)
;;    '("n" . meow-search)
;;    '("o" . meow-block)
;;    '("O" . meow-to-block)
;;    '("p" . meow-yank)
;;    '("q" . meow-quit)
;;    '("Q" . meow-goto-line)
;;    '("r" . meow-replace)
;;    '("R" . meow-swap-grab)
;;    '("s" . meow-kill)
;;    '("t" . meow-till)
;;    '("u" . meow-undo)
;;    '("U" . meow-undo-in-selection)
;;    '("v" . meow-visit)
;;    '("w" . meow-mark-word)
;;    '("W" . meow-mark-symbol)
;;    '("x" . meow-line)
;;    '("X" . meow-goto-line)
;;    '("y" . meow-save)
;;    '("Y" . meow-sync-grab)
;;    '("z" . meow-pop-selection)
;;    '("'" . repeat)
;;    '("<escape>" . ignore)
;;    '(":" . execute-extended-command)))
;; (require 'meow)
;; (meow-setup)
;; (meow-global-mode 1)

;; Denote
(straight-use-package 'denote)

(require 'denote)
(setq denote-directory (expand-file-name "~/notes/"))
(setq denote-known-keywords '("emacs" "math" "statistics" "cs" "proglang"))
(setq denote-infer-keywords t)
(setq denote-sort-keywords t)
(setq denote-prompts '(title keywords))

(setq denote-date-prompt-use-org-read-date t)

(setq denote-allow-multi-word-keywords t)
(setq denote-link-fontify-backlinks t)

(defun mt/denote-journal ()
  "Create an entry tagged 'journal', with the date as its title."
  (interactive)
  (denote
   (format-time-string "%A %e %B %Y") ; format like Tuesday 14 June 2022
   '("journal")))

(defun mt/denote-journal-with-tmr ()
  "Like `my-denote-journal', but also se a 10-minute times.
The `tmr' command is part of the `tmr' package."
  (interactive)
  (denote
   (format-time-string "%A %e %B %Y")
   '("journal"))
  (tmr "10" "Practice writing in my journal")) ; Set a 10 minutes timer with a description

(defun mt/denote-split-org-subtree ()
  "Create new Denote as an Org file using current Org subtree."
  (interactive)
  (let ((text (org-get-entry))
        (heading (org-get-heading :no-tags :no-todo :no-priority :no-comment))
        (tags (org-get-tags)))
    (delete-region (org-get-entry-beginning-position) (org-entry-end-position))
    (denote heading tags 'org)
    (insert text)))

(defvar prot-search--grep-hist '()
  "Input history of grep searches.")

;;;###autoload
(defun prot-search-grep (regexp &optional recursive)
  "Run grep for REGEXP.

Search in the current directory using `lgrep'. With optional
prefix argument (\\[universal-argument]) for RECURSIVE, run a
search starting wfrom the current directory with `rgrep'."
  (interactive
   (list
    (read-from-minibuffer (concat (if current-prefix-arg
                                      (propertize "Recursive" 'face 'warning)
                                    "Local")
                                  " grep for PATTERN: ")
                          nil nil nil 'prot-search--grep-hist)
    current-prefix-arg))
  (unless grep-command
    (grep-compute-defaults))
  (if recursive
      (rgrep regexp "*" default-directory)
;; or use consult
    (lgrep regexp "*" deft-directory)))



(let ((map (make-sparse-keymap)))
  (define-key global-map (kbd "C-c n") map)
  (define-key map (kbd "j") #'mt/denote-journal)
  (define-key map (kbd "n") #'denote)
  (define-key map (kbd "N") #'denote-type)
  (define-key map (kbd "d") #'denote-date)
  (define-key map (kbd "s") #'denote-subdirectory)
  (define-key map (kbd "t") #'denote-template)
  (define-key map (kbd "i") #'denote-link)
  (define-key map (kbd "I") #'denote-link-add-links)
  (define-key map (kbd "l") #'denote-link-find-file)
  (define-key map (kbd "b") #'denote-link-backlinks)
  (define-key map (kbd "r") #'denote-rename-file)
  (define-key map (kbd "R") #'denote-rename-file-using-front-matter))

(let ((map dired-mode-map))
  (define-key map (kbd "C-c C-d C-i") #'denote-link-dired-marked-notes)
  (define-key map (kbd "C-c C-d C-r") #'denote-dired-rename-marked-files)
  (define-key map (kbd "C-c C-d C-R")
              #'denote-dired-rename-marked-files-using-front-matter))



(straight-use-package 'ef-themes)
(load-theme 'ef-deuteranopia-light)

(straight-use-package 'tmr)
(straight-use-package 'agitate)
(straight-use-package 'fontaine)

;;; config.el ends here

;; (defun pixel-scroll-setup ()
;;   (interactive)
;;   (setq pixel-scroll-precision-large-scroll-height 1.0)
;;   (setq pixel-scroll-precision-interpolation-factor 1))

;; (when (boundp 'pixel-scroll-precision-mode)
;;   (pixel-scroll-setup)
;;   (add-hook 'prog-mode-hook #'pixel-scroll-precision-mode)
;;   (add-hook 'org-mode-hook #'pixel-scroll-precision-mode))
