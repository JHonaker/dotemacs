;;; completion-mod.el ---                            -*- lexical-binding: t; -*-

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

(straight-use-package 'vertico)
(straight-use-package 'embark)
(straight-use-package 'consult)
(straight-use-package 'marginalia)
(straight-use-package 'orderless)

(straight-use-package 'corfu)
(straight-use-package 'cape)

(straight-use-package 'embark-consult)

(with-eval-after-load 'consult
  (with-eval-after-load 'embark
    (require 'embark-consult)))


(defun completion/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent folder,
otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
	  (zap-up-to-char (- arg) ?/)
	(delete-minibuffer-contents))
    (backward-kill-word arg)))

;;;; Vertico
(vertico-mode)

(setq vertico-count 15)

(customize-set-variable 'vertico-cycle t)

;;;; Marginalia

(require 'marginalia)
(setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
(marginalia-mode 1)

;;;; Orderless

(require 'orderless)

(customize-set-variable 'completion-styles '(orderless))
(customize-set-variable 'completion-category-overrides
			'((file (styles . (partial-completion)))))
(customize-set-variable 'orderless-component-separator
			#'split-string-and-unquote)

;;;;; Orderless Style Dispatchers

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
				    my/initialism-if-comma))

;;;; Embark

(with-eval-after-load 'embark
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

  (push 'embark--allow-edit (alist-get 'revert-buffer embark-target-injection-hooks))
  (push 'embark--allow-edit (alist-get 'rename-file-and-buffer embark-target-injection-hooks)))


;;;; Consult

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

(with-eval-after-load 'consult

  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))

  (setq consult-narrow-key (kbd "C-+")))

;;;; Embark & Consult

(add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)

;;;; Completion Mechanisms

;; Replace `completing-read-multiple' with an enhanced version.
(advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

(setq tab-always-indent 'complete)

;;;;; Corfu

(corfu-global-mode)

(defun corfu-move-to-minibuffer ()
  (interactive)
  (let ((completion-extra-properties corfu--extra)
	completion-cycle-threshold completion-cycling)
    (apply #'consult-completion-in-region completion-in-region--data)))

;;;;; CAPE - Completion at Point Extensions

(dolist (cape-fn (list #'cape-file
		       #'cape-tex
		       #'cape-dabbrev
		       #'cape-keyword))
  (add-to-list 'completion-at-point-functions cape-fn))

;;;; Keybindings

;;;;; Embark
(define-key vertico-map (kbd "C-o") 'embark-export)
(define-key vertico-map (kbd "C->") 'embark-become)

(global-set-key (kbd "C-.") #'embark-act)
(global-set-key (kbd "C-;") #'embark-dwim)
(global-set-key [remap describe-bindings] #'embark-bindings)

(setq prefix-help-command #'embark-prefix-help-command)

;;;;; Consult

(define-key help-map "a" 'consult-apropos)

(define-key minibuffer-local-map (kbd "C-r") 'consult-history)

(let ((km mode-specific-map))
  (define-key km "h" 'consult-history)
  (define-key km "m" 'consult-mode-command)
  (define-key km "b" 'consult-bookmark))

(let ((km ctl-x-map))
  (define-key km (kbd "r b") 'consult-bookmark)
  (define-key km (kbd "M-:") 'consult-complex-command)
  (define-key km "b" 'consult-buffer)
  (define-key km "4 b" 'consult-buffer-other-window)
  (define-key km "5 b" 'consult-buffer-other-frame))

(let ((km goto-map)) ;; M-g
  (define-key km "e" 'consult-compile-error)
  (define-key km "f" 'consult-flymake)
  (define-key km "g" 'consult-goto-line)
  (define-key km (kbd "M-g") 'consult-goto-line)
  (define-key km "o" 'consult-outline)
  (define-key km "m" 'consult-mark)
  (define-key km "k" 'consult-global-mark)
  (define-key km "i" 'consult-imenu)
  (define-key km "I" 'consult-project-imenu))

(let ((km isearch-mode-map))
  (define-key km (kbd "M-e") 'consult-isearch)
  (define-key km (kbd "M-s e") 'consult-isearch)
  (define-key km (kbd "M-s l") 'consult-line))

(let ((km global-map))
   ;; Register commands
   (define-key km (kbd "M-#") 'consult-register-load)
   (define-key km (kbd "M-'") 'consult-register-store)
   (define-key km (kbd "C-M-#") 'consult-register)
   ;; Yank commands
   (define-key km (kbd "M-y") 'consult-yank-pop))

(with-eval-after-load 'embark
  (let ((km embark-become-file+buffer-map))
    (define-key km "m" 'consult-bookmark)
    (define-key km "b" 'consult-buffer)
    (define-key km "f" 'consult-find)))

;;;;; Marginalia

(define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle)

;;;;; Corfu

(define-key corfu-map (kbd "SPC") 'corfu-insert-separator)
(define-key corfu-map (kbd "M-m") 'corfu-move-to-minibuffer)

;;;;; CAPE

(let ((km mode-specific-map))
  (define-key km (kbd "p p") 'completion-at-point)
  (define-key km (kbd "p d") 'cape-dabbrev)
  (define-key km (kbd "p f") 'cape-file)
  (define-key km (kbd "p i") 'cape-ispell)
  (define-key km (kbd "p w") 'cape-dict)
  (define-key km (kbd "p \\") 'cape-tex))


(provide 'completion-mod)
;;; completion-mod.el ends here
