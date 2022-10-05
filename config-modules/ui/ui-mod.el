;;; ui-mod.el --- UI Module                          -*- lexical-binding: t; -*-

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

(straight-use-package 'rainbow-mode)

(require 'helpful-mod)
(require 'font-mod)
(require 'tab-bar-mod)
(require 'themes-mod)

(straight-use-package '(pulsar :host gitlab
                               :repo "protesilaos/pulsar"
                               :type git))

(pulsar-setup)


;;;; Line Numbers

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;;;; Visual Column Indicator

(custom-set-variables '(fill-column 80))
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)


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

;;;; Minibuffer customizations

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

;;;; Mode-line customization

(defcustom mode-line-alarm-flash-color "#F2804F"
  "The color to flash the mode-line when the visual alarm goes off.")

(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line mode-line-alarm-flash-color)
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

;;;; Colorization

(straight-use-package 'rainbow-delimiters)
(straight-use-package 'prism)
(straight-use-package 'hl-todo)

(add-hook 'prog-mode-hook #'hl-todo-mode)

(with-eval-after-load 'hl-todo
  (custom-set-variables
   '(hl-todo-highlight-punctuation ":")
   '(hl-todo-keyword-faces
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
       ("XXX" font-lock-constant-face bold)))))

(with-eval-after-load 'rainbow-mode
  (setq rainbow-ansi-colors nil)
  (setq rainbow-x-colors nil))

(provide 'ui-mod)
;;; ui-mod.el ends here
