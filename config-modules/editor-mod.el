;;; editor-mod.el ---                                -*- lexical-binding: t; -*-

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

(straight-use-package 'ws-butler)
(straight-use-package 'scratch)

(add-hook 'text-mode-hook 'ws-butler-mode)
(add-hook 'prog-mode-hook 'ws-butler-mode)

;;;; The All Important Parentheses

(show-paren-mode 1)
(setq show-paren-when-point-in-periphery t)

;;;; Scratch buffers don't get no respect

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

(add-hook 'scratch-create-buffer-hook 'my/scratch-buffer-setup)

;;;; Outline Mode

;;;; Keybindings

(define-key mode-specific-map "s" 'scratch)

(with-eval-after-load 'outline
  (let ((km outline-minor-mode-map))
    (define-key km (kbd "<tab>") 'outline-cycle)
    (define-key km (kbd "C-c C-n") 'outline-next-visible-heading)
    (define-key km (kbd "C-c C-p") 'outline-previous-visible-heading)
    (define-key km (kbd "<backtab>") (lambda ()
				       (interactive)
				       (outline-back-to-heading)
				       (outline-cycle)))))

(provide 'editor-mod)
;;; editor-mod.el ends here
