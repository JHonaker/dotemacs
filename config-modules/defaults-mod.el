;;; defaults-mod.el --- Common default settings      -*- lexical-binding: t; -*-

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

;;; Revert mode should be on by default
(customize-set-variable 'global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

;; Keep .emacs.d clean
(require 'no-littering)

;; Use spaces!
(setq-default indent-tabs-mode nil)

;; Recentf mode
(setq-default recentf-max-saved-items 100)

(require 'recentf)

(custom-set-variables
 '(backup-by-copying t)
 '(version-control t)
 '(backup-by-copying t)
 '(kept-new-versions 10)
 '(kept-old-versions 5))

(add-to-list 'recentf-exclude no-littering-etc-directory)
(add-to-list 'recentf-exclude no-littering-var-directory)

;; Autosave

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Customize File

(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Do not save duplicates in kill-ring
(customize-set-variable 'kill-do-not-save-duplicates t)

(global-so-long-mode 1)

;; Make #! files executable when saved
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Save command history
(savehist-mode 1)

(customize-set-variable 'sentence-end-double-space nil)


(setq delete-by-moving-to-trash t)

(setq read-process-output-max (* 1024 1024))

(auto-insert-mode)

;; Unbind these awful keys
(global-unset-key (kbd "<insert>"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(provide 'defaults-mod)
;;; defaults-mod.el ends here