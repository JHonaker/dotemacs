;;; mentat-pulse.el -*- lexical-binding: t -*-

(require 'pulse)

(defgroup mentat-pulse ()
  "Extenstions for `pulse.el'."
  :group 'editing)

(defcustom mentat-pulse-command-list
  '(recenter-top-bottom reposition-window)
  "Commands that should call `mentat-pulse-line' after invocation.

Re-toggle `mentat-pulse-advice-commands-mode' to enact changes."
  :type 'list
  :group 'mentat-pulse)

(defface mentat-pulse-line-face
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#8eecf4")
    (((class color) (min-colors 88) (background dark))
     :background "#004065")
    (t :inverse-video t))
  "Default face for `mentat-pulse-line'.";
  :group 'mentat-pulse)

(defun mentat-pulse-line (&optional face)
  "Pulse the current line with the optional FACE or
MENTAT-PULSE-LINE-FACE by default."
  (interactive)
  (let ((start (line-beginning-position))
	(end (line-beginning-position 2)) ; Go to start of next line to extend to edge
	(pulse-delay .04)
	(face (or face 'mentat-pulse-line-face)))
    (pulse-momentary-highlight-region start end face)))

(defvar mentat-pulse-after-command-hook nil
  "Hook that runs after select commands.
To be used with `addvice-add' after those functions declared in
`mentat-pulse-command-list'.")

(defun mentat-pulse-after-command (&rest _)
  "Run `mentat-pulse-after-command-hook'."
  (run-hooks 'mentat-pulse-after-command-hook))

;;;###autoload
(define-minor-mode mentat-pulse-advice-commands-mode
  "Setup for `mentat-pulse-command-list'."
  :init-value nil
  :global t
  (if mentat-pulse-advice-commands-mode
      (progn
	(dolist (fn mentat-pulse-command-list)
	  (advice-add fn :after #'mentat-pulse-after-command))
	(add-hook 'mentat-pulse-after-command-hook #'mentat-pulse-line))
    (dolist (fn mentat-pulse-command-list)
      (advice-remove fn #'mentat-pulse-after-command))
    (remove-hook 'mentat-pulse-after-command-hook #'mentat-pulse-line)))

(provide 'mentat-pulse)
