;;; pulse-line-mod.el --- Pulse a light after a command   -*- lexical-binding: t; -*-

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

;;; Custom Variables:

(defgroup pulse-it-mod ()
  "Extenstions for `pulse.el'."
  :group 'editing)

(defcustom pulse-it-command-list
  '(recenter-top-bottom
    reposition-window
    move-to-window-line-top-bottom
    bookmark-jump
    other-window
    delete-window
    delete-other-windows
    forward-page
    backward-page
    scroll-up-command
    scroll-down-command
    windmove-left
    windmove-right
    windmove-up
    windmove-down
    windmove-swap-states-right
    windmove-swap-state-left
    windmove-swap-states-up
    windmove-swap-states-down
    tab-new
    tab-close
    tab-next)
  "Commands that should call `pulse-it-line' after invocation. "
  :type '(repeat function)
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
	 (if (eq value (default-value symbol))
	     (set-default symbol value)
	   (pulse-it-teardown)
	   (set-default symbol value)
	   (pulse-it-setup)))
  :group 'pulse-line-mod)

(defcustom pulse-it-enable t
  "When non-nil pulse line after commands in pulse-line-command-list."
  :type 'boolean
  :group 'pulse-it-mod)

(defcustom pulse-it-delay 0.04
  "How long to wait between pulse iterations in a `pulse-it-line' pulse."
  :type 'number
  :group 'pulse-it-mod)

(defcustom pulse-it-iterations 10
  "How many pulse iterations to perform in a `pulse-it-line' pulse."
  :type 'integer
  :group 'pulse-it-mod)

;;; Hooks

(defvar pulse-it-after-command-hook nil
  "Hook that runs after select commands.
To be used with `addvice-add' after those functions declared in
`puls-linee-command-list'.")

(defun pulse-it-after-command (&rest _)
  "Run `pulse-line-after-command-hook'."
  (run-hooks 'pulse-it-after-command-hook))

;;; Faces

(defgroup pulse-it-faces ()
  "Faces for the pulse-line module.")

(defface pulse-it-face
  '((t :inherit pulse-hightlight-start-face :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#8eecf4")
    (((class color) (min-colors 88) (background dark))
     :background "#8eecf4")
    (t :inverse-video t))
  "Default face for `pulse-line'.";
  :group 'pulse-it-faces)

;;; Functions

;;;###autoload
(defun pulse-it-line (&optional face)
  "Pulse the current line with the optional FACE or
PULSE-LINE-FACE by default."
  (interactive)
  (let ((start (line-beginning-position))
	(end (line-beginning-position 2)) ; Go to start of next line to extend to edge
	;(pulse-delay pulse-it-delay)
	;(pulse-iterations pulse-it-iterations)
	(face (or face 'pulse-it-face)))
    (pulse-momentary-highlight-region start end face)))

;;;###autoload
(defun pulse-line-setup ()
  (dolist (fn pulse-it-command-list)
    (advice-add fn :after #'pulse-it-after-command))
  (add-hook 'pulse-it-after-command-hook #'pulse-it-line))

(defun pulse-line-teardown ()
  (dolist (fn pulse-it-command-list)
    (advice-remove fn #'pulse-it-after-command))
  (remove-hook 'pulse-it-after-command-hook #'pulse-it-line))

(provide 'pulse-line-mod)
;;; pulse-line-mod.el ends here
