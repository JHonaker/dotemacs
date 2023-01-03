;;; init.el --- My Emacs configuration               -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <john@coeus>
;; Keywords: lisp

;;; Package.el

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;;; UI

(scroll-bar-mode 0)
(tool-bar-mode 0)
(show-paren-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)

(setopt echo-keystrokes 0.01)

(minibuffer-depth-indicate-mode 1)

;;; Editing

(electric-pair-mode 1)
;;(electric-indent-mode 1)

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
