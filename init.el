;;; init.el -*- lexical-binding: t; -*-

;;; Bootstrapping and Setup

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s."
                     (emacs-init-time))))

;; Garbage Collector
(let ((normal-gc-cons-threshold (* 2 1000 1000))
      (init-gc-cons-threshold (* 100 1000 1000)))
  ;; Make garbage collector threshold larger during startup
  (setq gc-cons-threshold init-gc-cons-threshold)
  ;; And make it smaller after startup
  (add-hook 'after-init-hook
	    (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(require 'init-util (locate-user-emacs-file "init-util.el"))

(defvar modules-directory (locate-user-emacs-file "config-modules")
  "The location of the configuration module files.")

(defvar personal-packages-directory (locate-user-emacs-file "site-lisp")
  "The location of locally developed packages.")

;; Add the modules folders to the load path
(recursive-add-folder-to-load-path modules-directory)
(recursive-add-folder-to-load-path personal-packages-directory)

;; Confirm when killing Emacs.
(setq confirm-kill-emacs
      (lambda (prompt)
	(let ((seconds 2))
	  (y-or-n-p-with-timeout prompt seconds nil))))

;;; OS Predicates

(defconst emacs-28+-p (>= (string-to-number (substring emacs-version 0 2))
                          28)
  "Is t if `emacs-version' is at least version 28.")
(defconst on-windows (string-equal system-type "windows-nt")
  "Is the OS Windows?")
(defconst on-linux (string-equal system-type "gnu/linux")
  "Is the OS Linux?")

(require 'straight-mod)

(defvar config-file (locate-user-emacs-file "config.el")
  "My config file for loading modules and additional configuration.")


;;; Load Config.el

(when (file-exists-p config-file)
  (load config-file nil 'nomessage))

;;; Emacs Server

(add-hook 'after-init-hook
	  (lambda ()
	    (require 'server)
	    (unless (server-running-p)
	      (server-start))))

(when on-linux
  (require 'os-linux-mod))

(when on-windows
  (require 'os-msft-mod))

;;; init.el ends here
