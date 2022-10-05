;;; early-init.el -*- lexical-binding: t; -*-

;; Native compilation settings

(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq native-comp-deferred-compilation t)
  (if (fboundp 'startup-redirect-eln-cache)
      (startup-redirect-eln-cache
       (convert-standard-filename
	(expand-file-name "var/eln-cache/" user-emacs-directory)))
    (add-to-list 'native-comp-eln-load-path
		 (expand-file-name "var/eln-cache/" user-emacs-directory))))

;; Do not initialize package.el at startup since we are using
;; straight.el to manage packages.
(setq package-enable-at-startup nil)

;; Some UI tweaks
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; (setq inhibit-splash-screen t)
(setq use-dialog-box t) ; only for mouse events
(setq use-file-dialog nil)

