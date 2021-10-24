;; Do not initialize package.el at startup since we are using
;; straight.el to manage packages.
(setq package-enable-at-startup nil)

(tool-bar-mode -1)
(scroll-bar-mode -1)

;; (setq inhibit-splash-screen t)
(setq use-dialog-box t) ; only for mouse events
(setq use-file-dialog nil)

(setq native-comp-async-report-warnings-errors 'silent)
