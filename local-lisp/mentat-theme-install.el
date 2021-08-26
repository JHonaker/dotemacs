;;; -*-lexical-binding: t-*-

(defun mentat/install-themes (themes)
  "Installs the individual theme pacakges in the list THEMES.

Themes are provided as symbols or lists that represent
straight.el recipes as an example:

(install-themes '(doom-themes
                  (nano-theme :type git
                              :host github
                              :repo \"404cn/nano-theme.el\")))

Uses `straight.el' for installation."
  (if (null themes)
      nil
    (let ((theme (car themes))
	  (rest (cdr themes)))
      (condition-case err
	  (straight-use-package theme)
	(error (let ((name (if (atom theme)
			       theme
			     (car theme))))
		  
		 (display-warning 'mentat-theme-installer
				  (format "Installing `%s' failed." name)
				  :warning))))
      (mentat/install-themes rest))))



;; (defmacro mentat/install-themes (themes)
;;   "Installs the individual theme pacakges in the list THEMES.

;; Themes are provided as symbols or lists that represent
;; straight.el recipes as an example:

;; (install-themes doom-themes
;;                 (nano-theme :type git
;;                             :host github
;;                             :repo \"404cn/nano-theme.el\"))

;; Uses `straight.el' for installation."
;;   (if (null themes)
;;       nil
;;     `(cons (straight-use-package ',(car themes))
;; 	   (mentat/install-themes ,(cdr themes)))))

;; Better version of the above macro
;; (defmacro mentat/install-themes (themes)
;;   ``(,(straight-use-pacakge ,(car themes))
;;      ,@(mentat/install-themes ,(cdr themes))))

;; This is probably more straightforward, but I wanted to get the
;; recursive approach above correct.

;; (defmacro install-themes (&rest themes)
;;   "Installs the individual theme pacakges in the list THEMES.

;; Themes are provided as symbols or lists that represent
;; straight.el recipes as an example:

;; (install-themes doom-themes
;;                 (nano-theme :type git
;;                             :host github
;;                             :repo \"404cn/nano-theme.el\"))

;; Uses `straight.el' for installation."
;;   `(dolist (theme ,themes)
;;      (straight-use-package theme)))

(provide 'mentat-theme-install)
