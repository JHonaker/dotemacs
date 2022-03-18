;;; themes-mod.el ---                                -*- lexical-binding: t; -*-

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

;;;; Utilities

(defun my/install-themes (themes)
  "Installs the individual theme pacakges in the list THEMES.

Themes are provided as symbols or lists that represent
straight.el recipes as an example:

(install-themes '(doom-themes
                  (nano-theme :type git
                              :host github
                              :repo \"rougier/nano-theme.el\")))

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
      (my/install-themes rest))))


(setq custom-theme-directory (locate-user-emacs-file "themes"))

(straight-use-package 'autothemer)

(defgroup theme-mod ()
  "Variables for configuring the theme module.")

(defcustom my-theme-list
  '(doom-themes
    leuven-theme
    brutalist-theme
    challenger-deep-theme
    darcula-theme
    dracula-theme
    eink-theme
    espresso-theme
    flatland-theme
    gandalf-theme
    naysayer-theme
    northcode-theme
    tao-theme
    afternoon-theme
    humanoid-themes
    curry-on-theme
    nano-theme
    plan9-theme)
  "A list of themes to install by default."
  :type '(repeat symbol)
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (my/install-themes value))
  :group 'theme-mod)

;; (setq modus-themes-bold-constructs t
;;       modus-themes-syntax '(alt-syntax))
;; (modus-themes-load-themes)
;; (modus-themes-load-operandi)

(provide 'themes-mod)
;;; themes-mod.el ends here
