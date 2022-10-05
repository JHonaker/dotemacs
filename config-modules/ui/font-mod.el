;;; font-mod.el --- Configure fontsets               -*- lexical-binding: t; -*-

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

;; TODO: Look into fontsets

(custom-set-faces
 '(default ((t (:family "Fira Code" :height 110))))
 '(fixed-pitch ((t (:family "Fira Code" :height 110))))
 '(variable-pitch ((t (:family "Fira Code"
			       :height 110)))))



(when (boundp 'text-scale-remap-header-line)
  (setq-default text-scale-remap-header-line t))

(provide 'font-mod)
;;; font-mod.el ends here

(print (font-family-list))

