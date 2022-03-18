;;; tab-bar-mod.el ---                               -*- lexical-binding: t; -*-

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

;;;; Tab Bar Setup

(use-package tab-bar
  :config
  (defun my/tab-bar-format-menu-bar ()
    "Propertize the menu bar to show a custom character instead of the text `Menu'."
    `((menu-bar menu-item (propertize " λ " 'face 'tab-bar-tab-inactive)
		tab-bar-menu-bar :help "Menu Bar")))

  (add-to-list 'tab-bar-format #'my/tab-bar-format-menu-bar)
  (menu-bar-mode -1)
  (tab-bar-mode 1))


(provide 'tab-bar-mod)
;;; tab-bar-mod.el ends here
