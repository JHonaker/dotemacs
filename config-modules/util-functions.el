;;; util-functions.el --- Random assortment of useful functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  John Honaker

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

;;; Commentary:

;; 

;;; Code:

;;;###autoload
(defun my/find-file-dir (file)
    (interactive (list (read-file-name "Jump to dir of file: ")))
    (dired (file-name-directory file)))

;;;###autoload
(defun sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (when (file-writable-p file)
    (user-error "File is user writeable, aborting sudo"))
  (find-file (if (file-remote-p file)
                 (concat "/" (file-remote-p file 'method) ":"
                         (file-remote-p file 'user) "@" (file-remote-p file 'host)
                         "|sudo:root@"
                         (file-remote-p file 'host) ":" (file-remote-p file 'localname))
               (concat "/sudo:root@localhost:" file))))

;;;###autoload
(defun my/minor-modes-active ()
  "Returns a list of active minor modes for the current buffer."
  (let (active-modes)
    (mapc (lambda (m)
	    (when (and (boundp m) (symbol-value m))
	      (push m active-modes)))
	  minor-mode-list)
    active-modes))

;;;###autoload
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME." 
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (if (vc-registered filename)
            (vc-rename-file name new-name)
          (rename-file name new-name 1))
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

(defun jh/open-directory-in-named-tab (directory tab-name)
  "Opens the `directory' in a tab called `tab-name'.

Switches to or creates the tab if it does not exist."
  (tab-switch tab-name)
  (find-file directory))

;;;###autoload
(defun jh/open-emacs-config-dired ()
  "Opens the Emacs configuration directory in a tab called \"Emacs Config\"."
  (interactive)
  (jh/open-directory-in-named-tab
   user-emacs-directory
   "Emacs Config"))

(defun jh/open-nix-config-dired ()
  "Opens the NixOS configuration director in a tab called \"Nix Config\"."
  (interactive)
  (jh/open-directory-in-named-tab
   (expand-file-name "~/dots/nixos/")
   "Nix Config"))


(provide 'util-functions)
;;; util-functions.el ends here

