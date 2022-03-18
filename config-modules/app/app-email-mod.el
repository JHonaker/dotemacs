;;; app-email-mod.el ---                             -*- lexical-binding: t; -*-

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

;; Installed via Guix

(custom-set-variables
 ;; This is set to 't' to avoid mail syncing issues when using mbsync
 '(mu4e-change-filenames-when-moving t)
 ;; Refresh mail using isync every 10 minutes
 '(mu4e-update-interval (* 10 60))
 '(mu4e-get-mail-command "mbsync -a")
 '(mu4e-maildir "~/Mail")

 '(mu4e-drafts-folder "/[Gmail]/Drafts")
 '(mu4e-sent-folder "/[Gmail]/Sent Mail")
 '(mu4e-refile-folder "/[Gmail]/All Mail")
 '(mu4e-trash-folder "/[Gmail]/Trash")
 '(mu4e-maildir-shortcuts ((:maildir "/Inbox" :key ?i)
			   (:maildir "/[Gmail]/Sent Mail" :key ?s)
			   (:maildir "/[Gmail]/All Mail" :key ?a)
			   (:maildir "/[Gmail]/Trash" :key ?t)
			   (:maildir "/[Gmail]/Drafts" :key ?d))))

(provide 'app-email-mod)
;;; app-email-mod.el ends here
