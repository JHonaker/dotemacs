;;; note-search.el --- A Note Search Tool            -*- lexical-binding: t; -*-

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

;; Photon

;;; Code:

(require 'dash)
(require 's)
(require 'f)



(defvar photon-directory "~/org/notes/"
  "The directory Photon will search for notes.")

(defvar photon-minimum-search-length 3
  "The minimum search length befor we begin to filter.")

(defvar photon--window-restore-point nil
  "The window setup to restore when finished with Photon.")

(defvar photon--full-file-cache nil
  "An alist (FILENAME . CONTENT) cache of note files.")

(defvar photon--filtered-file-cache nil
  "The cache of files relevant to the current search.")

(defvar photon--term-list-cache nil
  "The cache of search terms for incremental filtering.")

(defvar photon-mode-hook nil
  "List of functions to call when entering Photon mode.")

;;; Files and Search

(defun photon--files ()
  "Return a list of all files 'n `photon-directory'."
  (f-files photon-directory))

(defun photon--filter-terms (term-list &optional file-cache)
  "Return files in FILE-CACHE that contains all terms in TERM-LIST.
If FILE-CACHE is nil, use `photon--files'.

SEARCH should be a space separated sequence of search terms."
  (let ((file-cache (or file-cache (photon--files))))
    (-filter (lambda (filename)
	       (with-temp-buffer
		 (insert filename)
		 (insert-file-contents filename)
		 (photon--matches-all-terms-p term-list)))
	     file-cache)))

(defun photon--get-term-list ()
  "Return the term list. Assumes the current buffer is the Photon buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((raw-terms (buffer-substring-no-properties (point) (line-end-position))))
      (s-split " +" (s-trim raw-terms)))))

(defun photon--search-forward (term)
  "Searches forward in the current buffer for TERM.
Returns NIL if TERM does not occur in the forward direction."
  (search-forward term nil 'no-error))

(defun photon--matches-term-p (term)
  "Returns t if the current buffer contains a match for the TERM."
  (goto-char (point-min))
  (not (null (photon--search-forward term))))

(defun photon--matches-all-terms-p (term-list)
  "Returns t if the current buffer contains a match for each term in TERM-LIST"
  (-every #'photon--matches-term-p term-list))


;;; Display

;;; Main Function

(defun photon--update ()
  "The main entry for the functionality of Photon.

Calling `photon--update' grabs the search term from the first
line, initiates or continues the search, and updates the
displayed information."
  (interactive)
  (let ((term-list (photon--get-term-list))))
  )

;;; Mode Definition

(defvar photon-mode-map
  (let ((map (make-sparse-keymap)))
    ;;(define-key map (kbd "RET") #'some-function)
    map))

(define-derived-mode photon-mode fundatmental-mode "Photon"
  "Search text at the speed of light!
\\<photon-mode-map>"
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq default-directory photon-directory)
    (photon--save-window-restore-point)))

(defun photon--save-window-restore-point ()
  "Save the current window configuration for restoration after Photon exits."
  (setq photon--window-restore-point (current-window-configuration)))

(defun photon--buffer ()
  "Get or create the Photon buffer."
  (get-buffer-create "*Photon*"))

(defun photon ()
  "Start Photon."
  (interactive)
  (photon--save-window-restore-point))

(provide 'note-search)
;;; note-search.el ends here

(photon--matches-term-p "photon")
(photon--matches-all-terms-p '("photon"))
