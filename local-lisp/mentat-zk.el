(require 'f)
(require 'org-ql)
(require 'org-ql-view)
(require 'dash)

(defun mentat/find-files-with-ext (ext dir &optional recursive)
  "Find files with EXT in DIR.
If RECURSIVE is non-nil, searches recursively."
  (f-files dir
	   (lambda (file) (f-ext? file ext))
	   recursive))

(defun mentat/zk-files ()
  "Returns a list of the org files inside `zk-directory'."
  (mentat/find-files-with-ext "org" mentat/zk-directory))

(defun mentat/org-ql-search (target &optional query)
  "Perform an org-ql search with the specified TARGET.
If QUERY is not specified, read it from the minibuffer. "
  (let ((query (or query (read-from-minibuffer "Query : "))))
    (funcall #'org-ql-search target query)))

(defun mentat/org-ql-search-agenda (&optional query)
  "Search `org-agenda-files' with QUERY.
If QUERY is unspecified, read it from the minibuffer."
  (interactive)
  (mentat/org-ql-search (org-agenda-files) query))

(defvar mentat/zk-directory "~/org/notes/")

(defun mentat/org-ql-search-zk (&optional query)
  "Search `mentat/zk-directory' with the specified QUERY.
If QUERY is unspecified, read it from the minibuffer."
  (interactive)
  (mentat/org-ql-search (mentat/zk-files) query))

;;; Formatting

(defun mentat/zk-format-element (line)
  "Format org heading LINE derived from org-ql-views
Adds file name and outline path for more robust filtering in minibuffer"
  (let* ((marker (get-text-property 0 'org-hd-marker line))
	 (level (get-text-property 0 'level line))
	 (file (propertize (org-with-point-at marker
			     (or (file-name-nondirectory (buffer-file-name (buffer-base-buffer)))))
			   'face 'success))
	 (path (when (> level 1)
		 (propertize (org-with-point-at marker
			       (mapconcat #'substring-no-properties
					  (org-get-outline-path nil t) "/"))
			     'face 'completions-annotations)))
	 (new (concat file ":" (make-string (max 0 (- 15 (length file))) ? )
		      line (when path "\t\t\t || ")
		      path))
	 (props (text-properties-at 0 line)))
    (org-add-props new props)))

(defun mentat/zk-current-headline ()
  "Format the current entry headline.
Used as the default candidate when searching `current-buffer'."
  (save-excursion
    (when (derived-mode-p 'org-mode)
      (unless (or (org-before-first-heading-p)
		  (org-in-archived-heading-p))
	(org-back-to-heading)
	(mentat/zk-format-element
	 (org-ql-view--format-element
	  (org-ql--add-markers
	   (org-element-headline-parser (line-end-position)))))))))

(defun mentat/zk-format-candidates (headings)
  "Formats headings from Org format to zk completing read format."
  (--map (mentat/zk-format-element (org-ql-view--format-element it)) headings))

(defun mentat/zk-extract-candidate-info (candidate)
  (let ((id (get-text-property 0 'ID candidate))
	(mark (get-text-property 0 'org-hd-marker candidate))
	(title (get-text-property 0 'raw-value candidate)))
    `(,candidate . ((id . ,id) (mark . ,mark) (title . ,title)))))

(defun mentat/zk-collect-candidates ()
  "Collect headlines in org files inside `zk-directory'."
  (let* ((search-targets (mentat/zk-files))
	 (headings (org-ql-select search-targets t
		     :action 'element-with-markers))
	 (candidates (mentat/zk-format-candidates headings)))
    (mapcar #'mentat/zk-extract-candidate-info candidates)))

(defun mentat/zk-collect-backlinks (id)
  (let* ((search-targets (mentat/zk-files))
	 (uuid (format "id:%s" id))
	 (headings (org-ql-select search-targets `(link :target ,uuid)
		     :action 'element-with-markers))
	 (candidates (->> headings
			  (-map #'org-ql-view--format-element)
			  (-map #'mentat/zk-format-element))))
    (mapcar #'mentat/zk-extract-candidate-info candidates)))

(defvar mentat/zk-goto-heading-history nil)

(defun mentat/zk-goto-heading ()
  (interactive)
  (let* ((prompt "[GOTO] Headline: ")
	 (candidates (mentat/zk-collect-candidates))
	 (entry (completing-read prompt candidates
				 nil nil nil mentat/zk-goto-heading-history))
	 (marker (alist-get 'mark (cdr (assoc-string entry candidates)))))
    (if marker
	(org-goto-marker-or-bmk marker)
      (mentat/zk-new-note entry))))

(defvar mentat/zk-goto-backlink-history nil)

(defun mentat/zk-goto-backlink ()
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an Org buffer: %s" (buffer-name)))
  (let* ((id (org-id-get))
	 (candidates (mentat/zk-collect-backlinks id))
	 (entry (when candidates
		  (completing-read "[Backlinks] " candidates
				   nil t nil mentat/zk-goto-backlink-history)))
	 (marker (when entry (alist-get 'mark (cdr (assoc-string entry candidates))))))
    (if marker
	(org-goto-marker-or-bmk marker)
      (user-error "No Backlinks!"))))

(defun mentat/zk-new-note (&optional text slug)
  (interactive)
  (let* ((note-slug (or slug (read-from-minibuffer "Note Filename: ")))
	 (note-filename (concat mentat/zk-directory note-slug ".org")))
    (find-file note-filename)
    (insert (concat "* " text "\n"))))

(defun mentat/zk-insert-link (&optional id)
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an Org buffer: %s" (buffer-name)))
  (let* ((candidates (mentat/zk-collect-candidates))
	 (entry (completing-read "[LINK] Headline: " candidates))
	 (entry-data (cdr (assoc-string entry candidates)))
	 (title (alist-get 'title entry-data))
	 (mark (alist-get 'mark entry-data))
	 (id (org-id-get mark t)))
    (org-insert-link nil (format "id:%s" id) title)))

;;; Need to evaluate still

(defun mentat/org-post-follow-hooks (&optional base-buff)
  "Format buffer after visiting headline.
If visiting headline in a new buffer place BASE-BUFF bottom of the prev-buffer
list."
  (when (eq major-mode 'org-mode)
    (org-tree-to-indirect-buffer)
    (org-show-entry)
    (org-show-children)
    (unless (equal base-buff (caar (window-prev-buffers)))
      ;; The selected heading was in a different buffer.  Put the
      ;; non-indirect buffer at the bottom of the prev-buffers list
      ;; so it won't be selected when the indirect buffer is killed.
      (set-window-prev-buffers nil (append (cdr (window-prev-buffers))
                                           (list (car (window-prev-buffers))))))))

; (add-hook 'org-agenda-after-show-hook #'baal-org-post-follow-hooks)



(provide 'mentat-zk)
