;;; -*- lexical-binding: t -*-

;; NOTE I should probably make Emacs detect DPI instead of this hack.

(setq org-agenda-files '("~/org/refile.org"
			 "~/org/projects.org")
      org-default-notes-file "~/org/refile.org")

(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (R . t)
   (lisp . t)
   (emacs-lisp . t)
   (calc . t)))

(setq org-confirm-babel-evaluate nil)

(setq org-babel-lisp-eval-fn #'sly-eval)

(autoload #'org-indent-mode "org" nil t)
(add-hook 'org-mode-hook #'org-indent-mode)


(let ((map global-map))
  (define-key map (kbd "C-c l") 'org-store-link)
  (define-key map (kbd "C-c a") 'org-agenda)
  (define-key map (kbd "C-c c") 'org-capture)
  (define-key map (kbd "C-c b") 'org-switchb))


(setq org-log-done 'time)

(setq org-use-fast-todo-selection 'expert)
(setq org-fast-tag-selection-include-todo t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "PROJ(p)" "|" "DONE(d)")
	(sequence "WAITING(w@/!)" "INACTIVE(i)" "|" "CANCELLED(c@)")
	(sequence "AMOTIVATOR(MA)" "TMOTIVATOR(MT)" "CMOTIVATOR(MC)")))

(setq org-refile-targets '((nil :maxlevel . 9)
			   (org-agenda-files :maxlevel . 9)))

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

(setq org-archive-location "archive/%s_archive::")


;;; Agenda


;;;; Agenda Views

(defmacro def-ql (name query &optional header)
  "Defines an `org-ql-block' called NAME for the QUERY with an optional HEADER."
  (declare (indent 1))
  (let ((query-name (intern (concat (symbol-name name) "-query")))
	(block-name (intern (concat (symbol-name name) "-block")))
	(header-stmt (if header
			  `(((org-ql-block-header ,header)))
		       nil)))
    `(progn
      (setq ,query-name ,query)
      (setq ,block-name '((org-ql-block ,query
					,@header-stmt))))))

(def-ql mentat/all-projects
  '(todo "PROJ")
  "Projects")

(def-ql mentat/superprojects
  '(and (todo "PROJ")
	(descendants (todo "PROJ")))
  "Root Projects")

(def-ql mentat/subprojects
  '(and (todo "PROJ")
	(ancestors (todo "PROJ")))
  "Subprojects")

(setq mentat/all-project-tasks-query
      '(and (todo "TODO" "NEXT")
	(ancestors (todo "PROJ"))
	(or (not (scheduled))
	    (scheduled :to 0))))

(setq mentat/all-project-tasks-block
      '((org-ql-block mentat/all-project-tasks-query
		      ((org-ql-block-header "All Project Tasks")
		       (org-super-agenda-groups '((:auto-parent t)))))))

(def-ql mentat/scheduled-tasks
  '(and (todo "TODO" "NEXT")
	(scheduled :to 0))
  "Scheduled Tasks")

(def-ql mentat/due-soon
  '(and (todo "TODO" "NEXT")
	(deadline :to 14))
  "Tasks Due Soon")

(def-ql mentat/stuck-projects
  '(and (todo "PROJ")
	(not (descendants (todo "NEXT"))))
  "Stuck Projects")

(def-ql mentat/refile-tasks
  '(category "refile")
  "Tasks to Refile")

(def-ql mentat/closed-today
  '(closed :from 0 :to 0)
  "Completed Today")

(def-ql mentat/recently-closed
  '(closed :from -14 :to 0)
  "Recently Completed Tasks")

(def-ql mentat/waiting-tasks
  '(todo "WAITING")
  "Waiting Tasks")

(def-ql mentat/next-tasks
  '(and (todo "NEXT")
	(or (not (scheduled))
	    (scheduled :to 0)))
  "Next Tasks")

(setq org-agenda-custom-commands

      `(("d" "Day to Day View"
	 (,@mentat/next-tasks-block
	  ,@mentat/scheduled-tasks-block
	  ,@mentat/due-soon-block
	  ,@mentat/closed-today-block
	  ,@mentat/refile-tasks-block
	  ,@mentat/all-projects-block
	  ,@mentat/all-project-tasks-block))
	("w" "Weekly Review"
	 (,@mentat/refile-tasks-block
	  ,@mentat/waiting-tasks-block
	  ,@mentat/stuck-projects-block
	  ,@mentat/next-tasks-block
	  ,@mentat/all-projects-block))
	("r" "Review Report View"
	 (,@mentat/recently-closed-block
	  ,@mentat/waiting-tasks-block
	  ,@mentat/all-projects-block
	  ,@mentat/stuck-projects-block))))

;; Capture Templates

(setq org-capture-templates
  '(("t" "Task" entry (file org-default-notes-file)
     "* TODO %?
%t
%i
%a")))

;; Functions

(defun mentat/is-project-p ()
  "A task with a 'PROJ' keyword is a project."
  (member (nth 2 (org-heading-components)) '("PROJ")))

(defun mentat/find-project-task ()
  "Any task with a todo keyword that is in a project subtree"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
	(when (mentat/is-project-p)
	  (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun mentat/is-project-subtree-p ()
  "Any task wtih a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
			      (point))))
    (save-excursion
      (mentat/find-project-task)
      (if (equal (point) task)
	  nil t))))

;;; Review Functions

(defvar weekly-review-template "~/org/templates/weeklyreview.org")

(defun mentat/weekly-review ()
  "Open the Weekly Review checklist and agenda view in side-by-side windows.

Clears the current window setup."
  (interactive)
  (let ((buf-name "*Weekly Review*")
	(review-template-key "rr")))
  (delete-other-windows)
  (with-current-buffer (get-buffer-create buf-name)
    (insert-file-contents weekly-review-template)
    (org-mode))
  (switch-to-buffer buf-name)
  (org-agenda nil review-template-key))

(defun mentat/weekly-report ()
  (interactive)
  (let ((buf-name "*Weekly Report*"))
    (with-current-buffer (get-buffer-create buf-name)
      
      )
    )
  )

(defun ap/org-tree-to-indirect-buffer (&optional arg)
    "Create indirect buffer and narrow it to current subtree.
The buffer is named after the subtree heading, with the filename
appended.  If a buffer by that name already exists, it is
selected instead of creating a new buffer."
    (interactive "P")
    (let* ((new-buffer-p)
           (pos (point))
           (buffer-name (concat (org-link-display-format (nth 4 (org-heading-components)))
                                "::" (file-name-nondirectory (buffer-file-name (buffer-base-buffer)))))
           (new-buffer (or (get-buffer buffer-name)
                           (prog1 (condition-case nil
                                      (make-indirect-buffer (current-buffer) buffer-name 'clone)
                                    (error (make-indirect-buffer (current-buffer) buffer-name)))
                             (setq new-buffer-p t)))))
      (if arg
          (pop-to-buffer new-buffer)
        (switch-to-buffer new-buffer))
      (when new-buffer-p
        ;; I don't understand why setting the point again is necessary, but it is.
        (goto-char pos)
        (org-narrow-to-subtree))))
(advice-add 'org-tree-to-indirect-buffer :override 'ap/org-tree-to-indirect-buffer)

(provide 'mentat-org-setup)

