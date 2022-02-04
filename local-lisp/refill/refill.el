(defvar refill-mode nil
  "Mode variable for refill minor mode.")

(make-variable-buffer-local 'refill-mode)

(defun refill-mode (&optional arg)
  "Refill minor mode."
  (interactive "p")
  (setq refill-mode
	(if (null arg)
	    (not refill-mode)
	  (> arg 0)))
  (if refill-mode
      (add-hook 'after-change-functions 'refill nil t)
    (remove-hook 'after-change-functions 'refill t)))

(if (not (assq 'refill-mode minor-mode-alist))
    (setq minor-mode-alist
	  (cons '(refill-mode " Refill")
		minor-mode-alist)))

(defun refill (start end len)
  "After a text change, refill the current paragraph."
  (let ((left (if (or (zerop len)	; this is an insertion
		      (not (before-2nd-word-p start)))
		  start
		(save-excursion
		  (max
		   (progn		; beginning of previous line
		     (goto-char start)
		     (beginning-of-line 0)
		     (point))
		   (progn		; beginning of paragraph
		     (goto-char start)
		     (backward-paragraph 1)
		     (point)))))))
    (if (or (and (zerop len)		; If it's an insertion
		 (same-line-p start end) ; that doesn't span lines
		 (short-line-p end)) ; and the line's still short
	    ; or we're at whitespace followed by whitespace
	    (and (eq (char-syntax (preceding-char)) ; and t
		     ?\ )
		 (looking-at "\\s *$")))
	nil ; do nothing
      (save-excursion
	(fill-region left end nil nil t))))) ; otherwise refill
;
(defun before-2nd-word-p (pos)
  "Does POS lie before the second word on the line?"
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (skip-syntax-forward (concat "^ "
				 (char-to-string
				  (char-syntax ?\n)))) ; skip to first space or newline
    (skip-syntax-forward " ")
    (< pos (point))))

(defun same-line-p (start end)
  "Are START and END on the same line?"
  (save-excursion
    (goto-char start)
    (end-of-line)
    (<= end (point))))

(defun short-line-p (pos)
  "Does line containing POS stay within `fill-column'?"
  (save-excursion
    (goto-char pos)
    (end-of-line)
    (<= (current-column) fill-column)))

