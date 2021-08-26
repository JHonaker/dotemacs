(defun jh/find-user-init-file (&optional open-current-window-p)
  "Edit the `user-init-file', in another window. C-u to open in current window."
  (interactive "P")
  (if open-current-window-p
      (find-file user-init-file)
    (find-file-other-window user-init-file)))

(setq re-first-or-last-blank-line "\n[ \t]*\n")

(defun jh/insert-bracket-pair (left-bracket right-bracket &optional wrap-method)
  (if (use-region-p)
      (jh/wrap-region left-bracket right-bracket)
    (progn
      (cond
       ((eq wrap-method 'line)
	(jh/wrap-line left-bracket right-bracket))
       ((eq wrap-method 'block)
	(jh/wrap-block left-bracket right-bracket))
       ((point-at-bol-nonempty-p)
	(message "NONempty")
	(insert left-bracket)
	(end-of-line)
	(insert right-bracket))
       ((and (or (point-at-end-of-word-p)
		 (point-at-end-of-buffer-p))
	     (not (lisp-coding-mode-p)))
	(progn
	  (insert left-bracket right-bracket)
	  (search-backward right-bracket)))
       (t (let* ((start (jh/move-to-beginning-of-word))
		 (end (jh/move-to-end-of-word))
		 (shift-amount (+ (length left-bracket)
				  (length right-bracket))))
	    (message "T")
	    (goto-char end)
	    (insert right-bracket)
	    (goto-char start)
	    (insert left-bracket)
	    (goto-char (+ end shift-amount))))))))

(defun point-at-end-of-word-p ()
  (looking-at "[^_[:alnum:]]"))

(defun point-at-end-of-buffer-p ()
  (eq (point) (point-max)))

(defun jh/move-to-beginning-of-word ()
  (skip-chars-backward "-_[:alnum:]")
  (point))

(defun jh/move-to-end-of-word ()
  (skip-chars-forward "-_[:alnum:]")
  (point))

(defun lisp-coding-mode-p ()
  (or
   (string-equal major-mode "xah-elisp-mode")
   (string-equal major-mode "emacs-lisp-mode")
   (string-equal major-mode "lisp-mode")
   (string-equal major-mode "lisp-interaction-mode")
   (string-equal major-mode "common-lisp-mode")
   (string-equal major-mode "clojure-mode")
   (string-equal major-mode "xah-clojure-mode")
   (string-equal major-mode "scheme-mode")
   (string-equal major-mode "racket-mode")))

(defun jh/wrap-range (start-pos end-pos left-s right-s)
  (let* ((len-left (length left-s))
	 (len-right (length right-s))
	 (shifted-amount (+ len-left len-right)))
    (goto-char end-pos)
    (insert right-s)
    (goto-char start-pos)
    (insert left-s)
    (goto-char (+ end-pos shifted-amount))))

(defun jh/wrap-line (left-s right-s)
  (jh/wrap-range (line-beginning-position)
		 (line-end-position)
		 left-s right-s))

(defun jh/wrap-region (left-s right-s)
  (jh/wrap-range (region-beginning)
		 (region-end)
		 left-s right-s))

(defun jh/move-start-of-block ()
  (when (re-search-backward re-first-or-last-blank-line nil 'move)
    (re-search-forward re-first-or-last-blank-line))
  (point))

(defun jh/move-end-of-block ()
  (when (re-search-forward re-first-or-last-blank-line nil 'move)
    (re-search-backward re-first-or-last-blank-line))
  (point))

(defun jh/wrap-block (left-s right-s)
  (save-excursion
    (let ((block-start (jh/move-start-of-block))
	  (block-end (jh/move-end-of-block)))
      (jh/wrap-range block-start block-end
		     left-s right-s))))

(defun point-at-bol-nonempty-p ()
  (and (eq (point) (line-beginning-position))
       (not (eq (line-beginning-position) (line-end-position)))))

(defun jh/insert-paren ()
  (interactive)
  (jh/insert-bracket-pair "(" ")"))

(defun jh/insert-bracket ()
  (interactive)
  (jh/insert-bracket-pair "[" "]"))

(defun jh/insert-brace ()
  (interactive)
  (jh/insert-bracket-pair "{" "}"))

(defvar jh/left-brackets '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«" )
  "List of left bracket chars.")
(defvar jh/right-brackets '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»")
  "list of right bracket chars.")

(defun jh/backward-left-bracket ()
  (interactive)
  (re-search-backward (regexp-opt jh/left-brackets) nil t))

(defun jh/forward-right-bracket ()
  (interactive)
  (re-search-forward (regexp-opt jh/right-brackets) nil t))

(defun jh/goto-matching-bracket ()
  (interactive)
  (if (nth 3 (syntax-ppss))
      (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
    (cond
     ((eq (char-after) ?\")
      (forward-sexp))
     ((eq (char-before) ?\")
      (backward-sexp))
     ((looking-at (regexp-opt jh/left-brackets))
      (forward-sexp))
     ((looking-back (regexp-opt jh/right-brackets) (max (- (point) 1) 1))
      (backward-sexp))
     (t
      (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)))))


(defun jh/delete-backward-char-or-bracket-text ()
  (interactive)
  (if (and delete-selection-mode (region-active-p))
      (delete-region (region-beginning) (region-end))
    (cond
     ((looking-back "\\s)" 1)
      (if current-prefix-arg
	  (jh/delete-backward-bracket-pair)
	(jh/delete-backward-bracket-text)))
     ((looking-back "\\s(" 1)
      (backward-char)
      (forward-sexp)
      (if current-prefix-arg
	  (jh/delete-backward-bracket-pair)
	(jh/delete-backward-bracket-text)))
     ((looking-back "\\s\"" 1)
      (if (nth 3 (syntax-ppss))
	  (progn
	    (backward-char)
	    (jh/delete-forward-brackt-pairs (not current-prefix-arg)))
	(if current-prefix-arg
	    (jh/delete-backward-bracket-pair)
	  (jh/delete-backward-bracket-text))))
     (t
      (delete-char -1)))))

(defun jh/delete-backward-bracket-text ()
  (interactive)
  (forward-sexp -1)
  (mark-sexp)
  (kill-region (region-beginning) (region-end)))

(defun jh/delete-backward-bracket-pair ()
  (interactive)
  (let ((right-bracket-pos (point))
	left-bracket-pos)
    (forward-sexp -1)
    (setq left-bracket-pos (point))
    (goto-char right-bracket-pos)
    (delete-char -1)
    (goto-char left-bracket-pos)
    (delete-char 1)
    (push-mark (point) t)
    (goto-char (- right-bracket-pos 2))))

(defun jh/delete-forward-bracket-pairs (&optional delete-inner-text-p)
  (interactive)
  (cond
   (delete-inner-text-p
    (mark-sexp)
    (kill-region (region-beginning) (region-end)))
   (t
    (let ((left-bracket-pos (point)))
      (forward-sexp)
      (delete-char -1)
      (push-mark (point) t)
      (goto-char left-bracket-pos)
      (delete-char 1)))))

(defun jh/mark-text-in-between ()
  "Select text between closest left and right delimiters
This command does not consider nesting. For example, if text is
(a(b)c|) the selected char is 'c' not 'a(b)c'."
  (interactive)
  (let ((skip-chars "^\"`<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）〘〙")
	mark-start)

    (skip-chars-backward skip-chars)
    (setq mark-start (point))
    (skip-chars-forward skip-chars)
    (set-mark mark-start)))

(defun jh/mark-line (&optional NO-ADVANCE)
  "Mark current line. If region is active, advance selection down by one line.
Send a prefix command to inhibit advancement."
  (interactive "P")
  (cond ((and (not NO-ADVANCE) (region-active-p))
	 (forward-line)
	 (end-of-line))
	(t
	 (end-of-line)
	 (set-mark (line-beginning-position)))))

(defun jh/mark-block ()
  "Mark the current/next block. If region is active, extend selection down by one block."
  (interactive)
  (if (region-active-p)
      (re-search-forward re-first-or-last-blank-line nil 'move)
    (progn
      (skip-chars-forward " \n\t")
      (jh/move-start-of-block)
      (push-mark (point) t t)
      (jh/move-end-of-block))))

(defun jh/inside-sexp-p ()
  (not (jh/outside-sexp-p)))

(defun jh/outside-sexp-p ()
  (eq (syntax-ppss-depth (syntax-ppss)) 0))

(defun %jh/expand-selection-active-region ()
  (if (jh/inside-sexp-p)
      (progn
	(up-list -1 t t)
	(mark-sexp))
    (let ((region-b (region-beginning))
	  (region-e (region-end)))
      (goto-char region-b)
      (cond 
       
       ;; If region starts at beginning of current line.
       ((eq region-b (line-beginning-position)) 
      	(goto-char region-b)
	(let* ((eol (line-end-position))
	       (region-at-eol (= region-e eol))
	       (region-before-eol (< region-e eol))
	       (point-in-region (> region-e eol)))

	  (cond
	   (region-at-eol
	    ;; Extend region by one line
	    (jh/mark-line))
	   (region-before-eol
	    ;; Extend region to end of line
	    (end-of-line))
	   (point-in-region
	    (goto-char region-e)
	    (if (eq (point) (line-end-position))
		(jh/mark-line)
	      (end-of-line)))
	   (t (error "Should be unreachable. Region-end is not at, before, or after end of line.")))))
       ;; If region doesn't start at bol
       ((and (> (point) (line-beginning-position)) (<= (point) (line-end-position)))
	;; Extend to whole line
	(jh/mark-line 'NO-ADVANCE))
       (t
	nil)))))



(defun jh/extend-selection ()
  "Select the current word, bracket/quote expression, or expand selection.
Subsequent calls expand the selection.

When there is no selection:

- If point is on any type of bracket (including parentheses or
  quotation marks), select the entire bracketed expression
  including the bracket.

- Otherwise, select the current word.

When there is a selection, the selection behavior is still
experimental. But when the point is on any type of bracket, it
extends the selection to outer bracket."
  (interactive)
  (if (region-active-p)
      (%jh/expand-selection-active-region)
    (cond
     ;; Looking at an open paren
     ((looking-at "\\s(")
      (mark-sexp))
     ;; Looking at a closing paren
     ((looking-at "\\s)")
      (backward-up-list) (mark-sexp))
     ;; Looking at an string quotation
     ((looking-at "\\s\"")
      (mark-sexp))
     ;; Point at beginning of non-empy line
     ((and (eq (point) (line-beginning-position)) (not (looking-at "\n")))
      (jh/mark-line))
     ;; Looking at a symbol (\s_) or a word (\sw)
     ((or (looking-back "\\s_" 1) (looking-back "\\sw" 1))
      (push-mark)
      (skip-syntax-backward "_w" )
      (mark-sexp)
      (exchange-point-and-mark))  
     ;; Looking at whitespace on both sides
     ((and (looking-at "\\s ") (looking-back "\\s " 1))
      (skip-chars-backward "\\s " )
      (push-mark (point) t t)
      (skip-chars-forward "\\s "))
     ;; Lookint at newlines on both sides
     ((and (looking-at "\n") (looking-back "\n" 1))
      (skip-chars-forward "\n")
      (push-mark (point) t t)
      (re-search-forward re-first-or-last-blank-line))
     ;; Otherwise
     (t
      (mark-sexp)
      (exchange-point-and-mark))
     )))


(defun jh/goto-outermost-sexp (&optional pos)
  (interactive)
  (when (number-or-marker-p pos)
    (goto-char pos))
  (let ((p (point))
	(depth (syntax-ppss-depth (syntax-ppss))))
    (backward-up-list depth 'escape-strings 'no-syntax-crossing)
    (if (equal p (point))
	nil
      t)))

(defun jh/compact-parens (&optional begin end)
  "Remove whitespace and compact the parentheses at the send of an S-expression.
If the region is active, act on it instead of the root S-expression."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (save-excursion
       (jh/goto-outermost-sexp)
       (list (point) (scan-sexps (point) 1)))))
  (let ((begin begin) (end end))
    (when (not begin)
      (save-excursion
	(jh/goto-outermost-sexp)
	(setq begin (point)
	      end (scan-sexps (point) 1))))
    (jh/compact-parens-region begin end)))

(defun jh/compact-parens-region (begin end)
  "Remove whitespace and compact the parentheses at the end of a region."
  (interactive "r")
  (save-restriction
    (narrow-to-region begin end)
    (goto-char (point-min))
    (while (search-forward-regexp ")[ \t\n]+)" nil t)
      (let* ((stx (syntax-ppss (match-beginning 0)))
	     (string-or-comment (syntax-ppss-context stx)))
	(if string-or-comment
	    (search-forward ")")
	  (replace-match "))")
	  (search-backward ")"))))))

(defun jh/compact-blank-lines (&optional begin end n-newlines)
  "Replace consecutive blank lines with N-NEWLINES newline characters.

N-LINES by default is 2, meaning there will be one visible blank line.
If N-LINES is 0, the lines will be joined."
  (interactive (if (region-active-p)
		   (list (region-beginning) (region-end))
		 (list (point-min) (point-max))))
  (unless begin (setq begin (point-min)))
  (unless end (setq end (point-max)))
  (unless n-newlines (setq n-newlines 2))
  (save-excursion
    (save-restriction
      (narrow-to-region begin end)
      (goto-char (point-min))
      (while (search-forward-regexp "\n\\s-*\n\\(\\s-*\n\\)+" nil 'noerror)
	(replace-match (make-string n-newlines ?\n))))))

(defun jh/prettify-root-sexp ()
  "Prettify the root sexp of the current expression."
  (interactive)
  (save-excursion
    (jh/goto-outermost-sexp)
    (let ((p (point))
	  (p2 (scan-sexps (point) 1)))
      (save-excursion
	(save-restriction
	  (narrow-to-region p p2)
	  (goto-char (point-min))
	  (indent-sexp)
	  (jh/compact-parens-region (point-min) (point-max))
	  (jh/compact-blank-lines (point-min) (point-max))
	  (delete-trailing-whitespace (point-min) (point-max)))))))
