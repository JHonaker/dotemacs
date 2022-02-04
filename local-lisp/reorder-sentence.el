;;; -*- lexical-binding: t -*-

(defvar reorder-sentence--begin nil
  "The beginning of the sentence to be reordered.")

(defvar reorder-sentence--end nil
  "The end of the sentence to be reordered.")

(defconst reorder-sentence--buffer-name "*Reorder sentence*"
  "The name of the buffer used to construct a reordered sentence.")

(defvar reorder-sentence--buffer nil
  "The buffer used to construct a reordered sentence.")

(defvar reorder-sentence--previous-window-configuration nil
  "The window configuration from before reordering.")

(defun reorder-sentence (beg end)
  "Reorder the words in the region."
  (interactive (if (use-region-p)
		   (list (region-beginning) (region-end))
		 (reorder-sentence--current-sentence-bounds)))
  (setq reorder-sentence--begin (copy-marker beg t)
	reorder-sentence--end (copy-marker end)
	reorder-sentence--previous-window-configuration (current-window-configuration)
	reorder-sentence--buffer (get-buffer-create reorder-sentence--buffer-name t))
  (reorder-sentence--create-overlays reorder-sentence--begin reorder-sentence--end)
  (setq reorder-sentence--undo-stack nil)
  (with-current-buffer reorder-sentence--buffer
    (setq buffer-undo-list t)
    (erase-buffer))
  (display-buffer reorder-sentence--buffer)
  (deactivate-mark)
  (reorder-sentence-mode 1)
  (message
   (substitute-command-keys
    "Finish reordering with \\[reorder-sentence-finish] and cancel with \\[reorder-sentence-cancel].")))

(defun reorder-sentence--current-sentence-bounds ()
  "Find the positions of the current sentence's beginning and ending.
Use the preceding one of the point is between sentences. Return a two-element list containing them."
  ;; `backward-sentence' would move to previous one on bos
  (if (reorder-sentence--bos-p)
      (list (point)
	    (save-excursion
	      (forward-sentence)
	      (point)))
    (let (beg)
      (save-excursion
	(backward-sentence)
	(setq beg (point))
	(forward-sentence)
	(list beg (point))))))

(defun reorder-sentence--bos-p ()
  "Return t if at the beginning of a sentence."
  ;; because `forward-sentence' barfs at eobp
  (unless (eobp)
    (save-excursion
      (let ((pos (point)))
	(forward-sentence)
	(backward-sentence)
	(= pos (point))))))

(defun reorder-sentence-copy-word-at-point ()
  "Copy the word at point to the sentence reordering buffer."
  (interactive)
  (save-restriction
    (narrow-to-region reorder-sentence--begin reorder-sentence--end)
    (let ((word (current-word))
	  position)
      (cond ((> reorder-sentence--capitalize-count 0)
	     (setq word (capitalize word))
	     (setq reorder-sentence--capitalize-count (1- reorder-sentence--capitalize-count)))
	    ((< reorder-sentence--capitalize-count 0)
	     (setq word (downcase word))
	     (setq reorder-sentence--capitalize-count (1+ reorder-sentence--capitalize-count))))
      (with-current-buffer reorder-sentence--buffer
	(setq position (point))
	(when (not (bobp))
	  (insert " "))
	(insert word)
	(set-window-point (get-buffer-window reorder-sentence--buffer)
			  (point-max))
	position))))

(defun reorder-sentence-finish ()
  "Finirh the reordering of the sentence.
Replace the selected region with the constructed sentence and restore
the window configuration."
  (interactive)
  (reorder-sentence-mode -1)
  (reorder-sentence--remove-overlays)
  (setq reorder-sentence--capitalize-count 0)
  (goto-char reorder-sentence--begin)
  (delete-region reorder-sentence--begin reorder-sentence--end)
  (insert-buffer-substring reorder-sentence--buffer)
  (setq reorder-sentence--undo-stack nil)
  (set-window-configuration reorder-sentence--previous-window-configuration))

(defun reorder-sentence-cancel ()
  "Cancel the reordering of the sentence."
  (interactive)
  (reorder-sentence-mode -1)
  (reorder-sentence--remove-overlays)
  (setq reorder-sentence--capitalize-count 0)
  (setq reorder-sentence--undo-stack nil)
  (set-window-configuration
   reorder-sentence--previous-window-configuration))

;; Overlay

(defun reorder-sentence--put-overlay-on-word-at-point (key)
  "Put an overlay showing KEY over word at point and return it.
The overlay will display the KEY (a character) to the left."
  (let ((bounds (bounds-of-thing-at-point 'word))
	overlay)
    (when bounds
      (setq overlay (make-overlay (car bounds) (cdr bounds)))
      (overlay-put overlay 'before-string
		   (funcall reorder-sentence-prepare-key-for-display-function
			    key)))))

(defun reorder-sentence-prepare-key-for-display (key)
  "Format a string to mark a word using the KEY character."
  (propertize (format reorder-sentence-key-format key)
	      'face
	      'reorder-sentence-key))

(defun reorder-sentence--put-overlay-on-word-at-point (key)
  "Put an overlay showing KEY over a word at point and return it.
The overlay will display the KEY (a character) to the left."
  (let ((bounds (bounds-of-thing-at-point 'word))
	overlay)
    (when bounds
      (setq overlay (make-overlay (car bounds) (cdr bounds)))
      (overlay-put overlay 'before-string
		   (funcall reorder-sentence-prepare-key-for-display-function key))
      (overlay-put overlay 'reorder-sentence-key key)
      (overlay-put overlay 'reorder-sentence t))))

;; Generating the list of keys to use

(defun reorder-sentence--generate-keys ()
  "Return a list of characters to use for sentence reordering."
  (let ((list)
	(counter 97))
    (catch 'exit
      (while t
	(push counter list)
	(setq counter (1+ counter))
	(cond
	 ((= counter 123) (setq counter 48))
	 ((= counter 58) (setq counter 65))
	 ((= counter 91) (throw 'exit list)))))
    (nreverse list)))

;; Showing the keys for all words in the region

(defun reorder-sentence--create-overlays (begin end)
  "Create overlays for reordering region from BEGIN to END."
  (save-excursion
    (save-restriction
      (narrow-to-region begin end)
      (goto-char begin)
      (let ((keys (reorder-sentence--generate-keys)))
	(while (and keys
		    (< (point) end))
	  (forward-word)
	  (reorder-sentence--put-overlay-on-word-at-point (car keys))
	  (setq keys (cdr keys)))))))

(defun reorder-sentence--remove-overlays ()
  "Remove all reorder-sentence-related overlays from the buffer."
  (remove-overlays (point-min) (point-max) 'reorder-sentence t))

(defun reorder-sentence-copy-word ()
  "Copy word designated by the key used to invoke this function."
  (interactive)
  (let ((key last-command-event))
    (when (characterp key)
      (let* ((overlays (sort (overlays-in reorder-sentence--begin reorder-sentence--end)
			     (lambda (o1 o2)
			       (< (overlay-start o1) (overlay-start o2)))))
	     overlay
	     position)
	;; find the overlay corresponding to `key'
	(while (and overlays
		    (not (eq (overlay-get (car overlays) 'reorder-sentence-key)
			     key)))
	  (setq overlays (cdr overlays)))
	;; find the first active overlay
	(while (and overlays
		    (eq (overlay-get (car overlays) 'face)
			'reorder-sentence-inactive))
	  (setq overlays (cdr overlays)))
	;; copy the word
	(setq overlay (car overlays))
	(if overlay
	    (save-excursion
	      (goto-char (overlay-start overlay))
	      (setq position (reorder-sentence-copy-word-at-point))
	      (overlay-put overlay 'face 'reorder-sentence-inactive)
	      (push (position overlay) reorder-sentence--undo-stack))
	  (error "Key `%c' does not correspond to any word" key))))))

;; Mode

(define-minor-mode reorder-sentence-mode
  "Easily reorder a sentence or region.
\\<reorder-sentence-mode-map>
Use \\[reorder-sentence] to start a reordering session, press the
keys displayed before subsequent words to copy them into a temporary buffer,
press non-alphanumeric characters to insert themselves into the temporary
buffer, and press \\[reorder-sentence-finish] to finish. See below for more
keybindings.
Note: do not use \\[reorder-sentence-mode] directly.

\\{reorder-sentence-mode-map}"
  :lighter " Reorder sentence"
  :keymap `((,(kbd "C-c C-c") . reorder-sentence-finish)
	    (,(kbd "RET") . reorder-sentence-finish)
	    (,(kbd "C-c C-k") . reorder-sentence-cancel)
	    (,(kbd "M-c") . reorder-sentence-capitalize-word)
	    (,(kbd "M-l") . reorder-sentence-downcase-word)
	    ([remap undo] . reorder-sentence-undo)
	    (,(kbd "<backspace>") . reorder-sentence-undo)
	    ,@(mapcar (lambda (key)
			(cons (kbd (string key))
			      (if (reorder-sentence--copying-key-p key)
				  #'reorder-sentence-copy-word
				#'reorder-sentence-insert-punctuation)))
		      (reorder-sentence--range 32 127)))
  :interactive nil
  (if reorder-sentence-mode
      (setq buffer-read-only t)
    (setq buffer-read-only nil)))


;; Dealing with punctuation

(defun reorder-sentence-insert-punctuation ()
  "Insert the key typed in the sentence-reordering buffer."
  (interactive)
  (save-restriction
    (with-current-buffer reorder-sentence--buffer
      (push (cons (point) nil) reorder-sentence--undo-stack)
      (insert last-command-event)
      (set-window-point (get-buffer-window reorder-sentence--buffer)
			(point-max)))))

(defvar reorder-sentence-punctuation-characters "!\"'(),-.:;?"
  "Characters which should self-insert.")

(defun reorder-sentence--copying-key-p (key)
  "Return t if KEY should copy the corresponding word."
  (reorder-sentence--number-in-any-interval-p
   key
   reorder-sentence--word-copying-key-ranges))

(defun reorder-sentence--range (a b)
  "Return a list of numbers starting with A and ending before B."
  (let ((list ())
	(counter a))
    (while (< counter b)
      (push counter list)
      (setq counter (1+ counter)))
    (nreverse list)))

(defvar reorder-sentence--word-copying-key-ranges
  '((97 . 122)
    (48 . 57)
    (65 . 90)))

(defun reorder-sentence--generate-sequence (ranges)
  "Generate a sequence of numbers from RANGES.
It is a list of dotted pairs containing the first and last
element of a range."
  (let ((result))
    (dolist (range ranges)
      (push (reorder-sentence--range (car range) (1+ (cdr range)))
	    result))
    (apply #'append (nreverse result))))

(defun reorder-sentence--generate-keys ()
  "Return a list of characters to use for sentence reordering."
  (reorder-sentence--generate-sequence reorder-sentence--word-copying-key-ranges))

(defun reorder-sentence--number-in-any-interval-p (number intervals)
  "Return t if NUMBER is in any of the INTERVALS and nil otherwise.
INTERVALS is a list of conses, each describing a closed interval."
  (when intervals
    (or (<= (caar intervals) number (cdar intervals))
	(reorder-sentence--number-in-any-interval-p number (cdr intervals)))))

;; Dealing with Capitalization

(defvar reorder-sentence--capitalize-count 0
  "How many next words should be capitalized.")

(defun reorder-sentence-capitalize-word (count)
  "Capitalize next COUNT or previous -COUNT words."
  (interactive "p")
  (cond ((>= count 0)
	 (setq reorder-sentence--capitalize-count count))
	((< count 0)
	 (with-current-buffer reorder-sentence--buffer
	   (goto-char (point-max))
	   (capitalize-word count)))))

(defun reorder-sentence-downcase-word (count)
  "Downcase next COUNT or previous -COUNT words."
  (interactive "p")
  (cond ((>= count 0)
	 (setq reorder-sentence--capitalize-count  (- count)))
	((< count 0)
	 (with-current-buffer reorder-sentence--buffer
	   (goto-char (point-max))
	   (downcase-word count)))))

;; Undoing

(defvar reorder-sentence--undo-stack nil
  "The stack holding overlays corresponding to copied words.")

(defun reorder-sentence-undo (count)
  "Undo COUNT changes while reordering a sentence."
  (interactive "p")
  (let (undo-entry overlay position)
    (dotimes (_ count)
      (setq undo-entry (pop reorder-sentence--undo-stack))
      (when undo-entry
	(setq position (car undo-entry))
	(setq overlay (cdr undo-entry))
	(when (overlayp overlay)
	  (overlay-put overlay 'face nil))
	(with-current-buffer reorder-sentence--buffer
	  (delete-region position (point-max)))))))

;; Customize

(defgroup reorder-sentence nil
  "Reordering words."
  :group 'convenience)

(defface reorder-sentence-key
  '((t :foreground "chocolate" :underline t :height 0.9))
  "Face to display the key for sentence reordering."
  :group 'reorder-sentence)

(defcustom reorder-sentence-key-format "%c"
  "Format string to display the key for sentence reordering."
  :group 'reorder-sentence)

(defcustom reorder-sentence-prepare-key-for-display-function
  #'reorder-sentence-prepare-key-for-display
  "A function used to highlight the keys for reordering the sentence.
It should accept a character and return a string."
  :group 'reorder-sentence)

(defface reorder-sentence-inactive
  '((t :inherit 'shadow :strike-through t))
  "Face to display already copied words."
  :group 'reorder-sentence)

; This is a test of the stuff.

