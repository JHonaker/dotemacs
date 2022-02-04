;;; photon.el --- Quick movement you can see.        -*- lexical-binding: t; -*-

;; Copyright (C) 2021  John Honaker

;; Author: John Honaker <john@pop-os>
;; Keywords: convenience, tools

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

;; Package-Requires: ((dash "2.19.1"))


;; 

;;; Code:

(require 'dash)
(require 's)

;;;; Customize

(defgroup photon nil
  "Photon movement group"
  :group 'convenience)

(defface photon-unique-photon-face
  '((t :foreground "black" :underline t))
  "Face to display unique jump hints (photons)."
  :group 'photon)

(defface photon-photon-face
  '((t :foreground "blue" :underline t))
  "Face to display the jump hints (photons)."
  :group 'photon)

(defface photon-shortcut-photon-face
  '((t :foreground "red" :underline t))
  "Face to display shortucts during photon filtering."
  :group 'photon)

(defface photon-label-face
  '((t :foreground "white" :background "red"))
  "Face to display labels for jumping."
  :group 'photon)

(defface photon-dimmed-face
  '((t :foreground "gray"))
  "Face to use for the background when jumping with photon."
  :group 'photon)

;;;; Utility

(defun photon--ensure-buffer (&optional buffer-or-name)
  "Returns the buffer named BUFFER-OR-NAME.

If BUFFER-OR-NAME is nil, return `current-buffer'."
  (if buffer-or-name
      (get-buffer buffer-or-name)
    (current-buffer)))

(defun photon--gather (n lst)
  "Gather a LST into a list of lists of N successive items.

For example, (photon--gather 2 '(1 2 3 4)) ;; => ((1 2) (2 3) (3 4) (4)"
  (->> (-iota (length lst))      ;; For each index
       (--map (->> lst           ;; Take the list
		   (-drop it)    ;; Drop the first i items
		   (-take n))))) ;; And take the next n

(defmacro photon--with-region (start end &rest body)
  "Allows for the operation of BODY restricted to START and END."
  (declare (indent 2))
  `(save-excursion
    (save-restriction
      (narrow-to-region ,start ,end)
      (goto-char (point-min))
      ,@body)))

;;;; Data

(defun photon--create-candidate (char loc)
  "Create data for the jump candidates at LOC followed by CHAR"
  (list :char char :loc loc))

(defun photon--candidate-char (cand)
  "Return the character following the jump candidate CAND."
  (plist-get cand :char))

(defun photon--candidate-char-n (n cand)
  (save-excursion
    (goto-char (+ n
		  (photon--candidate-loc cand)))
    (char-after (point))))

(defun photon--candidate-loc (cand)
  "Return the location of the jump candidate CAND."
  (plist-get cand :loc))

(defun photon--candidate-label (cand)
  "Return the label of the jump candidate CAND."
  (plist-get cand :label))

(defun photon--candidate-overlays (cand)
  "Return the list of overlays for the jump candidate CAND."
  (plist-get cand :overlays))

(defun photon--candidate-start (cand)
  "Return the starting position of the jump candidate CAND."
  (photon--candidate-loc cand))

(defun photon--candidate-end (cand)
  "Return the end position of the jump candidate CAND."
  (+ (photon--candidate-start cand) 2))



;;;; Labels

(defun photon--create-label-trie (items labels)
  "Create trie for items using labels that is as balanced as possible.

For example:
(photon--create-label-trie '(1 2 3 4) '(a b)) ; => ((a (a 1) (b 2)) (b (a 3) (b 4)))"
  (let ((char (photon--candidate-char-n 1 (car items))))
    `(:branch ,char ,@(photon--create-label-trie-helper items labels))))

(defun photon--create-label-trie-helper (items labels)
  
  (let* ((base (length labels))
	 (n (length items))
	 (n-per (ceiling (/ (float n) base))))
    (cond ((<= n base)
	   (-map (lambda (item) `(:leaf ,@item))
		 (-zip labels items)))
	  (t
	   (->> items
		(-partition-all n-per)
		(--map (photon--create-label-trie-helper it labels))
		(-zip labels)
		(-list)
		(-map (lambda (item) `(:branch ,@item))))))))

(defun photon--tree-reduce-branches (collect-fn leaf-fn acc tree)
  "Reduce over branchs, collecting the result into the leaves.
Apply LEAF-FN at the end of the reduction.
Maintains tree structure.

LEAF-FN should take (acc leaf).
COLLECT-FN should take (acc item)."
  (cond
   ((eq (car tree) :leaf)
    (let* ((node (cadr tree))
	   (data (cddr tree))
	   (col (funcall collect-fn acc node)))

      `(:leaf ,node ,@(funcall leaf-fn col data))))
   ((eq (car tree) :branch)
    (let* ((node (cadr tree))
	   (branches (cddr tree))
	   (col (funcall collect-fn acc node)))
      `(:branch ,node ,@(--map (photon--tree-reduce-branches collect-fn
							    leaf-fn
							    col
							    it)
			       branches))))
   (t (error "Not a leaf or branch."))))

(defun photon--plist-update (plist key new-value)
  "Update the value of KEY in PLIST with NEW-VALUE.
Returns a copy."
  (cond
   ((null plist) (list key new-value))			; Reached the
							; end of the
							; list
   ((eq (car plist) key)
    (cons key (cons new-value (-copy (cddr plist)))))
   (t
    (cons (car plist)
	  (cons (cadr plist)
		(photon--plist-update (cddr plist) key new-value))))))

(defun photon--decorate-candidate (cand labels &optional buffer)
  (let ((rest labels)
	(offset 0)
	label
	ovrly
	overlays)
    (push (photon--make-jump-overlay cand buffer) overlays)
    (while rest
      (setq label (car rest))
      (setq ovrly (photon--make-label-overlay cand label offset buffer))
      (push ovrly overlays)
      (setq rest (cdr rest)
	    offset (1+ offset)))
    (photon--plist-update cand :overlays overlays)))


(defun photon--decorate-group (group &optional buffer)
  "Compute the labels "
  (photon--tree-reduce-branches
   #'photon--cons-acc
   (lambda (acc leaf)
     (photon--decorate-candidate leaf (reverse acc) buffer))
   '() group))

(defun photon--decorate (tree &optional buffer)
  (-map (lambda (branch)
	  (let ((label (cadr branch))
		(rest (--map (photon--decorate-group it buffer) (cddr branch))))
	    `(:branch ,label ,@rest)))
	tree)
  ;; (->> tree
  ;;      (--map (list :branch) (cddr it))
  ;;      (-flatten-n 1)
  ;;      (--map (photon--decorate-group it buffer)))
  )

(defun photon--cons-acc (acc item)
  "Conses ITEM onto ACC and returns result."
  (append (-list item) acc))

(defun photon--make-label-overlay (cand label offset &optional buffer)
  "Create overlay with the text LABEL shifted OFFSET from the end of CAND.

If BUFFER is nil, uses `current-buffer'."
  (let* ((start (+ (photon--candidate-end cand) offset))
	 (end (1+ start))
	 (overlay (photon--make-overlay-in-range start
						 end
						 'photon-label-face
						 buffer)))
    (overlay-put overlay 'display (char-to-string label))
    overlay))

;;;; Candidate Generation

(defun photon--unique-chars-in-region (start end)
  "Return a list of the unique characters between START and END.
The list will be a list of (char . position)."
  (photon--with-region start end
    (let (unique-chars)
      (while (< (point) (point-max))
	(let* ((ch (char-after))
	       (other-occurrence (assq ch unique-chars)))
	  (if other-occurrence
	      (setcdr other-occurrence 'not-unique)
	    (push (photon--create-candidate ch (point))
		  unique-chars)))
	(forward-char))
      (seq-remove
       (lambda (ch)
	 (let ((loc (cdr ch)))
	   (eq loc 'not-unique)))
       (nreverse unique-chars)))))

(defun photon--jump-candidates (char &optional buffer)
  "Return a list of candidate locations in the visible region of BUFFER that start with CHAR.

If BUFFER is nil, uses `current-buffer'."
  (let ((buffer (photon--ensure-buffer buffer))
	(window (get-buffer-window buffer)))
    (with-current-buffer buffer
      (photon--with-region (window-start window) (window-end window)
	(let (candidates)
	  (while (re-search-forward (regexp-quote (string char)) nil t)
	    (push (photon--create-candidate (char-after (point))
					    (match-beginning 0))
		  candidates))
	  (nreverse candidates))))))


;;;; Candidate Manipulation

(defun photon--candidates-overlap-p (c1 c2)
  "Returns t if c2 starts before c1 ends."
  (< (photon--candidate-start c2)
     (photon--candidate-end c1)))

(defun photon--non-overlapping-candidates (cands)
  "Filters CANDS to remove elements that overlap with their predecessor."
  (let* ((pairs  (photon--gather 2 cands))	; For each index
	 (non-overlapping 
	  (--remove (or (< (length it) 2) ; Keep it if it's the end of the list
			(photon--candidates-overlap-p
			 (car it)
			 (cadr it))) ; Or if it doesn't overlap
		    pairs))
	 (non-overlapping-cands (--map (car it) non-overlapping)))
    non-overlapping-cands))

(defun photon--group-candidates (cands)
  "Group a list of jump candidates CANDS by the character that follows the search."
  (--group-by (photon--candidate-char it) cands))

(defun photon--candidate-group-unique-p (group)
  "Return t if the candidate group only has a single option."
  (let ((candidates (cdr group)))
    (= (length candidates) 1)))

;;;; Overlay

(defun photon--make-overlay-in-range (beg end &optional face buffer)
  "Create a new overlay with FACE in range BEG to END in BUFFER and return it.

The overlay will be tagged in the 'photon category.
If FACE is nil, then no face will be applied.
If BUFFER is nil, the current buffer will be used."
  (let ((overlay (make-overlay beg end (photon--ensure-buffer buffer))))
    (overlay-put overlay 'category 'photon)
    (when face
      (overlay-put overlay 'face face))
    overlay))

(defun photon--make-overlay-at-loc (loc &optional face buffer)
  "Create a new overlay with FACE at LOC in BUFFER and return it.

The overlay will be tagged in the 'photon category.
If FACE is nil, then no face will be applied.
If BUFFER is nil, the current buffer will be used."
  (photon--make-overlay-in-range loc
				 (+ loc 2)
				 face
				 buffer))

(defun photon--make-overlay (cand &optional face buffer)
  "Create a new overlay with FACE at CAND's location in BUFFER and return it.

The overlay will be tagged in the 'photon category.
If FACE is nil, then no face will be applied.
If BUFFER is nil, the current buffer will be used."
  (photon--make-overlay-at-loc (photon--candidate-loc cand)
			       face
			       buffer))

(defun photon--make-dimming-overlay (&optional buffer)
  "Create a dimming overlay in BUFFER.

If WINDOW is nil, use `selected-window'."
  (let* ((buffer (photon--ensure-buffer buffer))
	 (window (get-buffer-window buffer))
	 (start (window-start window))
	 (end (window-end window)))
    (photon--make-overlay-in-range start
				   end
				   'photon-dimmed-face
				   buffer)))

(defun photon--make-jump-overlay (cand &optional buffer)
  "Place a photon jump overlay at CAND in BUFFER.

If BUFFER is nil, uses `current-buffer'"
  (photon--make-overlay cand 'photon-photon-face buffer))

(defun photon--make-unique-overlay (cand &optional buffer)
  "Place a photon jump overlay at CAND in BUFFER.

If BUFFER is nil, uses `current-buffer'"
  (photon--make-overlay cand 'photon-unique-photon-face buffer))

;; (defun photon--make-label-overlay (cand &optional buffer)
;;   "Place a label overlay after CAND in BUFFER.

;; If BUFFER is nil, uses `current-buffer'."
;;   (let ((label (photon--candidate))))
;;   (photon--make-overlay-in-range (photon--candidate-end cand) ))

(defun photon--decorate-candidates (list-of-cands decorate-fn &optional buffer)
  "Decorate all candidates in LIST-OF-CANDS it BUFFER using the DECORATE-FN.

If BUFFER is nil, uses `current-buffer'"
  (let (overlays)
    (dolist (cand list-of-cands overlays)
      (push (funcall decorate-fn cand buffer) overlays))))

(defun photon--group-overlay-type (group)
  "Returns the applicable overlay type for a group of candidates"
  (if (photon--candidate-group-unique-p group)
      'unique
    'default))

(defun photon--get-decorator (type)
  "Return the overlay decorator function for a group type."
  (cond
   ((eq type 'default) #'photon--make-jump-overlay)
   ((eq type 'unique) #'photon--make-unique-overlay)
   (t (error "Unknown candidate group type: %s" type))))

(defun photon--remove-overlays (&optional buffer)
  "Remove all overlays created with photon from BUFFER.

If BUFFER is nil, uses `current-buffer'."
  (let ((buffer (photon--ensure-buffer buffer)))
    (with-current-buffer buffer
      (save-restriction
	(widen)
	(remove-overlays nil nil 'category 'photon)))))

(defun photon--remove-overlays-from-tree (tree &optional buffer)
  (photon--tree-reduce-branches (lambda (acc item) acc)
				(lambda (acc leaf)
				  (--each (photon--candidate-overlays leaf)
				    (delete-overlay it)))
				'()
				tree))

;;;; Input

(defun photon--lowercase-char-p (char)
  "Is CHAR between 'a' and 'z'?"
  (<= 97 char 122))

(defun photon--uppercase-char-p (char)
  "Is CHAR between 'A' and 'Z'?"
  (<= 65 char 90))

(defun photon--enter-char-p (char)
  "Does CHAR correspond to <enter> (13, ^M)?"
  (= char 13))

;;;; Main Entry

(defun photon--read-char (&optional prompt)
  "Reads a character from input.
Translates Return into new-line (ASCII 13 to 10)."
  (let* ((raw-char (read-char prompt))
	(char (if (photon--enter-char-p raw-char)
		  10 ;; The ASCII code for \n
		raw-char)))
    char))

(defun photon--get-initial-candidates (char &optional buffer)
  "Create the initial jump candidates that start with CHAR."
  (photon--group-candidates
   (photon--jump-candidates char buffer)))

(defun photon--filter-candidates (char candidates)
  "Select the candidate group in CANDIDATES that is followed by CHAR."
  (let ((item (car candidates)))
    (cond
     ((null item)
      'not-found)
     ((eq (cadr item) char)
      item)
     (t
      (photon--filter-candidates char (cdr candidates)))))
  (-separate (lambda (item) (eq (cadr item) char)) candidates))

(defun photon--select-candidates (candidates)
  "Select the candidate group in CANDIDATES that is followed by CHAR."
  (let* (selected
	 split
	 not-selected
	 next-char)
    (while (null selected)
      (setq next-char (photon--read-char))
      (setq split (photon--filter-candidates next-char candidates))
      (setq selected (caar split)
	    not-selected (cadr split)))
    split))

(defun photon--jump-point-p (item)
  (eq (car item) :leaf))

(defun photon--group-decorator (group)
  "Return the decoration function for a group."
  (-> group
      photon--group-overlay-type
      photon--get-decorator))

(defvar photon-keys '(?a ?r ?s ?t ?n ?e ?i ?o))

(defun photon (&optional char buffer)
  (interactive)
  (unwind-protect
      (let* ((char (or char (photon--read-char)))
	     (raw-cands (photon--get-initial-candidates char buffer))
	     (cands (--map (photon--create-label-trie (cdr it) photon-keys) raw-cands)))
	(photon--make-dimming-overlay buffer)
	(setq cands (photon--decorate cands buffer))
	(while (not (photon--jump-point-p cands))
	  (let ((split-cands (photon--select-candidates cands))
		selected
		not-selected)
	    (setq selected (caar split-cands)
		  not-selected (cadr split-cands))
	    (if (photon--jump-point-p selected)
		(setq cands selected)
	      (setq cands (cddr selected))
	      (--each not-selected
		(photon--remove-overlays-from-tree it)))))
	(let* ((cand (cddr cands))
	       (jump-loc (photon--candidate-loc cand)))
	  (goto-char jump-loc)))
    (photon--remove-overlays buffer)))


(provide 'photon)
;;; photon.el ends here
