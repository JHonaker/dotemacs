;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun mentat-minor-modes-active ()
  "Returns a list of active minor modes for the current buffer."
  (let (active-modes)
    (mapc (lambda (m)
	    (when (and (boundp m) (symbol-value m))
	      (push m active-modes)))
	  minor-mode-list)
    active-modes))

(provide 'mentat-core)
