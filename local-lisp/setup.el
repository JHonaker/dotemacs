
(defmacro config-with-package (package &rest body)
  "Requires PACKAGE and evaluates the forms in BODY. If there is an error, warn that loading failed.

PACKAGE is a quoted symbol. BODY is a series of lisp forms."
  (declare (indent 1))
  `(if (require ,package nil 'noerror)
       (progn ,@body)
     (display-warning 'mentat-config
		      (format "Loading `%s' failed." ,package)
		      :warning)))




