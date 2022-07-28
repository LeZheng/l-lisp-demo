
(defclass lc-exp ()
  ())

(defclass var-exp (lc-exp)
  ((var :type symbol)))

(defclass lambda-exp (lc-exp)
  ((bound-var :type symbol)
   (body :type lc-exp)))

(defclass app-exp (lc-exp)
  ((rator :type lc-exp)
   (rand :type lc-exp)))

(defun field-reader-name (type-sym field-sym)
  (concatenate 'string
	       (symbol-name type-sym)
	       "->"
	       (symbol-name field-sym)))

(defmacro define-datatype (type-name &rest variants)
  `(progn
     (defclass ,type-name () ())
     ,@(mapcar (lambda (variant)
		 (let ((name (car variant))
		       (fields (cdr variant)))
		   `(defclass ,name ()
		      ,(mapcar (lambda (field)
				 (list
				  (car field)
				  :type (cadr field)
				  :initarg (intern (symbol-name (car field)) 'keyword)
				  :accessor (intern (field-reader-name name (car field)))))
			       fields))))
	       variants)))
