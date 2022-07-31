
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

(define-datatype lc-exp
    (var-exp
     (var symbol))
  (lambda-exp
   (bound-var symbol)
   (body lc-exp))
  (app-exp
   (rator lc-exp)
   (rand lc-exp)))

(defmacro define-datatype (type-name &rest variants)
  (flet ((field-reader-name (type-sym field-sym)
	   (concatenate 'string
			(symbol-name type-sym)
			"->"
			(symbol-name field-sym))))
    
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
		 variants))))

(defmacro cases (exp &rest clauses)
  (let ((exp-sym (gensym)))
    `(let ((,exp-sym ,exp))
       (ctypecase ,exp-sym
	 ,@(mapcar
	    #'(lambda (clause)
		`(,(car clause)
		  (with-slots ,(cadr clause) ,exp-sym
		    ,@(cddr clause))))
	    clauses)))))

(defun test1 ()
  (define-datatype lc-exp
      (var-exp
       (var symbol))
    (lambda-exp
     (bound-var symbol)
     (body lc-exp))
    (app-exp
     (rator lc-exp)
     (rand lc-exp)))
  (let ((v1 (make-instance 'var-exp :var 'int))
	(a1 (make-instance 'app-exp :rator '+ :rand '1))
	(l1 (make-instance 'lambda-exp :bound-var 'x :body '(+ 1 x))))
    (dolist (i (list v1 a1 l1))
      (cases i
	     (var-exp (var) (format t "var-exp var:~A~%" var))
	     (lambda-exp (bound-var body) (format t "lambda-exp bound-var[~A] body[~A]~%" bound-var body))
	     (app-exp (rator rand) (format t "app-exp rator[~A] rand[~A]~%" rator rand))))))
