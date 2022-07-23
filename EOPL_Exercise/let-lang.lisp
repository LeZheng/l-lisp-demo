
(defun empty-env ()
  (list 'empty-env))
(defun extend-env (var val env)
  (list 'extend-env var val env))
(defun apply-env (env search-var)
  (cond
    ((eql (car env) 'empty-env)
     (report-no-binding-found search-var))
    ((eql (car env) 'extend-env)
     (let ((saved-var (cadr env))
	   (saved-val (caddr env))
	   (saved-env (cadddr env)))
       (if (equal search-var saved-var)
	   saved-val
	   (apply-env saved-env search-var))))
    (t (report-invalid-env env))))
(defun report-no-binding-found (search-var)
  (error "No binding for ~s" search-var))

(defun report-invalid-env (env)
  (error "Bad environment: ~s" env))


(defmacro define-datatype (type-name type-predicate-name &rest variants)
  (let ((variant-pred-list (mapcar (lambda (v)
				     (intern (concatenate 'string
							  (symbol-name (car v))
							  "?")))
				   variants)))
    `(progn
       (defun ,type-predicate-name (e)
	 (some (lambda (p) funcall p e) ,variant-pred-list))
       ,@(mapcar (lambda (variant-exp)
		   (let* ((variant-name (car variant-exp))
			  (variant-fields (cdr variant-exp))
			  (variant-pred (intern (concatenate 'string
							     (symbol-name variant-name)
							     "?"))))
		     `(progn
			(defun ,variant-name (&rest fields)
			  (cons ',variant-name
				(mapcar (lambda (pred field)
					  (if (funcall pred field)
					      field
					      (error "field[~s] init field." field)))
					(mapcar #'cadr ',variant-fields)
					fields)))
			(defun ,variant-pred (e)
			  (eql (car e) ',variant-name))
			,@(loop for i from 0 below (length variant-fields)
				for field in variant-fields
				collect `(let ((index (+ 1 ,i)))
					   (defun ,(intern (concatenate 'string
									(symbol-name variant-name)
									"->"
									(symbol-name (car field)))) (e)
					     (nth index e)))))))
		 variants))))

(defmacro cases (type-name expression &rest clauses)
  (let ((exp-sym (gensym)))
    `(let ((,exp-sym ,expression))
       (cond ,@(mapcar (lambda (c)
			 (let ((variant-name (car c)))
			   (if (eql variant-name 'else)
			       `(t ,(cadr c))
			       (let ((var-pred (intern (concatenate 'string (symbol-name variant-name) "?")))
				     (bound-var-list (cadr c)))
				 (format t "var-pred:~A~%" var-pred)
				 `((,var-pred ,exp-sym)
				   (let ,(loop for i from 0 below (length bound-var-list)
					       for var in bound-var-list
					       collect (list var `(nth (1+ ,i) ,exp-sym)))
				     ,@(cddr c)))))))
		       clauses)))))

(defun number? (n)
  (numberp n))

(defun identifier? (e)
  (and
   (atom e)
   (not (eql e 'lambda))))

(defun boolean? (b)
  (typep b 'boolean))

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?)))

(defun expval->num (val)
  (cases expval val
	 (num-val (num) num)
	 (else (error "expval->num extract error ~A~%" val))))

(defun expval->bool (val)
  (cases expval val
	 (bool-val (bool) bool)
	 (else (error "expval->bool extract error ~A~%" val))))


(define-datatype program program?
  (a-program
   (exp1 expression?)))

(define-datatype expression expression?
  (const-exp
   (num number?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (zero?-exp
   (exp1 expression?))
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (var-exp
   (var identifier?))
  (let-exp
   (var identifier?)
   (exp1 expression?)
   (body expression?))
  (minus-exp
   (num number?))
  (add-exp
   (exp1 expression?)
   (exp2 expression?))
  (mul-exp
   (exp1 expression?)
   (exp2 expression?))
  (div-exp
   (exp1 expression?)
   (exp2 expression?))
  (equal-exp
   (exp1 expression?)
   (exp2 expression?))
  (greater-exp
   (exp1 expression?)
   (exp2 expression?))
  (less-exp
   (exp1 expression?)
   (exp2 expression?)))


(defun init-env ()
  (extend-env
   'i (num-val 1)
   (extend-env
    'v (num-val 5)
    (extend-env
     'x (num-val 10)
     (empty-env)))))


(defvar let-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(defvar let-grammar
  '((program (expression) a-program)

    (expression (number) const-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)
    
    (expression
     ("zero?" "(" expression ")")
     zero?-exp)

    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression (identifier) var-exp)

    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)   

    ))

(defun scan&parse (s)
    (funcall (make-string-parser let-lexical-spec let-grammar) s))

(defun run (string)
  (value-of-program (cons 'a-program (scan&parse string))))

(defun value-of-program (pgm)
  (format t "program:~A~%" pgm)
  (cases program pgm
	 (a-program (exp1)
		    (value-of exp1 (init-env)))))

(defun value-of (exp env)
  (format t "value-of:~A <= ~A~%" exp env)
  (cases expression exp
	 (const-exp (num) (num-val num))
	 (var-exp (var) (apply-env env var))
	 (diff-exp (exp1 exp2)
		   (let ((val1 (value-of exp1 env))
			 (val2 (value-of exp2 env)))
		     (let ((num1 (expval->num val1))
			   (num2 (expval->num val2)))
		       (num-val
			(- num1 num2)))))
	 (zero?-exp (exp1)
		    (let ((val1 (value-of exp1 env)))
		      (let ((num1 (expval->num val1)))
			(if (zerop num1)
			    (bool-val t)
			    (bool-val nil)))))
	 (if-exp (exp1 exp2 exp3)
		 (let ((val1 (value-of exp1 env)))
		   (if (expval->bool val1)
		       (value-of exp2 env)
		       (value-of exp3 env))))
	 (let-exp (var exp1 body)
		  (let ((val1 (value-of exp1 env)))
		    (value-of body
			      (extend-env var val1 env))))
	 (minus-exp (num)
		    (num-val (- num)))
	 (add-exp (exp1 exp2)
		  (let ((val1 (value-of exp1 env))
			(val2 (value-of exp2 env)))
		    (let ((num1 (expval->num val1))
			  (num2 (expval->num val2)))
		      (num-val
		       (+ num1 num2)))))
	 (mul-exp (exp1 exp2)
		  (let ((val1 (value-of exp1 env))
			(val2 (value-of exp2 env)))
		    (let ((num1 (expval->num val1))
			  (num2 (expval->num val2)))
		      (num-val
		       (* num1 num2)))))
	 (div-exp (exp1 exp2)
		  (let ((val1 (value-of exp1 env))
			(val2 (value-of exp2 env)))
		    (let ((num1 (expval->num val1))
			  (num2 (expval->num val2)))
		      (num-val
		       (/ num1 num2)))))
	 (equal-exp (exp1 exp2)
		    (let ((val1 (value-of exp1 env))
			  (val2 (value-of exp2 env)))
		      (let ((num1 (expval->num val1))
			    (num2 (expval->num val2)))
			(bool-val
			 (= num1 num2)))))
	 (greater-exp (exp1 exp2)
		      (let ((val1 (value-of exp1 env))
			    (val2 (value-of exp2 env)))
			(let ((num1 (expval->num val1))
			      (num2 (expval->num val2)))
			  (bool-val
			   (> num1 num2)))))
	 (less-exp (exp1 exp2)
		   (let ((val1 (value-of exp1 env))
			 (val2 (value-of exp2 env)))
		     (let ((num1 (expval->num val1))
			   (num2 (expval->num val2)))
		       (bool-val
			(< num1 num2)))))))
