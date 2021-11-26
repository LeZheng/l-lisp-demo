;;;;0.0 parser start
;;;TODO 这个解析器需要重写，把词法分析和语法分析分开
(defun subseq-and-trim (&rest s)
  (string-trim '(#\Space #\Tab #\NewLine)
	       (apply #'subseq s)))

(defun start-with (s prefix)
  (= (string/= s prefix) (length prefix)))

(defun skip-prefix (s prefix)
  (let ((start (string/= s prefix)))
    (if (= start (length prefix))
	(subseq-and-trim s start)
      (error "skip failed:~A" prefix))))

(defun scan&parse (s)
  (list 'a-program
	(parse-expression s)))

(defun parse-expression (s)
  (cond
   ((start-with s "zero? ")
    (parse-zero?-exp s))
   ((start-with s "if ")
    (parse-if-exp s))
   ((start-with s "let ")
    (parse-let-exp s))
   ((or (start-with s "- ") (start-with s "-("))
    (parse-diff-exp s))
   ((numberp (read-from-string s))
    (parse-const-exp s))
   ((symbolp (read-from-string s))
    (parse-var-exp s))
   (t (error "unknown expression:~A~%" s))))

(defun parse-zero?-exp (s)
  (let ((exp1-start (1+ (or (position #\( s)
			    (error "parse-zero?-exp:position #\( error")))))
    (multiple-value-bind
     (exp1 s) (parse-expression (subseq-and-trim s exp1-start))
     (let ((exp-end (1+ (or (position #\) s)
			    (error "parse-zero?-exp:position #\) error")))))
       (values (list 'zero?-exp exp1)
	       (subseq-and-trim s exp-end))))))

(defun parse-const-exp (s)
  (multiple-value-bind
   (num exp-end) (read-from-string s)
   (values (list 'const-exp num)
	   (subseq-and-trim s exp-end))))

(defun parse-var-exp (s)
  (multiple-value-bind
   (var exp-end) (read-from-string s)
   (values (list 'var-exp var)
	   (subseq-and-trim s exp-end))))

(defun parse-diff-exp (s)
  (multiple-value-bind
   (exp1 s) (parse-expression (subseq-and-trim
			       s
			       (1+ (or (position #\( s)
				       (error "parse-diff-exp:position #\( error")))))
   (multiple-value-bind
    (exp2 s) (parse-expression (subseq-and-trim
				s
				(1+ (or (position #\, s)
					(error "parse-diff-exp:position #\, error")))))
    (values (list 'diff-exp exp1 exp2) s))))

(defun parse-if-exp (s)
  (multiple-value-bind
   (exp1 s) (parse-expression (skip-prefix "if"))
   (multiple-value-bind
    (exp2 s) (parse-expression (skip-prefix "then"))
    (multiple-value-bind
     (exp3 s) (parse-expression (skip-prefix "else"))
     (values (list 'if-exp exp1 exp2 exp3) s)))))

(defun parse-let-exp (s)
  (multiple-value-bind
   (var s) (parse-expression (skip-prefix "let"))
   (multiple-value-bind
    (exp1 s) (parse-expression (skip-prefix "="))
    (multiple-value-bind
     (body s) (parse-expression (skip-prefix "in"))
     (values (list 'let-exp var exp1 body) s)))))	    
;;;;0.0 parser end

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
      (if (eql search-var saved-var)
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
			       `(t ,(cdr c))
			     (let ((var-pred (intern (concatenate 'string (symbol-name variant-name) "?")))
				   (bound-var-list (cadr c)))
			       `((,var-pred ,exp-sym)
				 (let ,(loop for i from 0 below (length bound-var-list)
					     for var in bound-var-list
					     collect (list var `(nth (1+ ,i) ,exp-sym)))
				   ,@(cddr c)))))))
		       clauses)))))

(defun split-symbol-p (c)
  (case c
	(#\( t)
	(#\) t)
	(#\Space  t)
	(#\, t)
	(otherwise nil)))

(defun parse-let-exp (s c)
  (let* ((eql-pos (position #\= s))
	 (var (string-trim " " (subseq s 3 position))))
    (scan&parse
     (subseq s (+ position 1))
     (lambda (exp1 res-str)
       (scan&parse
	(subseq (string-trim " " res-str) 2)
	(lambda (body res-str)
	  (funcall c
		   (let-exp var exp1 body)
		   res-str)))))))

(defun parse-if-exp (s c)
  (let ((exp1-start (+ (position #\Space s) 1)))
    (scan&parse
     (subseq s exp1-start)
     (lambda (exp1 res-str)
       (scan&parse
	(subseq (string-trim " " res-str) 4)
	(lambda (exp2 res-str)
	  (scan&parse
	   (subseq (string-trim " " res-str) 4)
	   (lambda (exp3 res-str)
	     (funcall c
		      (if-exp exp1 exp2 exp3)
		      res-str)))))))))

(defun parse-op-exp (s c)
  (let* ((op-end (position #\Space s))
	 (op (subseq s 0 op-end))
	 (arg-start (position #\( s))
	 (arg-list nil))
    (labels ((arg-collector (exp res-str)
			    (setf arg-list (cons exp arg-list))
			    (let ((res-str (string-trim " " res-str)))
			      (ecase (svref res-str 0)
				    (#\, (scan&parse (subseq res-str 1) #'arg-collector))
				    (#\) (funcall c
						  (create-op-exp op (reverse arg-list))
						  (subseq res-str 1)))))))
	    (scan&parse
	     (subseq s (+ arg-start 1))
	     #'arg-collector))))

(defun create-op-exp (op arg-list)
  (ecase op
	 ("-" (apply #'diff-exp arg-list))
	 ("zero?" (apply #'zero?-exp arg-list))))

(defun scan&parse (s c)
  (if (> (length s) 0)
      (let* ((str (string-trim "\n " s))
	     (split-index (position-if #'split-symbol-p str))
	     (token (subseq str 0 split-index)))
	(case token
	      ("-" (parse-diff-exp str (lambda (exp res-str)
					 (funcall c exp res-str))))
	      ("let" (parse-let-exp str (lambda (exp res-str)
					   (funcall c exp res-str))))
	      ("zero?" (parse-zerop-exp str (lambda (exp res-str)
					      (funcall c exp res-str))))
	      ("if" (parse-if-exp str (lambda (exp res-str)
					(funcall c exp res-str))))
	      (otherwise (parse-var-const-exp str (lambda (exp res-str)
						    (funcall c exp res-str))))))))

(defun number? (n)
  (numberp n))

(defun identifier? (e)
  (and
   (atom e)
   (not (eql e 'lambda))))

(defun boolean? (b)
  (typep b 'boolean))

;;;;3.2
;;;exercise 3.1
;;翻译成人话：n的表达式的值为n
;;x 3 v i 都用到了这个事实

;;;exercise 3.2
;;-0

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

(defun run (string)
  (value-of-program (scan&parse string)))

(defun value-of-program (pgm)
  (cases (pgm)
	 (a-program (exp1)
		    (value-of exp1 (init-env)))))

(defun value-of (exp env)
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
			(if (zero? num)
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

;;;exercise 3.3
;;因为减法用一个操作符就可以表示加减法，而加法需要两个。

;;exercise 3.4
;;TODO

;;exercise 3.5
;;TODO

;;exercise 3.6
;;见value-of和expression的定义中对minus的处理
;;解析器的调整 TODO

;;;exercise 3.7
;;见上方对add、mul和div的处理
;;解析器的调整 TODO

;;;exercise 3.8
;;见上方对equal、greater和less的处理
;;解析器的调整 TODO

;;;exercise 3.9
;;TODO

;;;exercise 3.10
;;TODO

;;;exercise 3.11
;;添加一个统一的 op-exp，TODO

;;;exercise 3.12
;;TODO

;;;exercise 3.13
;;把if表达式处理中 expval->bool 改为 (/= 0 (expval->num ...)) 实现：TODO

;;;exercise 3.14
;;TODO

;;;exercise 3.15
;;实现：TODO
;;不可表达的原因是这个操作符具有打印控制台的副作用，无法用函数形式表达。

;;;exercise 3.16
;;需要修改let的定义
;;实现：TODO

;;;exercise 3.17
;;TODO 展开成多级let

;;exercise 3.18
;;TODO 
