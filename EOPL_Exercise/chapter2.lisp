;;;exercise 2.1
(defvar bigit-base-n 16)
(defun bigit-zero ()
  '())
(defun bigit-is-zero? (n)
  (cond
   ((null n) t)
   ((zerop (car n)) (bigit-is-zero? (cdr n)))
   (t nil)))
(defun bigit-successor (n)
  (cond
   ((bigit-is-zero? n) (list 1))
   ((= (+ 1 (car n)) bigit-base-n)
    (cons 0 (bigit-successor (cdr n))))
   (t (cons (+ 1 (car n)) (cdr n)))))
(defun bigit-predecessor (n)
  (cond
   ((bigit-is-zero? n) (error "Zero is not support"))
   ((> (car n) 0) (cons (- (car n) 1) (cdr n)))
   (t (cons (- bigit-base-n 1) (bigit-predecessor (cdr n))))))

(defun bigit-number (num)
  (if (= 0 num)
      (bigit-zero)
    (bigit-successor (bigit-number (- num 1)))))
(defun bigit-plus (a b)
  (if (bigit-is-zero? a)
      b
    (bigit-plus (bigit-predecessor a) (bigit-successor b))))
(defun bigit-factorial (n)
  ;;TODO
  )

;;;exercise 2.2
;;TODO

;;;exercise 2.3
;;1. 对于任意的 n，使得n = n1 - n2，其中n1可以为任何数，因此n的表示是无穷的。
;;2.
(defun dt-number (n)
  (cond
   ((= n 1) '(one))
   ((< n 1) (list 'diff (dt-number (+ n 1)) '(one)))
   ((> n 1) (list 'diff (dt-number (- n 1)) '(diff (diff (one) (one)) (one))))))

(defun dt-value (n)
  (if (equal 'one (car n))
      1
    (- (dt-value (cadr n)) (dt-value (caddr n)))))
(defun dt-zero ()
  '(diff (one) (one)))
(defun dt-is-zero? (n)
  (= 0 (dt-value n)))
(defun dt-successor (n)
  (if (equal 'one (car n))
      '(diff (one) (diff (diff (one) (one)) (one)))
    (list 'diff (cadr n) (list 'diff (caddr n) (one)))))
(defun dt-predecessor (n)
  (if (equal 'one (car n))
      '(diff (one) (one))
    (list 'diff (list 'diff (cadr n) (one)) (caddr n))))
(defun diff-tree-plus (a b) ; (n1 - n2) + (n3 - n4) = (n1 - n2) - (n4 - n3)
  (list 'diff
	a
	(cond
	 ((equal 'one (car b)) '(diff (diff (one) (one)) (one)))
	 (t (list 'diff (caddr b) (cadr b))))))

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

;;;exercise 2.4
;;TODO

;;;exercise 2.5
(defun al-empty-env ()
  '())
(defun al-extend-env (var val env)
  (cons (cons var val) env))
(defun al-apply-env (env search-var)
  (cond
   ((null env) (report-no-binding-found search-var))
   (t (let ((saved-var (caar env))
	    (saved-val (cdar env))
	    (saved-env (cdr env)))
	(if (eql search-var saved-var)
	    saved-val
	  (al-apply-env saved-env search-var))))))

;;;exercise 2.6
;;1. hash表表示法：key为var，value为val
;;2. 2维动态数组表示法：a[i][0]为var，a[i][1]为val
;;3. 向量表示法：v[0]为var，v[1]为val，以此类推

;;;exercise 2.7
;;TODO

;;;exercise 2.8
(defun al-empty-env (env)
  (null env))

;;;exercise 2.9
(defun has-binding (env s)
  (cond
   ((null env) nil)
   (t (let ((saved-var (caar env))
	    (saved-val (cdar env))
	    (saved-env (cdr env)))
	(if (eql s saved-var)
	    t
	  (has-binding saved-env s))))))

;;;exercise 2.10
(defun extend-env* (var-list val-list env)
  (cond
   ((null var-list) env)
   (t (extend-env* (cdr var-list)
		   (cdr val-list)
		   (extend-env (car var-list) (car val-list) env)))))

;;;exercise 2.11
(defun r-empty-env ()
  '())
(defun r-extend-env (var val env)
  (cons (cons (cons var nil)
	      (cons val nil))
	env))
(defun r-find-var (vars vals search-var)
  (if (null vars)
      nil
    (if (eql (car vars) search-var)
	(cons (car vars) (car vals))
      (r-find-var (cdr vars) (cdr vals) search-var))))
(defun r-apply-env (env search-var)
  (cond
   ((null env) (report-no-binding-found search-var))
   (t (let ((saved-vars (caar env))
	    (saved-vals (cdar env))
	    (saved-env (cdr env)))
	(let ((r (r-find-var saved-vars saved-vals search-var)))
	  (if (null r)
	      (r-apply-env saved-env search-var)
	    (cdr r)))))))
(defun extend-env* (vars vals env)
  (cons (cons vars vals)
	env))

;;;exercise 2.12
(defun empty-stack ()
  (lambda (op)
    (case op
	  (is-empty t)
	  (otherwise (error "The stack is empty")))))
(defun stack-push (n stack)
  (lambda (op)
    (case op
	  (is-empty nil)
	  (pop stack)
	  (top n)
	  (otherwise (error "operation[~A] is not support" op)))))
(defun stack-pop (stack)
  (funcall stack 'pop))
(defun stack-top (stack)
  (funcall stack 'top))
(defun emtpy-stack? (stack)
  (funcall stack 'is-empty))
    
;;;exercise 2.13
(defun p-empty-env ()
  (list (lambda (search-var)
	  (report-no-binding-found search-var))
	(lambda () t)))
(defun p-extend-env (saved-var saved-val saved-env)
  (list (lambda (search-var)
	  (if (eql search-var saved-var)
	      saved-val
	    (p-apply-env saved-env search-var)))
	(lambda () nil)))
(defun p-apply-env (env search-var)
  (funcall (car env) search-var))

;;;exercise 2.14
(defun p-empty-env ()
  (list (lambda (search-var)
	  (report-no-binding-found search-var))
	(lambda () t)
	(lambda (search-var)
	  nil)))
(defun p-extend-env (saved-var saved-val saved-env)
  (list (lambda (search-var)
	  (if (eql search-var saved-var)
	      saved-val
	    (p-apply-env saved-env search-var)))
	(lambda () nil)
	(lambda (search-var)
	  (if (eql search-var saved-val)
	      t
	    (funcall (caddr saved-env) search-var)))))
(defun p-is-empty-env? (env)
  (funcall (cadr env)))
(defun p-has-binding? (env search-var)
  (funcall (caddr saved-env) search-var))


;;;;2.3
;;;exercise 2.15
(defun var-exp (var)
  var)
(defun lambda-exp (var lc-exp)
  `(lambda (,var) ,lc-exp))
(defun app-exp (lc-exp1 lc-exp2)
  (list lc-exp1 lc-exp2))
(defun var-exp? (lc-exp)
  (atom lc-exp))
(defun lambda-exp? (lc-exp)
  (eql 'lambda (car lc-exp)))
(defun app-exp? (lc-exp)
  (and
   (lc-exp? (car lc-exp))
   (lc-exp? (cadr lc-exp))))
(defun lc-exp? (exp)
  (or
   (var-exp? exp)
   (lambda-exp? exp)
   (app-exp? exp)))
(defun var-exp->var (exp)
  exp)
(defun lambda-exp->bound-var (lc-exp)
  (caadr lc-exp))
(defun lambda-exp->body (lc-exp)
  (cddr lc-exp))
(defun app-exp->rator (lc-exp)
  (car lc-exp))
(defun app-exp->rand (lc-exp)
  (cadr lc-exp))

;;;exercise 2.16
(defun lambda-exp-2 (var lc-exp)
  `(lambda ,var ,lc-exp))
(defun lambda-exp->bound-var (lc-exp)
  (cadr lc-exp))
;;其余不变

;;;exercise 2.17
;;1. (identifier -> lc-exp)
;;2. (identifier => lc-exp)
;;实现略 TODO

;;;exercise 2.18
(defun number->sequence (n)
  (list n '() '()))
(defun current-element (s)
  (car s))
(defun move-to-left (s)
  (if (not (null (cadr s)))
      (list (car (cadr s))
	    (cdr (cadr s))
	    (cons (car s) (caddr s)))))
(defun move-to-right (s)
  (if (not (null (caddr s)))
      (list (car (caddr s))
	    (cons (car s) (cadr s))
	    (cdr (caddr s)))))
(defun insert-to-left (n s)
  (list (car s)
	(cons n (cadr s))
	(caddr s)))
(defun insert-to-right (n s)
  (list (car s)
	(cadr s)
	(cons n (caddr s))))

;;;exercise 2.19
(defun number->bintree (n)
  (list n '() '()))
(defun current-element (tree)
  (car tree))
(defun insert-to-left (n tree)
  (list (current-element tree)
	(list n (cadr tree) nil)
	(caddr tree)))
(defun insert-to-right (n tree)
  (list (current-element tree)
	(cadr tree)
	(list n (caddr tree) nil)))
(defun move-to-left (tree)
  (cadr tree))
(defun move-to-right (tree)
  (caddr tree))
(defun at-leaf? (tree)
  (null tree))

;;;exercise 2.20
;;TODO

;;;;2.4
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
			       

;;;exercise 2.21
(define-datatype env env?
  (empty-env)
  (extend-env
   (var identifier?)
   (val identity)
   (env env?)))

;;;exercise 2.22
;;TODO

;;;exercise 2.23
(defun identifier? (e)
  (and
   (atom e)
   (not (eql e 'lambda))))

;;;exercise 2.24
(define-datatype bintree bintree?
  (leaf-node
    (num integer?))
  (interior-node
    (key symbol?)
    (left bintree?)
    (right bintree?)))
(defun bintree-to-list (t)
  (if (null t)
      nil
    (if (leaf-node? t)
	(list 'leaf-node (leaf-node->num t))
      (list 'interior-node
	    (interior-node->key t)
	    (interior-node->left t)
	    (interior-node->right t)))))

;;;exercise 2.25
(defun max-interior-1 (t)
  (cases bintree t
	 (leaf-node (num)
		    (cons nil num))
	 (interior-node (key left right)
			(let ((lr (max-interior-1 left))
			      (rr (max-interior-1 right)))
			  (let ((l (cdr lr))
				(r (cdr rr))
				(c (+ (cdr lr) (cdr rr))))
			    (cond
			     ((and (car lr) (>= l c)) lr)
			     ((and (car rr) (>= r c)) rr)
			     (t (cons key c))))))))
(defun max-interior (t)
  (car (max-interior-1 t)))

;;;exercise 2.26
(define-datatype red-blue-tree red-blue-tree?
  (red-blue-subtree))

(define-datatype red-blue-subtree red-blue-subtree?
  (red-node
   (left-node red-blue-subtree?)
   (right-node red-blue-subtree?))
  (blue-node
   (nodes))
  (leaf-node
   (value integer?)))

(defun proc1 (t num)
  (cases red-blue-sub t
	 (red-node (left-node right-node)
		   (red-node (proc1 left-node (+ num 1))
			     (proc1 right-node (+ num1))))
	 (blue-node (nodes)
		    (blue-node (mapcar (lambda (node)
					 (proc1 node num))
				       nodes)))
	 (leaf-node (leaf-node num))))
(defun proc (t)
  (proc1 t 0))

;;;;2.5
;;;exercise 2.27
;;TODO

;;;exercise 2.28
;;Lc-exp ::= Identifier
;;       ::= proc Identifier => Lc-exp
;;       ::= Lc-exp(Lc-exp)
(defun unparse-lc-exp-2 (exp)
  (cases lc-exp exp
	 (var-exp (var) var)
	 (lambda-exp (bound-var body)
		     (list proc bound-var '=> (unparse-lc-exp-2 body)))
	 (app-exp (rator rand)
		  (list (unparse-lc-exp-2 rator) (unparse-lc-exp-2 rand)))))

;;;exercise 2.29
(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-vars (list-of identifier?))
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rands (list-of lc-exp?))))

;;;exercise 2.30
(defun parse-expression (datum)
  (cond
   ((symbolp datum) (var-exp datum))
   ((consp datum)
    (if (eql (car datum) 'lambda)
	(if (lambda-exp? datum)
	    (lambda-exp
	     (car (cadr datum))
	     (parse-expression (caddr datum)))
	  (error "parse lambda-exp failed"))
      (if (app-exp? datum)
	  (app-exp
	   (parse-expression (car datum))
	   (parse-expression (cadr datum)))
	(error "parse app-exp failed"))))
   (t (error "invalid concrete syntax: ~A" datum))))

;;;exercise 2.31
(define-datatype prefix-exp prefix-exp?
  (const-exp
    (num integer?))
  (diff-exp
    (operand1 prefix-exp?)
    (operand2 prefix-exp?)))

(defun parse-prefix-exp-1 (datum)
  (if (eql (car datum) '-)
      (let ((temp (parse-prefix-exp-1 (cdr datum))))
	(let ((temp2 (parse-prefix-exp-1 (cdr temp))))
	  (cons (diff-exp (car temp) (car temp2)) (cdr temp2))))
    (cons (const-exp (car datum)) (cdr datum))))

(defun parse-prefix-exp (datum)
  (car (parse-prefix-exp-1 datum)))

      
			
	  
    
    
  
