;;;;4.1.1
(defun list-of-values (exps env)
  (if (no-operands? exps)
      '()
      (cons (l-eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))

(defun eval-if (exp env)
  (if (true? (l-eval (if-predicate exp) env))
      (l-eval (if-consequent exp) env)
      (l-eval (if-alternative exp) env)))

(defun eval-sequence (exps env)
  (cond ((last-exp? exps) (l-eval (first-exp exps) env))
	(t (l-eval (first-exp exps) env)
	   (eval-sequence (rest-exps exps) env))))

(defun eval-assignment (exp env)
  (set-variable-value! (assignment-variable exp)
		       (l-eval (assignment-value exp) env)
		       env)
  'ok)

(defun eval-definition (exp env)
  (define-variable! (definition-variable exp)
      (l-eval (definition-value exp) env)
    env)
  'ok)

(defun l-eval (exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp)
	 (make-procedure (lambda-parameters exp)
			 (lambda-body exp)
			 env))
	((begin? exp)
	 (eval-sequence (begin-actions exp) env))
	((cond? exp) (l-eval (cond->if exp) env))
	((application? exp)
	 (l-apply (l-eval (operator exp) env)
		  (list-of-values (operands exp) env)))
	(t (error "Unknown expression type -- EVAL" exp))))

(defun l-apply (procedure arguments)
  (format t "l-apply:~A~%" procedure)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure procedure arguments))
	((compound-procedure? procedure)
	 (eval-sequence
	  (procedure-body procedure)
	  (extend-environment
	   (procedure-parameters procedure)
	   arguments
	   (procedure-environment procedure))))
	(t (error "Unknown procedure type -- APPLY" procedure))))

;;;exercise 4.1
(defun l->r-list-of-values (exps env)
  (if (no-operands? exps)
      '()
      (let ((left (l-eval (first-operand exps) env)))
	(let ((right (list-of-values (rest-operands exps) env)))
	  (cons left right)))))

(defun r->l-list-of-values (exps env)
  (if (no-operands? exps)
      '()
      (let ((rigth (list-of-values (rest-operands exps) env)))
	(let (left (l-eval (first-operand exps) env))
	  (cons left right)))))

(defun self-evaluating? (exp)
  (cond ((numberp exp) t)
	((stringp exp) t)
	(t nil)))

(defun variable? (exp)
  (symbolp exp))

(defun tagged-list? (exp tag)
  (if (consp exp)
      (eql (car exp) tag)
      nil))

(defun quoted? (exp)
  (tagged-list? exp 'quote))

(defun text-of-quotation (exp)
  (cadr exp))

(defun assignment? (exp)
  (tagged-list? exp 'set!))

(defun assignment-variable (exp)
  (cadr exp))

(defun assignment-value (exp)
  (caddr exp))

(defun definition? (exp)
  (tagged-list? exp 'define))

(defun definition-variable (exp)
  (if (symbolp (cadr exp))
      (cadr exp)
      (caadr exp)))

(defun definition-value (exp)
  (if (symbolp (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
		   (cddr exp))))

(defun lambda? (exp)
  (tagged-list? exp 'lambda))

(defun lambda-parameters (exp)
  (cadr exp))

(defun lambda-body (exp)
  (cddr exp))

(defun make-lambda (parameters body)
  (cons 'lambda (cons parameters body)))

(defun if? (exp)
  (tagged-list? exp 'if))

(defun if-predicate (exp)
  (cadr exp))

(defun if-consequent (exp)
  (caddr exp))

(defun if-alternative (exp)
  (if (not (null (cdddr exp)))
      (cadddr exp)
      nil))

(defun make-if (predicate consequent alternative)
  (list 'if predicate consequent alternative))

(defun begin? (exp)
  (tagged-list? exp 'begin))

(defun begin-actions (exp)
  (cdr exp))

(defun last-exp? (seq)
  (null (cdr seq)))

(defun first-exp (seq)
  (car seq))

(defun rest-exps (seq)
  (cdr seq))

(defun sequence-exp (seq)
  (cond ((null seq) seq)
	((last-exp? seq) (first-exp seq))
	(t (make-begin seq))))

(defun make-begin (seq)
  (cons 'begin seq))

(defun application? (exp)
  (consp exp))

(defun operator (exp)
  (car exp))

(defun operands (exp)
  (cdr exp))

(defun no-operands? (ops)
  (null ops))

(defun first-operand (ops)
  (car ops))

(defun rest-operands (ops)
  (cdr ops))

(defun cond? (exp)
  (tagged-list? exp 'cond))

(defun cond-clauses (exp)
  (cdr exp))

(defun cond-else-clause? (clause)
  (eql (cond-predicate clause) 'else))

(defun cond-predicate (clause)
  (car clause))

(defun cond-actions (clause)
  (cdr clause))

(defun cond->if (exp)
  (expand-clauses (cond-clauses exp)))

(defun expand-clauses (clauses)
  (if (null clauses)
      'nil
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null rest)
		(sequence-exp (cond-actions first))
		(error "ELSE clause isn't last -- COND->IF" clauses))
	    (make-if (cond-predicate first)
		     (sequence-exp (cond-actions first))
		     (expand-clauses rest))))))

;;;exercise 4.2
;;后续的if cond 之类的表达式都被当成过程应用处理了。
(defun application?-2 (exp)
  (eql (car exp) 'call))

;;;exercise 4.3
(defun d-eval (exp env)
  (let ((eval-proc (get 'eval (expression-type exp))))
    (if (null eval-proc)
	(error "Unknown expression type -- EVAL" exp)
	(funcall eval-proc exp env))))

(defun expression-type (exp)
  (cond ((self-evaluating? exp) 'self-evaluating)
	((variable? exp) 'variable)
	((quoted? exp) 'quote)
	((assignment? exp) 'assignment)
	((definition? exp) 'definition)
	((if? exp) 'if)
	((lambda? exp) 'lambda)
	((begin? exp) 'begin)
	((cond? exp) 'cond)
	((let? exp) 'let)
	((application? exp) 'application)
	(t (error "Unknown expression type -- EXPRESSION-TYPE" exp))))

(defun install-eval ()
  (setf (get 'eval 'self-evaluating) (lambda (exp env) exp))
  (setf (get 'eval 'variable) #'lookup-variable-value)
  (setf (get 'eval 'quote)
	(lambda (exp env) (text-of-quotation exp)))
  (setf (get 'eval 'assignment) (lambda (exp env)
				  (set-variable-value! (assignment-variable exp)
						       (d-eval (assignment-value exp) env)
						       env)
				  'ok))
  (setf (get 'eval 'definition) (lambda (exp env)
				  (define-variable! (definition-variable exp)
				      (d-eval (definition-value exp) env)
				    env)
				  'ok))
  (setf (get 'eval 'if) (lambda (exp env)
			  (if (true? (d-eval (if-predicate exp) env))
			      (d-eval (if-consequent exp) env)
			      (d-eval (if-alternative exp) env))))
  (setf (get 'eval 'lambda)
	(lambda (exp env)
	  (make-procedure (lambda-parameters exp)
			 (lambda-body exp)
			 env)))
  (setf (get 'eval 'begin)
	(lambda (exp env)
	  (eval-sequence (begin-actions exp) env)))
  (setf (get 'eval 'cond)
	(lambda (exp env)
	  (d-eval (cond->if exp) env)))
  (setf (get 'eval 'application)
	(lambda (exp env)
	  (l-apply (d-eval (operator exp) env)
		   (list-of-values (operands exp) env)))))
;(install-eval)

;;;exercise 4.4
(defun eval-and (exps env)
  (let ((r (d-eval (car exps) env)))
    (cond ((null r) nil)
	  ((null (cdr exps)) r)
	  (t (eval-and (cdr exps) env)))))
	
(setf (get 'eval '(and)) (lambda (exp env) (eval-and (cdr exp) env)))

(defun eval-or (exps env)
  (let ((r (d-eval (car exps) env)))
    (cond ((not (null r)) r)
	  ((not (null (cdr exps))) (eval-or (cdr exps) env))
	  (t nil))))

(setf (get 'eval '(or)) (lambda (exp env) (eval-or (cdr exp) env)))

;;TODO and or 实现为派生表达式

;;;exercise 4.5
(defun test-recipient? (exp)
  (and (= 3 (length exp)) (eql '=> (cadr exp))))

(defun expand-clauses (clauses)
  (if (null clauses)
      'nil
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null rest)
		(sequence-exp (cond-actions first))
		(error "ELSE clause isn't last -- COND->IF" clauses))
	    (if (test-recipient? first)
		(let ((value (car first))
		      (proc (caddr first)))
		  (make-if value (list 'd-eval (list proc value)) (expand-clauses rest))) ;这里的value会求值2次，会有问题
		(make-if (cond-predicate first)
			 (sequence-exp (cond-actions first))
			 (expand-clauses rest)))))))

;;;exercise 4.6
(defun let? (exp)
  (tagged-list? exp 'let))

(defun let-variables (exp)
  (let ((vars '()))
    (dolist (item (cadr exp))
      (push (car item) vars))
    (reverse vars)))

(defun let-exps (exp)
  (let ((exps '()))
    (dolist (item (cadr exp))
      (push (cadr item) exps))
    (reverse exps)))

(defun let-body (exp)
  (caddr exp))

(defun let->combination (exp)
  (cons (list 'lambda (let-variables exp) (let-body exp))
	(let-exps exp)))

(setf (get 'eval '(let)) (lambda (exp env) (d-eval (let->combination exp) env)))

;;;exercise 4.7
(defun let*->nested-lets (exp)
  (labels ((iter (vars)
	     (list 'let (cons (car vars) nil)
		   (if (null (cdr cars))
		       (let-body exp)
		       (iter (cdr vars))))))
    (iter (cadr exp))))

(defun let*? (exp)
  (tagged-list? exp '(let*)))

(setf (get 'eval '(let*)) (lambda (exp env) (d-eval (let*->nested-lets exp) env)))
;;不必以非派生方式来扩充

;;;exercise 4.8
;;TODO

;;;exercise 4.9
;;TODO do for while until


;;;exercise 4.10
;;修改选择函数和构造函数即可 TODO 不是很理解？

;;;; 4.1.3
(defun true? (x)
  (not (eql x nil)))

(defun false? (x)
  (eql x nil))

(defun make-procedure (parameters body env)
  (list 'procedure parameters body env))

(defun compound-procedure? (p)
  (tagged-list? p 'procedure))

(defun procedure-parameters (p)
  (cadr p))

(defun procedure-body (p)
  (caddr p))

(defun procedure-environment (p)
  (cadddr p))

(defun enclosing-environment (env)
  (cdr env))

(defun first-frame (env) (car env))

(defvar the-empty-environment '())

(defun make-frame (variables values)
  (cons variables values))

(defun frame-variables (frame) (car frame))

(defun frame-values (frame) (cdr frame))

(defun add-binding-to-frame! (var val frame)
  (setf (car frame) (cons var (car frame)))
  (setf (cdr frame) (cons val (cdr frame))))

(defun extend-environment (vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
	  (error "Too many arguments supplied" vars vals)
	  (error "Too few arguments supplied" vars vals))))

(defun lookup-variable-value (var env)
  (labels ((env-loop (env)
	     (format t "env-loop:~A - ~A~%" var env)
	     (labels ((scan (vars vals)
			(cond ((null vars) (env-loop (enclosing-environment env)))
			      ((eql var (car vars)) (car vals))
			      (t (scan (cdr vars) (cdr vals))))))
	       (if (eql env the-empty-environment)
		   (error "Unbound variable" var)
		   (let ((frame (first-frame env)))
		     (scan (frame-variables frame)
			   (frame-values frame)))))))
    (env-loop env)))

(defun set-variable-value! (var val env)
  (labels ((env-loop (env)
	     (labels ((scan (vars vals)
			(cond ((null vars) (env-loop (enclosing-environment env)))
			      ((eql var (car vars)) (setf (car vals) val))
			      (t (scan (cdr vars) (cdr vals))))))
	       (if (eql env the-empty-environment)
		   (error "Unbound variable" var)
		   (let ((frame (first-frame env)))
		     (scan (frame-variables frame)
			   (frame-values frame)))))
	     (env-loop env)))))

(defun define-variable! (var val env)
  (let ((frame (first-frame env)))
    (labels ((scan (vars vals)
	       (cond ((null vars) (add-binding-to-frame! var val frame))
		     ((eql var (car vars)) (setf (car vals) val))
		     (t (scan (cdr vars) (cdr vals))))))
      (scan (frame-variables frame)
	    (frame-values frame)))))

;;;exercise 4.11
(defun l-make-frame (variables values)
  (if (or (null variables) (null values))
      nil
      (cons (cons (car variables) (car values))
	    (make-frame (cdr variables) (cdr values)))))

(defun l-frame-variables (frame)
  (if (null frame)
      nil
      (cons (caar frame) (frame-variables (cdr frame)))))

(defun l-frame-values (frame)
  (if (null frame)
      nil
      (cons (cdar frame) (frame-values (cdr frame)))))

(defun l-add-binding-to-frame! (var val frame)
  (setf (cdr frame) frame)
  (setf (car frame) (cons var val)))

;;;exercise 4.12
(defun scan-map (vars vals found-action no-action)
  (cond ((null vars) (funcall no-action))
	((eql var (car vars)) (funcall found-action vars vals))
	(t (scan-map (cdr vars) (cdr vals)))))

;;;exercise 4.13 TODO

;;;;4.1.4
(setf primitive-procedures (list (list 'car #'car)
				 (list 'cdr #'cdr)
				 (list 'cons #'cons)
				 (list 'null #'null)
				 ))

(defun primitive-procedure-names ()
  (mapcar #'car primitive-procedures))

(defun primitive-procedure-objects ()
  (mapcar (lambda (proc) (list 'primitive (cadr proc)))
	  primitive-procedures))

(defun setup-environment ()
  (let ((initial-env (extend-environment (primitive-procedure-names)
					 (primitive-procedure-objects)
					 the-empty-environment)))
    (define-variable! 'true t initial-env)
    (define-variable! 'false nil initial-env)
    initial-env))

(setf the-global-environment (setup-environment))

(defun primitive-procedure? (proc)
  (tagged-list? proc 'primitive))

(defun primitive-implementation (proc)
  (cadr proc))

(setf (symbol-function 'apply-in-underlying-scheme) #'apply)

(defun apply-primitive-procedure (proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(setf input-prompt ";;; M-Eval input:")
(setf output-prompt ";;; M-Eval value:")

(defun driver-loop ()
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (d-eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(defun prompt-for-input (string)
  (format t "~%~%~A~%" string))

(defun announce-output (string)
  (format t "~%~A~%" string))
       
(defun user-print (object)
  (if (compound-procedure? object)
      (format t "~A" (list 'compound-procedure
			 (procedure-parameters object)
			 (procedure-body object)
			 ))
      (format t "~A" object)))

;;;exercise 4.14
;;运行环境不同
