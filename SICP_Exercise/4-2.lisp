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
  ;(format t "l-apply:~A~%" procedure)
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
	     ;(format t "env-loop:~A - ~A~%" var env)
	     (labels ((scan (vars vals)
			(cond ((null vars) (env-loop (enclosing-environment env)))
			      ((eql var (car vars)) (car vals))
			      (t (scan (cdr vars) (cdr vals))))))
	       (if (eql env the-empty-environment)
		   (error "Unbound variable:~A" var)
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


(defun scan-map (vars vals found-action no-action)
  (cond ((null vars) (funcall no-action))
	((eql var (car vars)) (funcall found-action vars vals))
	(t (scan-map (cdr vars) (cdr vals)))))

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
    (let ((output (l-eval input the-global-environment)))
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

;;;;4-2
;;;4.2.2
(defun l-eval (exp env)
  (format t "l-eval: ~A <=> ~%" exp)
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
	 (l-apply (actual-value (operator exp) env)
		  (operands exp)
		  env))
	(t (error "Unknown expression type -- EVAL" exp))))

(defun actual-value (exp env)
  (format t "actual-value:~A <=>~%" exp)
  (force-it (l-eval exp env)))

(defun l-apply (procedure arguments env)
  ;(format t "l-apply:~A~%" procedure)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure procedure
				    (list-of-arg-values arguments env)))
	((compound-procedure? procedure)
	 (eval-sequence
	  (procedure-body procedure)
	  (extend-environment
	   (procedure-parameters procedure)
	   (list-of-delayed-args arguments env)
	   (procedure-environment procedure))))
	(t (error "Unknown procedure type -- APPLY" procedure))))

(defun list-of-arg-values (exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
	    (list-of-arg-values (rest-operands exp) env))))

(defun list-of-delayed-args (exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
	    (list-of-delayed-args (rest-operands exps) env))))

(defun eval-if (exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (l-eval (if-consequent exp) env)
      (l-eval (if-alternative exp) env)))

(setf input-prompt ";;; L-Eval input:")
(setf output-prompt ";;; L-Eval output:")

(defun driver-loop ()
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(defun force-it (obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

(defun delay-it (exp env)
  (list 'thunk exp env))

(defun thunk? (obj)
  (tagged-list? obj 'thunk))

(defun thunk-exp (thunk)
  (cadr thunk))

(defun thunk-env (thunk)
  (caddr thunk))

(defun evaluated-thunk? (obj)
  (tagged-list? obj 'evaluated-thunk))

(defun thunk-value (evaluated-thunk)
  (cadr evaluated-thunk))

(defun force-it (obj)
  (cond ((thunk? obj)
	 (let ((result (actual-value (thunk-exp obj) (thunk-env boj))))
	   (setf (car obj) 'evaluated-thunk)
	   (setf (car (cdr obj)) result)
	   (setf (cdr (cdr obj)) '())
	   result))
	((evaluated-thunk? obj)
	 (thunk-value obj))
	(t obj)))

;;;4.27
;;0
;;10
;;2

;;;4.28
;;TODO 未知

;;;4.29
;;略
;;100
;;count => 1或2


;;;4.30 TODO

;;;4.31 TODO important

;;;;4.2.3 TODO 待定

	       
