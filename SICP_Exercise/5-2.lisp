(defun test-demo-1 ()
  (let ((gcd-machine (make-machine
		      '(a b t)
		      (list (list 'rem #'rem) (list '= #'=))
		      '(test-b
			(test (op =) (reg b) (const 0))
			(branch (label gcd-done))
			(assign t (op rem) (reg a) (reg b))
			(assign a (reg b))
			(assign b (reg t))
			(goto (label test-b))
			gcd-done))))
    (set-register-contents gcd-machine 'a 206)
    (set-register-contents gcd-machine 'b 40)
    (start gcd-machine)
    (get-register-contents gcd-machine 'a)))

(defun tagged-list? (exp tag)
  (if (consp exp)
      (eql (car exp) tag)
      nil))

(defun make-machine (register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (dolist (register-name register-names)
      (funcall (funcall machine 'allocate-register) register-name))
    (funcall (funcall machine 'install-operations) ops)
    (funcall (funcall machine 'install-instruction-sequence) (assemble controller-text machine))
    machine))

(defun make-register (name)
  (let ((contents '*unassigned*))
    (lambda (message)
      (case message
	    ('get contents)
	    ('set (lambda (v) (setf contents v)))
	    (otherwise (error "Unknown request -- REGISTER" message))))))

(defun get-contents (register)
  (funcall register 'get))

(defun set-contents (register value)
  (funcall (funcall register 'set) value))

(defun make-stack ()
  (let ((s '()))
    (labels ((s-push (x) (setf s (cons x s)))
	     (s-pop ()
		    (if (null s)
			(error "Empty stack -- POP")
		      (let ((top (car s)))
			(setf s (cdr s))
			top)))
	     (initialize ()
			 (setf s '())
			 'done))
	    (lambda (message)
	      (case message
		    ('push #'s-push)
		    ('pop (s-pop))
		    ('initialize (initialize)))))))

(defun s-pop (stack)
  (funcall stack 'pop))

(defun s-push (stack value)
  (funcall (funcall stack 'push) value))

(defun make-new-machine ()
  (let ((pc (make-register 'pc))
	(flag (make-register 'flag))
	(stack (make-stack))
	(the-instruction-sequence '()))
    (let ((the-ops
	   (list (list 'initialize-stack (lambda () (funcall stack 'initialize)))))
	  (register-table
	   (list (list 'pc pc) (list 'flag flag))))
      (labels ((allocate-register (name)
				  (if (assoc name register-table)
				      (error "Multiply defined register:" name)
				    (setf register-table (cons (list name (make-register name)) register-table)))
				  'register-allocated)
	       (lookup-register (name)
				(let ((val (assoc name register-table)))
				  (if val
				      (cadr val)
				    (error "Unknown register:" name))))
	       (execute ()
			(let ((insts (get-contents pc)))
			  (if (null insts)
			      'done
			    (progn
			      (funcall (instruction-execution-proc (car insts)))
			      (execute))))))
	      (lambda (message)
		(case message
		      ('start (set-contents pc the-instruction-sequence)
			      (execute))
		      ('install-instruction-sequence (lambda (seq) (setf the-instruction-sequence seq)))
		      ('allocate-register #'allocate-register)
		      ('get-register #'lookup-register)
		      ('install-operations (lambda (ops) (setf the-ops (append the-ops ops))))
		      ('stack stack)
		      ('operations the-ops)
		      (otherwise (error "Unknown request -- MACHINE" message))))))))
      
(defun start (machine)
  (funcall machine 'start))

(defun get-register-contents (machine register-name)
  (get-contents (get-register machine register-name)))

(defun set-register-contents (machine register-name value)
  (set-contents (get-register machine register-name) value)
  'done)

(defun get-register (machine reg-name)
  (funcall (funcall machine 'get-register) reg-name))

(defun assemble (controller-text machine)
  (extract-labels controller-text
		  (lambda (insts labels)
		    (update-insts insts labels machine)
		    insts)))

(defun extract-labels (text receive)
  (if (null text)
      (funcall receive '() '())
    (extract-labels (cdr text)
		    (lambda (insts labels)
		      (let ((next-inst (car text)))
			(if (symbolp next-inst)
			    (if (find-if (lambda (l) (equal l next-inst)) labels :key #'car)
				(error "label is existed")
			    (funcall receive insts (cons (make-label-entry next-inst insts) labels)))
			  (funcall receive (cons (make-instruction next-inst)
					 insts)
				   labels)))))))

(defun update-insts (insts labels machine)
  (let ((pc (get-register machine 'pc))
	(flag (get-register machine 'flag))
	(stack (funcall machine 'stack))
	(ops (funcall machine 'operations)))
    (dolist (inst insts)
      (set-instruction-execution-proc inst
				      (make-execution-procedure (instruction-text inst) labels machine pc flag stack ops)))))

(defun make-instruction (text)
  (cons text '()))

(defun instruction-text (inst)
  (car inst))

(defun instruction-execution-proc (inst)
  (cdr inst))

(defun set-instruction-execution-proc (inst proc)
  (setf (cdr inst) proc))

(defun make-label-entry (label-name insts)
  (cons label-name insts))

(defun lookup-label (label-table label-name)
  (let ((val (assoc label-name label-table)))
    (if val
	(cdr val)
      (error "Undefined label label -- ASSEMBLE" label-name))))

(defun make-execution-procedure (inst labels machine pc flag stack ops)
  (case (car inst)
	('assign (make-assign inst machine labels ops pc))
	('test (make-test inst machine labels ops flag pc))
	('branch (make-branch inst machine labels flag pc))
	('goto (make-goto inst machine labels pc))
	('save (make-save inst machine stack pc))
	('restore (make-restore inst machine stack pc))
	('perform (make-perform inst machine labels ops pc))
	(otherwise (error "Unknown instruction type -- ASSEMBLE" inst))))

(defun make-assign (inst machine labels operations pc)
  (let ((target (get-register machine (assign-reg-name inst)))
	(value-exp (assign-value-exp inst)))
    (let ((value-proc (if (operation-exp? value-exp)
			  (make-operation-exp value-exp machine labels operations)
			(make-primitive-exp (car value-exp) machine labels))))
      (lambda ()
	(set-contents target (funcall value-proc))
	(advance-pc pc)))))

(defun assign-reg-name (assign-instruction)
  (cadr assign-instruction))

(defun assign-value-exp (assign-instruction)
  (cddr assign-instruction))

(defun advance-pc (pc)
  (set-contents pc (cdr (get-contents pc))))

(defun make-test (inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
	(let ((condition-proc (make-operation-exp condition machine labels operations)))
	  (lambda ()
	    (set-contents flag (funcall condition-proc))
	    (advance-pc pc)))
      (error "Bad TEST instruction -- ASSEMBLE" inst))))

(defun test-condition (test-instruction)
  (cdr test-instruction))

(defun make-branch (inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
	(let ((insts (lookup-label labels (label-exp-label dest))))
	  (lambda ()
	    (if (get-contents flag)
		(set-contents pc insts)
	      (advance-pc pc))))
      (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(defun branch-dest (branch-instruction)
  (cadr branch-instruction))

(defun make-goto (inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
	   (let ((insts (lookup-label labels (label-exp-label dest))))
	     (lambda ()
	       (set-contents pc insts))))
	  ((register-exp? dest)
	   (let ((reg (get-register machine (register-exp-reg dest))))
	     (lambda ()
	       (set-contents pc (get-contents reg)))))
	  (otherwise (error "Bad GOTO instruction -- ASSEMBLE" inst)))))

(defun goto-dest (goto-instruction)
  (cadr goto-instruction))

(defun make-save (inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (s-push stack (get-contents reg))
      (advance-pc pc))))

(defun make-restore (inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents reg (s-pop stack))
      (advance-pc pc))))

(defun stack-inst-reg-name (stack-instruction)
  (cadr stack-instruction))

(defun make-perform (inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
	(let ((action-proc (make-operation-exp action machine labels operations)))
	  (lambda ()
	    (funcall action-proc)
	    (advance-pc pc)))
      (error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(defun perform-action (inst)
  (cdr inst))

(defun make-primitive-exp (exp machine labels)
  (cond ((constant-exp? exp)
	 (let ((c (constant-exp-value exp)))
	   (lambda () c)))
	((label-exp? exp)
	 (let ((insts (lookup-label labels (label-exp-label exp))))
	   (lambda () insts)))
	((register-exp? exp)
	 (let ((r (get-register machine (register-exp-reg exp))))
	   (lambda () (get-contents r))))
	(otherwise (error "Unknown expression type -- ASSEMBLE" exp))))

(defun register-exp? (exp) (tagged-list? exp 'reg))
(defun register-exp-reg (exp) (cadr exp))
(defun constant-exp? (exp) (tagged-list? exp 'const))
(defun constant-exp-value (exp) (cadr exp))
(defun label-exp? (exp) (tagged-list? exp 'label))
(defun label-exp-label (exp) (cadr exp))

(defun make-operation-exp (exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
	(aprocs (mapcar (lambda (e) (make-primitive-exp e machine labels))
			(operation-exp-operands exp))))
    (lambda ()
      (apply op (mapcar (lambda (p) (funcall p)) aprocs)))))

(defun operation-exp? (exp)
  (and (consp exp) (tagged-list? (car exp) 'op)))

(defun operation-exp-op (operation-exp)
  (cadr (car operation-exp)))

(defun operation-exp-operands (operation-exp)
  (cdr operation-exp))

(defun lookup-prim (symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
	(cadr val)
      (error "Unknown operation -- ASSEMBLE" symbol))))



	
