(defvar all-regs '(exp env val continue proc argl unev))
(defvar label-count 0)
(defun make-label (symbol)
  (intern (string-concat (symbol-name symbol) (format nil "~A" label-count))))

(defun l-compile (exp target linkage)
  (cond ((self-evaluating? exp)
	 (compile-self-evaluating exp target linkage))
	((quoted? exp) (compile-quoted exp target linkage))
	((variable? exp)
	 (compile-variable exp target linkage))
	((assignment? exp)
	 (compile-assignment exp target linkage))
	((definition? exp)
	 (compile-definition exp target linkage))
	((if? exp) (compile-if exp target linkage))
	((lambda? exp) (compile-lambda exp target linkage))
	((begin? exp)
	 (compile-sequence (begin-actions exp)
			   target
			   linkage))
	((cond? exp) (l-compile (cond->if exp) target linkage))
	((application? exp)
	 (compile-application exp target linkage))
	(t (error "Unknown expression type -- COMPILE" exp))))

(defun make-instruction-sequence (needs modifies statements)
  (list needs modifies statements))

(defun empty-instruction-sequence ()
  (make-instruction-sequence '() '() '()))

(defun compile-linkage (linkage)
  (case linkage
	('return (make-instruction-sequence '(continue) '() '((goto (reg continue)))))
	('next (empty-instruction-sequence))
	(otherwise (make-instruction-sequence '() '() `((goto (label ,linkage)))))))

(defun end-with-linkage (linkage instruction-sequence)
  (preserving '(continue)
	      instruction-sequence
	      (compile-linkage linkage)))

(defun compile-self-evaluating (exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence '() (list target)
			      `((assign ,target (const ,exp))))))

(defun compile-quoted (exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence '() (list target)
			      `((assign ,target (const ,(text-of-quotation exp)))))))

(defun compile-variable (exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence '(env) (list target)
			      `((assign ,target
					(op lookup-variable-value)
					(const ,exp)
					(reg env))))))

(defun compile-assignment (exp target linkage)
  (let ((var (assignment-variable exp))
	(get-value-code
	 (l-compile (assignment-value exp) 'val 'next)))
    (end-with-linkage
     linkage
     (preserving '(env)
		 get-value-code
		 (make-instruction-sequence
		  '(env val)
		  (list target)
		  `((perform (op set-variable-value)
			     (const ,var)
			     (reg val)
			     (reg env))
		    (assign ,target (const ok))))))))

(defun compile-definition (exp target linkage)
  (let ((var (definition-variable exp))
	(get-value-code (l-compile (definition-value exp) 'val 'next)))
    (end-with-linkage
     linkage
     (preserving
      '(env)
      get-value-code
      (make-instruction-sequence
       '(env val)
       (list target)
       `((perform (op define-variable)
		  (const ,var)
		  (reg val)
		  (reg env))
	 (assign ,target (const ok))))))))

(defun compile-if (exp target linkage)
  (let ((t-branch (make-label 'true-branch))
	(f-branch (make-label 'false-brance))
	(after-if (make-label 'after-if)))
    (let ((consequent-linkage
	   (if (eq linkage 'next) after-if linkage)))
      (let ((p-code (l-compile (if-predicate exp) 'val 'next))
	    (c-code (l-compile (if-consequent exp) target consequent-linkage))
	    (a-code (l-compile (if-alternative exp) target linkage)))
	(preserving
	 '(env continue)
	 p-code
	 (append-instruction-sequences
	  (make-instruction-sequence
	   '(val)
	   '()
	   `((test (op false?) (reg val))
	     (brance (label ,f-branch))))
	  (parallel-instruction-sequences
	   (append-instruction-sequences t-branch c-code)
	   (append-instruction-sequences f-branch a-code))
	  after-if))))))

(defun compile-sequence (seq target linkage)
  (if (last-exp? seq)
      (l-compile (first-exp seq) target linkage)
    (preserving '(env continue)
		(l-compile (first-exp seq) target 'next)
		(compile-sequence (rest-exps seq) target linkage))))

(defun compile-lambda (exp target linkage)
  (let ((proc-entry (make-label 'entry))
	(after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
	   (if (eq linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
	(end-with-linkage
	 lambda-linkage
	 (make-instruction-sequence
	  '(env)
	  (list target)
	  `((assign ,target
		    (op make-compiled-procedure)
		    (label ,proc-entry)
		    (reg env)))))
	(compile-lambda-body exp proc-entry))
       after-lambda))))

(defun compile-lambda-body (exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence
      '(env proc argl)
      '(env)
      `(,proc-entry
	(assign env (op compiled-procedure-env) (env proc))
	(assign env
		(op extend-environment)
		(const ,formals)
		(reg argl)
		(reg env))))
     (compile-sequence (lambda-body exp) 'val 'return))))

(defun compile-application (exp target linkage)
  (let ((proc-code (l-compile (operator exp) 'proc 'next))
	(operand-codes
	 (mapcar (lambda (operand) (l-compile operand 'val 'next))
		 (operands exp))))
    (preserving
     '(env continue)
     proc-code
     (preserving
      '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call target linkage)))))

(defun construct-arglist (operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null operand-codes)
	(make-instruction-sequence
	 '()
	 '(argl)
	 `((assign argl (const ()))))
      (let ((code-to-get-last-arg
	     (append-instruction-sequences
	      (car operand-codes)
	      (make-instruction-sequence
	       '(val)
	       '(argl)
	       '((assign argl (op list) (reg val)))))))
	(if (null (cdr operand-codes))
	    code-to-get-last-arg
	  (preserving
	   '(env)
	   code-to-get-last-arg
	   (code-to-get-rest-args (cdr operand-codes))))))))

(defun code-to-get-rest-args (operand-codes)
  (let ((code-for-next-arg
	 (preserving
	  '(argl)
	  (car operand-codes)
	  (make-instruction-sequence
	   '(val argl)
	   '(argl)
	   `((assign argl (op cons) (reg val) (reg argl)))))))
    (if (null (cdr operand-codes))
	code-for-next-arg
      (preserving
       '(env)
       code-for-next-arg
       (code-to-get-rest-args (cdr operand-codes))))))

(defun compile-procedure-call (target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
	(compiled-branch (make-label 'compiled-branch))
	(after-call (make-label 'after-call)))
    (let ((compiled-linkage
	   (if (eq linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence
	'(proc)
	'()
	`((test (op primitive-procedure) (reg proc))
	  (branch (label ,primitive-branch))))
       (parallel-instruction-sequences
	(append-instruction-sequences
	 compiled-branch
	 (compile-proc-appl target compiled-linkage))
	(append-instruction-sequences
	 primitive-branch
	 (end-with-linkage
	  linkage
	  (make-instruction-sequence
	   '(proc argl)
	   (list target)
	   `((assign ,target
		     (op apply-primitive-procedure)
		     (reg proc)
		     (reg argl)))))))
       after-call))))

(defun compile-proc-appl (target linkage)
  (cond ((and (eq target 'val) (not (eq linkage 'return)))
	 (make-instruction-sequence
	  '(proc)
	  all-regs
	  `((assign continue (label ,linkage))
	    (assign val (op compiled-procedure-entry)
		    (reg proc))
	    (goto (reg val)))))
	((and (not (eq target 'val))
	      (not (eq linkage 'return)))
	 (let ((proc-return (make-label 'proc-return)))
	   (make-instruction-sequence
	    '(proc)
	    all-regs
	    `((assign continue (label ,proc-return))
	      (assign val (op compiled-procedure-entry)
		      (reg proc))
	      (goto (reg val))
	      ,proc-return
	      (assign ,target (reg val))
	      (goto (label ,linkage))))))
	((and (eq target 'val) (eq linkage 'return))
	 (make-instruction-sequence
	  '(proc continue)
	  all-regs
	  '((assign val
		    (op compiled-procedure-entry)
		    (reg proc))
	    (goto (reg val)))))
	((and (not (eq tatget val)) (eq linkage 'return))
	 (error "return linkage, target not val -- COMPILE" target))))

(defun statements (s)
  (if (symbolp s) '() (caddr s)))

(defun registers-needed (s)
  (if (symbolp s) '() (car s)))

(defun registers-modified (s)
  (if (symbolp s) '() (cadr s)))

(defun needs-register? (seq reg)
  (member reg (registers-needed seq)))

(defun modifies-register? (seq reg)
  (member reg (registers-modified seq)))

(defun append-instruction-sequences (&rest seqs)
  (labels
   ((append-2-sequences (seq1 seq2)
			       (make-instruction-sequence
				(list-union (registers-needed seq1)
					    (list-difference (registers-needed seq2)
							     (registers-modified seq1)))
				(list-union (registers-modified seq1)
					    (registers-modified seq2))
				(append (statements seq1) (statements seq2)))))
   (labels ((append-seq-list (seqs)
			     (if (null seqs)
				 (empty-instruction-sequence)
			       (append-2-sequences (car seqs)
						   (append-seq-list (cdr seqs))))))
	   (append-seq-list seqs))))

(defun list-union (s1 s2)
  (cond ((null s1) s2)
	((member (car s1) s2) (list-union (cdr s1) s2))
	(t (cons (car s1) (list-union (cdr s1) s2)))))

(defun list-difference (s1 s2)
  (cond ((null s1) '())
	((member (car s1) s2) (list-difference (cdr s1) s2))
	(t (cons (car s1) (list-difference (cdr s1) s2)))))

(defun preserving (regs seq1 seq2)
  (if (null regs)
      (append-instruction-sequences seq1 seq2)
    (let ((first-reg (car regs)))
      (if (and (needs-register? seq2 first-reg)
	       (modifies-register? seq1 first-reg))
	  (preserving
	   (cdr regs)
	   (make-instruction-sequence
	    (list-union (list first-reg)
			(registers-needed seq1))
	    (list-difference (registers-modified seq1)
			     (list first-reg))
	    (append `((save ,first-reg))
		    (statements seq1)
		    `((resotore ,first-reg))))
	   seq2)
	(preserving (cdr regs) seq1 seq2)))))

(defun tack-on-instruction-sequence (seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq) (statements body-seq))))

(defun parallel-instruction-sequences (seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
	       (registers-needed seq2))
   (list-union (registers-modified seq1)
	       (registers-modified seq2))
   (append (statements seq1) (statements seq2))))

(l-compile
 `(define (factorial n)
    (if (= n 1)
	1
      (* (factorial (- n 1)) n)))
 'val
 'next)
	   
	  
