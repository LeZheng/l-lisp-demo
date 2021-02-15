(defun l-require (p)
  (if (not p) (amb)))

(defun an-element-of (items)
  (l-require (not (null items)))
  (amb (car items) (an-element-of (cdr items))))

;;; 4.35
(defun an-integer-between (a b)
  (if (> a b)
      (amb)
      (amb a (an-integer-between (+ a 1) b))))

;;; 4.36
;;使用 an-integer-starting-from 会产生无尽的选择，导致程序无法正常继续下去
(defun a-pythagorean-triple-from (low)
  (let ((k (an-integer-starting-from low)))
    (let ((j (an-integer-between low k)))
      (let ((i (an-integer-between low j)))
	(l-require (= (+ (* i i) (* j j)) (* k k)))
	(list i j k)))))					; (a-pythagorean-triple-from 0)

;;; 4.37
;; 本题的方法高，少了一个搜索分支

;;;; 4.3.2
(defun multiple-dwelling ()
  (let ((baker (amb 1 2 3 4 5))
	(cooper (amb 1 2 3 4 5))
	(fletcher (amb 1 2 3 4 5))
	(miller (amb 1 2 3 4 5))
	(smith (amb 1 2 3 4 5)))
    (l-require (distinctp (list baker cooper fletcher miller smith)))
    (l-require (not (= baker 5)))
    (l-require (not (= cooper 1)))
    (l-require (not (= fletcher 5)))
    (l-require (not (= fletcher 1)))
    (l-require (> miller cooper))
    (l-require (not (= (abs (- smith fletcher)) 1)))
    (l-require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
	  (list 'cooper cooper)
	  (list 'fletcher fletcher)
	  (list 'miller miller)
	  (list 'smith smith))))

;;; 4.38
;; (l-require (> (abs (- fletcher smith)) 1))

;;; 4.39
;;约束条件的顺序不会影响答案，会影响找到答案的时间
;; distinct 放最后

;;; 4.40
(defun multiple-dwelling-2 ()
  (let ((fletcher (amb 1 2 3 4 5)))
    (l-require (not (= fletcher 5)))
    (l-require (not (= fletcher 1)))
    (let ((cooper (amb 1 2 3 4 5)))
      (l-require (not (= cooper 1)))
      (l-require (not (= (abs (- fletcher cooper)) 1)))
      (let ((smith (amb 1 2 3 4 5)))
	(l-require (not (= (abs (- smith fletcher)) 1)))
	(let ((miller (amb 1 2 3 4 5)))
	  (l-require (> miller cooper))
	  (let ((baker (amb 1 2 3 4 5)))
	    (l-require (not (= baker 5)))
	    (l-require (distinctp (list baker cooper fletcher miller smith)))
	    (list (list 'baker baker)
		  (list 'cooper cooper)
		  (list 'fletcher fletcher)
		  (list 'miller miller)
		  (list 'smith smith))))))))

;;; 4.41
(defun multiple-dwelling-cl ()
  (let ((result '())
	(level-list '(1 2 3 4 5)))
    (dolist (baker level-list)
      (dolist (cooper level-list)
	(dolist (fletcher level-list)
	  (dolist (miller level-list)
	    (dolist (smith level-list)
	      (if (and
		   (distinctp (list baker cooper fletcher miller smith))
		   (not (= baker 5))
	       	   (not (= cooper 1))
		   (not (= fletcher 5))
		   (not (= fletcher 1))
		   (> miller cooper)
		   (not (= (abs (- smith fletcher)) 1))
		   (not (= (abs (- fletcher cooper)) 1)))
		  (push (list (list 'baker baker)
			      (list 'cooper cooper)
			      (list 'fletcher fletcher)
			      (list 'miller miller)
			      (list 'smith smith)) result)))))))))

;;; 4.42
(defun require-half-truth (p1 p2)
  (l-require (or (and p1 (not p2)) (and (not p1) p2))))

(defun (liars-puzzle)
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5)))
    (require-half-truth (= kitty 2) (= betty 3))
    (require-half-truth (= ethel 1) (= joan 2))
    (require-half-truth (= joan 1) (= ethel 5))
    (require-half-truth (= kitty 2) (= mary 4))
    (require-half-truth (= mary 4) (= betty 1))
    (l-require (distinctp (list betty ethel joan kitty mary)))
    (list (list 'betty betty)
          (list 'ethel ethel)
          (list 'joan joan)
          (list 'kitty kitty)
          (list 'mary mary))))

;;; 4.43 TODO

;;; 4.44
(defun safe-position (pos positions)
  (if (notany #'(lambda (p) (= pos p)) positions)
      (let ((len (length positions)))
	(dotimes (i len)
	  (let ((p (elt positions i)))
	    (if (= (- len i) (abs (- pos p)))
		(return nil))))
	(return t))))
	  

(defun eight-queen (size)
  (labels ((next-pos (col postions)
	     (let ((pos (an-integer-between 1 8)))
	       (l-require (safe-position pos positions))
	       (push pos positions)
	       (if (= col size)
		   positions
		   (next-pos (+ col 1) positions)))))
    (next-pos 1 '())))
	     

;;;自然语言的语法分析
(setf nouns '(noun student professor cat class))
(setf verbs '(verb studies lectures eats sleeps))
(setf articles '(article the a))
(setf prepositions '(prep for to in by with))

(defun parse-sentence ()
  (list 'sentence
	(parse-noun-phrase)
	(parse-word verbs)))

(defun parse-noun-phrase ()
  (list 'noun-phrase
	(parse-word articles)
	(parse-word nouns)))

(defun parse-word (word-list)
  (l-require (not (null *unparsed*)))
  (l-require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (setf *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(setf *unparsed* '())

(defun parse (input)
  (setf *unparsed* input)
  (let ((sent (parse-sentence)))
    (l-require (null unparsed*))
    sent))

(defun parse-prepositional-phrase ()
  (list 'prep-phrase
	(parse-word prepositions)
	(parse-noun-phrase)))

(defun parse-sentence ()
  (list 'sentence
	(parse-noun-phrase)
	(parse-verb-phrase)))

(defun parse-verb-phrase ()
  (labels ((maybe-extend (verb-phrase)
	     (amb verb-phrase
		  (maybe-extend (list 'verb-phrase
				      verb-phrase
				      (parse-prepositional-phrase))))))
    (maybe-extend (parse-word verbs))))

(defun parse-simple-noun-phrase ()
  (list 'simple-noun-phrase
	(parse-word articles)
	(parse-word nouns)))

(defun parse-noun-phrase ()
  (labels ((maybe-extend (noun-phrase)
	     (amb noun-phrase
		  (maybe-extend (list 'noun-phrase
				      noun-phrase
				      (parse-prepositional-phrase))))))
    (maybe-extend (parse-simple-noun-phrase))))

;;;4.45 略

;;;4.46 句子是从左到右开始解析的

;;; 4.47 行不通，会导致死循环，改变求值顺序，还是会有这种现象

;;; 4.48 TODO
	   
;;; 4.49
(defun (amb-list lst) 
  (if (null lst) 
      (amb) 
      (amb (car lst) (amb-list (cdr lst))))) 

(defun parse-word-create (word-list)
  (l-require (not (null *unparsed*)))
  (l-require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (amb-list (cdr word-list))))
    (setf *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))
  
;;;;4.3.3

(defun amb? (exp)
  (tagged-list? exp 'amb))

(define amb-choices (exp)
  (cdr exp))

(defun ambeval (exp env succeed fail)
  (funcall (analyze exp) env succeed fail))

(defun analyze-self-evaluating (exp)
  (lambda (env succeed fail)
    (funcall succeed exp fail)))

(defun analyze-quoted (exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (funcall succeed qval fail))))

(defun analyze-variable (exp)
  (lambda (env succeed fail)
    (funcall succeed (lookup-variable-value exp env)
	     fail)))

(defun analyze-lambda (exp)
  (let ((vars (lambda-parameters exp))
	(bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (funcall succeed (make-procedure vars bproc env) fail))))

(defun analyze-if (exp)
  (let ((pproc (analyze (if-predicate exp)))
	(cproc (analyze (if-consequent exp)))
	(aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (funcall pproc env
	       (lambda (pred-value fail2)
		 (if (true? pred-value)
		     (funcall cproc env succeed fail2)
		     (funcall aproc env succeed fail2)))
	       fail))))

(defun analyze-sequence (exps)
  (labels ((sequentially (a b)
	     (lambda (env succeed fail)
	       (funcall a env
			(lambda (a-value fail2)
			  (funcall b env succeed fail2))
			fail))))
    (labels ((l-loop (first-proc rest-procs)
	       (if (null rest-procs)
		   first-proc
		   (l-loop (sequentially first-proc (car rest-procs))
			   (cdr rest-procs)))))
      (let ((procs (mapcar #'analyze exps)))
	(if (null procs)
	    (error "Empty sequence -- ANALYZE"))
	(l-loop (car procs) (cdr procs))))))

(defun analyze-defination (exp)
  (let ((var (definition-variable exp))
	(vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (funcall vproc env
	       (lambda (val fail2)
		 (define-variable! var val env)
		 (funcall succeed 'ok fail2))
	       fail))))

(defun analyze-assignment (exp)
  (let ((var (assignment-variable exp))
	(vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (funcall vproc env
	       (lambda (val fail2)
		 (let ((old-value (lookup-variable-value var env)))
		   (set-variable-value! var val env)
		   (funcall succeed 'ok
			    (lambda ()
			      (set-variable-value! var old-value env)
			      (funcall fail2)))))
	       fail))))

(defun analyze-application (exp)
  (let ((fproc (analyze (operator exp)))
	(aprocs (mapcar #'analyze (operands exp))))
    (lambda (env succeed fail)
      (funcall fproc env
	       (lambda (proc fail2)
		 (get-args aprocs
			   env
			   (lambda (args fail3)
			     (execute-application proc args succeed fail3))
			   fail2))
	       fail))))

(defun get-args (aprocs env succeed fail)
  (if (null aprocs)
      (succeed '() fail)
      (funcall (car aprocs)
	       env
	       (lambda (arg fail2)
		 (get-args (cdr aprocs)
			   env
			   (lambda (args fail3)
			     (funcall succeed (cons arg args) fail3))
			   fail2))
	       fail)))

(defun execute-application (proc args succeed fail)
  (cond ((primitive-procedure? proc)
	 (funcall succeed (apply-primitive-procedure proc args)
		  fail))
	((compound-procedure? proc)
	 (funcall (procedure-body proc)
		  (extend-environment (procedure-parameters proc)
				      args
				      (procedure-environment proc))
		  succeed
		  fail))
	(t (error "Unknown procedure type -- EXECUTE-APPLICATION"))))

(defun analyze-amb (exp)
  (let ((cprocs (mapcar #'analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (labels ((try-next (choices)
		 (if (null choices)
		     (fail)
		     (funcall (car choices)
			      env
			      succeed
			      (lambda () (try-next (cdr choices)))))))
	(try-next cprocs)))))

(setf input-prompt ";;; Amb-Eval input:")
(setf output-prompt ";;; Amb-Eval value:")

(defun driver-loop ()
  (labels ((internal-loop (try-again)
	     (prompt-for-input input-prompt)
	     (let ((input (read)))
	       (if (eql input 'try-again)
		   (funcall try-again)
		   (begin
		    (format "~%;;; Starting a new problem ")
		    (ambeval input the-global-environment
			     (lambda (val next-alternative)
			       (announce-output output-prompt)
			       (user-print val)
			       (internal-loop next-alternative))
			     (lambda ()
			       (announce-output ";;; There are no more values of")
			       (user-print input)
			       (driver-loop))))))))
    (internal-loop
     (lambda ()
       (format ";;; There is no current problem")
       (driver-loop)))))

;;; 4.50 TODO
;;; 4.51 TODO
;;; 4.52 TODO
;;; 4.53 TODO
;;; 4.54 TODO
