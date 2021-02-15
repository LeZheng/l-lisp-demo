;;; 4.55
;;a) (supervisor ?p (Bitdiddle Ben))
;;b) (job ?p (accounting . ?type))
;;c) (address ?p (Slumerville . ?address))

;;; 4.56
;;a) (and (supervisor ?p (Bitdiddle Ben)) (address ?p ?where))
;;b) (and (salary (Bitdiddle Ben) ?b-salary) (salary ?p ?salary) (lisp-value > ?b-salary ?salary))
;;c) (and (supervisor ?p ?boss) (not (job ?boss (computer . ?type))) (job ?boss ?b-job))

;;; 4.57
(rule (replace-dy ?a ?b)
      (and (job ?a ?s-job)
	   (job ?b ?s-job)
	   (not (same $a $b))))
;;a) (replace-dy (D.Fect Cy) ?p)
;;b) (and (salary ?a ?a-salary) (salary ?b ?b-salary) (lisp-value > ?a-salary ?b-salary) (replace-dy ?a ?b))

;;; 4.58
(rule (dawan ?a ?p)
      (and (job ?a (?p .))
	   (supervisor ?a ?boss)
	   (job ?boss (?p .))))

;;; 4.59
;;a) (meeting ?p (Friday ?t))
;;b) (rule (meeting-time ?person ?day-and-time) (or (meeting whole-company ?day-and-time) (job ?person (?p .)) (meeting ?p ?day-and-time)))
;;c) (meeting-time (Haccker Alyssa P) (Wednesday .))

;;; 4.60
;;因为两种都满足条件
;;对名称进行字母排序

;;; 4.61
;; ((2 3) next-to 4 in (1 (2 3) 4)) 
;; (1 next-to (2 3) in (1 (2 3) 4))

;; (3 next-to 1 in (2 1 3 1))
;; (2 next-to 1 in (2 1 3 1))

;;; 4.62
(rule (last-pair (?x) (?x)))
(rule (last-pair (?a . ?b) ?x)
      (last-pair ?v ?x))

;;; 4.63
(rule (grand-son ?s ?g)
      (and (son ?s ?f)
	   (son ?f ?g)))

(rule (son ?s ?m)
      (and (wife ?w ?m)
	   (son ?s ?w)))

;;; 4.64
;; outranked-by 的递归不会终止

;;; 4.65
;; 因为这里有4种数据满足wheel的情况

;;; 4.66
;; 结果会重复累加，添加去重语句

;;; 4.67 TODO

;;; 4.68
(rule (reverse () ()))
(rule (reverse (?x . ?y) ?z)
      (and (reverse ?y ?u)
	   (append-to-form ?u (?x) ?z)))

;;; 4.69 TODO
      
;;;; 4.4.4
(defparameter input-prompt ";;; Query input:")
(defparameter output-prompt ";;; Query Result:")

(defun query-driver-loop ()
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
	   (add-rule-or-assertion! (add-assertion-body q))
	   (format t "Assertion added to data base.")
	   (query-driver-loop))
	  (t
	   (format t "~%~A" output-prompt)
	   (display-stream
	    (stream-map
	     (lambda (frame)
	       (instantiate q frame (lambda (v f) (contract-question-mark v))))
	     (qeval q (singleton-stream '()))))
	   (query-driver-loop)))))

(defun instantiate (exp frame unbound-var-handler)
  (labels ((inner-copy (exp)
	     (cond ((var? exp)
		    (let ((binding (binding-in-frame exp frame)))
		      (if binding
			  (inner-copy (binding-value binding))
			  (funcall unbound-var-handler exp frame))))
		   ((pair? exp)
		    (cons (inner-copy (car exp)) (inner-copy (cdr exp))))
		   (t exp))))
    (inner-copy exp)))

(defun qeval (query frame-stream)
  (let ((qproc (get 'qeval (type query))))
    (if qproc
	(funcall qproc (contents query) frame-stream)
	(simple-query query frame-stream))))

(defun simple-query (query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame))))
   frame-stream))

(defun conjoin (conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
	       (qeval (first-conjunct conjuncts)
		      frame-stream))))

(setf (get 'qeval 'and) #'conjoin)

(defun disjoin (disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts) frame-stream)))))

(setf (get 'qeval 'or) #'disjoin)

(defun negate (operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null? (qeval (negated-query operands)
			      (singleton-stream frame)))
	 (singleton-stream frame)
	 the-empty-stream))
   frame-stream))

(setf (get 'qeval 'not) #'negate)

(defun lisp-value (call frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (execute
	  (instantiate
	   call
	   frame
	   (lambda (v f)
	     (error "Unknown pat var -- LISP-VALUE" v))))
	 (singleton-stream frame)
	 the-empty-stream))
   frame-stream))

(setf (get 'qeval 'lisp-value) #'lisp-value)

(defun execute (exp)
  (apply (eval (predicate exp) user-initial-environment)
	 (args exp)))

(defun always-true (ignore frame-stream)
  frame-stream)
(setf (get 'qeval 'always-true) #'always-true)

(defun find-assertions (pattern frame)
  (stream-flatmap
   (lambda (datum)
     (check-an-assertion datum pattern frame))
   (fetch-assertions pattern frame)))

(defun check-an-assertion (assertion query-pat query-frame)
  (let ((match-result (pattern-match query-pat assertion query-frame)))
    (if (eql match-result 'failed)
	the-empty-stream
	(singleton-stream match-result))))

(defun pattern-match (pat dat frame)
  (cond ((eql frame 'failed) 'failed)
	((equalp pat dat) frame)
	((var? pat) (extend-if-consistent pat dat frame))
	((and (pair? pat) (pair? dat))
	 (pattern-match (cdr pat) (cdr dat)
			(pattern-match (car pat) (car dat)
				       frarme)))
	(t 'failed)))

(defun extend-if-consistent (var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
	(pattern-match (binding-value binding) dat frame)
	(extend var dat frame))))

(defun apply-rules (pattern frame)
  (stream-flatmap
   (lambda (rule)
     (apply-a-rule rule pattern frame))
   (fetch-rules pattern frame)))

(defun apply-a-rule (rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result (unify-match query-pattern
				     (conclusion clean-rule)
				     query-frame)))
      (if (eql unify-result 'failed)
	  the-empty-stream
	  (qeval (rule-body clean-rule)
		 (singleton-stream unify-result))))))

(defun rename-variables-in (rule)
  (let ((rule-application-id (new-rule-application-id)))
    (labels ((tree-walk (exp)
	       (cond ((var? exp) (make-new-variable exp rule-application-id))
		     ((pair? exp)
		      (cons (tree-walk (car exp))
			    (tree-walk (cdr exp))))
		     (t exp))))
      (tree-walk rule))))

(defun unify-match (p1 p2 frame)
  (cond ((eql frame 'failed) 'failed)
	((equalp p1 p2) frame)
	((var? p1) (extend-if-possible p1 p2 frame))
	((var? p2) (extend-if-possible p2 p1 frame))
	((and (pair? p1) (pair? p2))
	 (unify-match (cdr p1)
		      (cdr p2)
		      (unify-match (car p1)
				   (car p2)
				   frame)))
	(t 'failed)))

(defun extend-if-possible (var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding (unify-match (binding-value binding) val frame))
	  ((var? val)
	   (let ((binding (binding-in-frame val frame)))
	     (if binding
		 (unify-match var (binding-value binding) frame) (extend var val frame))))
	  ((depends-on? val var frame) 'failed)
	  (t (extend var val frame)))))

(defun depends-on? (exp var frame)
  (labels ((tree-walk (e)
	     (cond ((var? e)
		    (if (equalp var e)
			t
			(let ((b (binding-in-frame e frame)))
			  (if b
			      (tree-walk (binding-value b))
			      nil))))
		   ((pair? e)
		    (or (tree-walk (car e))
			(tree-walk (cdr e))))
		   (t nil))))
    (tree-walk exp)))

(defparameter THE-ASSERTIONS the-empty-stream)

(defun fetch-assertions (pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))

(defun get-all-assertions () THE-ASSERTIONS)

(defun get-indexed-assertions (pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(defun get-stream (key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))

(defparameter THE-RULES the-empty-stream)

(defun fetch-rules (pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))

(defun get-all-rules () THE-RULES)

(defun get-indexed-rules (pattern)
  (stream-append
   (get-stream (index-key-of pattern) 'rule-stream)
   (get-stream '? 'rule-stream)))

(defun add-rule-or-assertion! (assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(defun add-assertion! (assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (setf THE-ASSERTIONS (cons-stream assertion old-assertions))
    'ok))

(defun add-rule! (rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (setf THE-RULES (cons-stream rule old-rules))
    'ok))

(defun store-assertion-in-index (assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
	(let ((current-assertion-stream (get-stream key 'assertion-stream)))
	  (setf (get 'assertion-stream key) (cons-stream assertion current-assertion-stream))))))

(defun store-rule-in-index (rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
	(let ((current-rule-stream (get-stream key 'rule-stream)))
	  (setf (get 'rule-stream key) (cons-stream rule current-rule-stream))))))

(defun indexable? (pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(defun index-key-of (pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

(defun use-index? (pat)
  (constant-symbol? (car pat)))

;;; 4.70
;; 保存旧的值 TODO

(defun stream-append-delayed (s1 delay-s2)
  (if (stream-null? s1)
      (force delay-s2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed (stream-cdr s1) delay-s2))))

(defun interleave-delayed (s1 delay-s2)
  (if (stream-null? s1)
      (force delay-s2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed (stream-cdr s1)
			      (interleave-delayed (force delay-s2)
						  (delay (stream-cdr s1)))))))

(defun stream-flatmap (proc s)
  (flatten-stream (stream-map proc s)))

(defun flatten-stream (stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))

(defun singleton-stream (x)
  (cons-stream x the-empty-stream))

(defun type (exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))

(defun contents (exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS" exp)))

(defun assertion-to-be-added? (exp)
  (eql (type exp) 'assert!))

(defun add-assertion-body (exp)
  (car (contents exp)))

(defun empty-conjunction? (exps) (null exps))
(defun first-conjunct (exps) (car exps))
(defun rest-conjuncts (exps) (cdr exps))

(defun empty-disjunction? (exps) (null exps))
(defun first-disjunct (exps) (car exps))
(defun rest-disjuncts (exps) (cdr exps))

(defun negated-query (exps) (car exps))

(defun predicate (exps)
  (car exps))

(defun args (exps)
  (cdr exps))

(defun rule? (statement)
  (tagged-list? statement 'rule))

(defun conclusion (rule)
  (cadr rule))

(defun rule-body (rule)
  (if (null (cddr rule))
      '(always-true)
      (caddr rule)))

(defun query-syntax-process (exp)
  (map-over-symbols #'expand-question-mark exp))

(defun (map-over-symbols proc exp)
    (cond ((pair? exp)
	   (cons (map-over-symbols proc (car exp))
		 (map-over-symbols proc (cdr exp))))
	  ((symbolp exp) (proc exp))
	  (t exp)))

(defun expand-question-mark (symbol)
  (let ((chars (symbol->string symbol)))
    (if (string= (substring chars 0 1) "?")
	(list '?
	      (string->symbol (substring chars 1 (string-length chars))))
	symbol)))

(defun var? (exp)
  (tagged-list? exp '?))

(defun constant-symbol? (exp)
  (symbolp exp))

(defvar rule-counter 0)

(defun new-rule-application-id ()
  (setf rule-counter (+ 1 rule-counter))
  rule-counter)

(defun make-new-variable (var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

(defun contract-question-mark (variable)
  (string->symbol
   (string-append "?"
		  (if (numberp (cadr variable))
		      (string-append (symbol->string (caddr variable))
				     "-"
				     (number->string (cadr variable)))
		      (symbol->string (cadr variable))))))

(defun make-binding (variable value)
  (cons variable value))

(defun binding-variable (binding)
  (car binding))

(defun binding-value (binding)
  (cdr binding))

(defun binding-in-frame (variable frame)
  (assoc variable frame))

(defun extend (varialbe value frame)
  (cons (make-binding variable value) frame))

;;; 剩余练习 TODO
	   
