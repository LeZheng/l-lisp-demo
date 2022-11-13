;;Scanner-spec      ::= ({Regexp-and-action }∗ )
;;Regexp-and-action ::= (Name ({Regexp}∗ ) Action)
;;Name              ::= Symbol
;;Regexp            ::= String | letter | digit | whitespace | any
;;                  ::= (not Character) | (or {Regexp}∗ )
;;                  ::= (arbno Regexp) | (concat {Regexp} ∗ )
;;Action            ::= skip | symbol | number | string

(setf scanner-spec-a
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

(defun re-read (v)
  (if (vectorp v)
      (let ((buffer (svref v 1))
	    (fun (svref v 0)))
	(if buffer
	    (cond
	      ((functionp fun) (re-read (vector (funcall fun (car buffer)) (cdr buffer))))
	      ((vectorp fun) (vector (svref fun 0) (append (svref fun 1) buffer)))
	      (t v))
	    fun))
      v))

(defun make-string-scanner (scanner-spec)
  (labels
      ((make-scanner (impl token-reducer)
	 (lambda (c) (if c
			 (funcall impl c)
			 (vector (funcall token-reducer nil) nil))))
       (regexp->scanner (regexp token-reducer chars-reducer)
	 (if (null regexp)
	     (vector (funcall token-reducer nil) nil)
	     (let ((exp (car regexp)))
	       (cond
		 ((consp exp)

		  (regexp->scanner exp
				   (lambda (s) (regexp->scanner
						(cdr regexp)
						(lambda (s2) (funcall token-reducer (append s s2)))
						chars-reducer))
				   chars-reducer))
		 ((stringp exp)
		  (regexp->scanner (cons (cons 'concat (coerce exp 'list)) (cdr regexp)) token-reducer chars-reducer))
		 ((characterp exp)
		  (make-scanner (lambda (c)
				  (if (equal exp c)
				      (regexp->scanner (cdr regexp)
						       (lambda (s) (funcall token-reducer (cons c s)))
						       (lambda (s) (funcall chars-reducer (cons c s))))
				      (vector nil (funcall chars-reducer (cons c nil)))))
				token-reducer))
		 ((eql exp 'letter)
		  (make-scanner (lambda (c)
				  (if (alpha-char-p c)
				      (regexp->scanner (cdr regexp)
						       (lambda (s) (funcall token-reducer (cons c s)))
						       (lambda (s) (funcall chars-reducer (cons c s))))
				      (vector nil (funcall chars-reducer (cons c nil)))))
				token-reducer))
		 ((eql exp 'digit)
		  (make-scanner (lambda (c)
				  (if (digit-char-p c)
				      (regexp->scanner (cdr regexp)
						       (lambda (s) (funcall token-reducer (cons c s)))
						       (lambda (s) (funcall chars-reducer (cons c s))))
				      (vector nil (funcall chars-reducer (cons c nil)))))
				token-reducer))
		 ((eql exp 'any)
		  (make-scanner (lambda (c)
				  (regexp->scanner (cdr regexp)
						   (lambda (s) (funcall token-reducer (cons c s)))
						   (lambda (s) (funcall chars-reducer (cons c s)))))
				token-reducer))
		 ((eql exp 'whitespace)
		  (regexp->scanner (cons '(concat (or #\Space #\NewLine) (arbno (or #\Space #\NewLine))) (cdr regexp))
				   token-reducer chars-reducer))
		 ((eql exp 'not)
		  (make-scanner (lambda (c)
				  (if (not (equal c (cadr regexp)))
				      (funcall token-reducer (cons c nil))
				      (vector nil (funcall chars-reducer (cons c nil)))))
				token-reducer))
		 ((eql exp 'or)
		  (labels ((make-or-scanner (scanners)
			     (make-scanner (lambda (c)
					     (let ((results (mapcar (lambda (scanner)
								      (if (functionp scanner)
									  (funcall scanner c)
									  scanner))
								    scanners)))
					       (let ((result (find-if (lambda (s) (and (vectorp s) (consp (svref s 0)))) results)))
						 ;;(format t "or scan :~A -> ~A~%" c results)
						 (if result
						     (re-read (vector (funcall token-reducer (svref result 0)) (svref result 1)))
						     (if (some #'functionp results)
							 (make-or-scanner results)
							 (vector nil (funcall chars-reducer (cons c nil))))))))
					   token-reducer)))
		    (make-or-scanner (mapcar (lambda (exp)
					       (regexp->scanner (cons exp nil) #'identity #'identity))
					     (cdr regexp)))))
		 ((eql exp 'arbno)
		  (make-scanner (lambda (c)
				  (let ((r (funcall (regexp->scanner (cdr regexp) #'identity #'identity) c)))
				    (labels ((handle-result (r reducer)
					       ;;(format t "handle result: ~A~%" r)
					       (etypecase r
						 (cons (regexp->scanner regexp
									(lambda (s) (funcall token-reducer (append r s)))
									(lambda (s) (funcall chars-reducer (append (funcall reducer nil) s)))))
						 (function
						  (make-scanner (lambda (c)
								  (handle-result (funcall r c) (lambda (s) (funcall reducer (cons c s)))))
								#'identity))
						 (vector
						  (if (null (svref r 0))
						      (re-read (vector (funcall token-reducer nil) (svref r 1)))
						      (re-read (vector
								(regexp->scanner regexp
										 (lambda (s) (funcall token-reducer (append (svref r 0) s)))
										 (lambda (s) (funcall chars-reducer (append (funcall reducer nil) s))))
								(svref r 1))))))))
				      (handle-result r #'identity))))
				token-reducer))
		 ((eql exp 'concat) (regexp->scanner (cdr regexp) token-reducer chars-reducer))
		 (t (error "Unknown expression:~A~%" exp)))))))
    (let ((scanner-list (mapcar (lambda (spec)
				  (destructuring-bind (name regexp action) spec
				    (regexp->scanner regexp
						     (lambda (s)
						       (let ((token-str (coerce s 'string)))
							 (ecase action
							   (skip (cons name nil))
							   (number (cons name (read-from-string token-str)))
							   (symbol (cons name (make-symbol token-str)))
							   (string (cons name token-str)))))
						     #'identity)))
				scanner-spec))
	  (token-list '()))
      (lambda (text)
	(labels ((iter-char (chars scanners tokens)
		   (format t "iter-char:~A -> ~A ~% tokens: ~A~%" (car chars) scanners tokens)
		   (if (some #'functionp scanners)
		       (iter-char (cdr chars)
				  (mapcar
				   (lambda (scanner)
				     (etypecase scanner
				       (function (let ((next (funcall scanner (car chars))))
						   (format t "next: ~A~%" next)
						   (etypecase next
						     (cons (vector next (cdr chars)))
						     (function next)
						     (vector (re-read (vector (svref next 0) (append (svref next 1) (cdr chars))))))))
				       (t scanner)))
				   scanners)
				  tokens)
		       (let ((token-result (reduce (lambda (&optional s1 s2)
						     ;;(format t "reduce: ~A ~A~%" s1 s2)
						     (if (> (length (svref s1 1)) (length (svref s2 1)))
							 s2
							 s1))
						   scanners)))
			 (if (null (svref token-result 0))
			     (error "Can not handle: ~A~%" (svref token-result 1))
			     (if (and (null chars) (null (svref token-result 1)))
				 (reverse tokens)
				 (iter-char (svref token-result 1)
					    scanner-list
					    (if (null (svref token-result 0))
						tokens
						(cons (svref token-result 0) tokens)))))))))
	  (iter-char (coerce text 'list) scanner-list '()))))))



(setf the-lexical-spec
      '((whitespace ((or #\Space #\NewLine) (arbno (or #\Space #\NewLine))) skip)
	(comment ("//" (arbno (not #\Newline))) skip)
	(identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
	(number (digit (arbno digit)) number)
	(number ("-" digit (arbno digit)) number)))

(setf the-grammar
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
	 ("let" (arbno identifier "=" expression) "in" expression)
	 let-exp)))

(defun test-scan ()
  (format t "---------------------- test scan --------------------------")
  (funcall (make-string-scanner the-lexical-spec)
	   "asdf  1234  -4321   // skdlajf"))

(defun scan1 (s)
  (funcall (make-string-scanner the-lexical-spec)
	   s))

(defun scan&parse1 (s)
  (funcall (make-string-parser the-lexical-spec the-grammar) s))

(defun test-parse ()
  (scan&parse1 "let x = y u1 = 321 in z "))
