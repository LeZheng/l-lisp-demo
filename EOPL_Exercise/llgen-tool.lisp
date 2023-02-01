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

(defstruct scan-result token-chars remain-chars)

(defun make-string-scanner (scanner-spec)
  (labels
      ((concat-reducer (reducer c)
	 (lambda (s) (funcall reducer (cons c s))))
       (regexp->scanner (regexp reducer next-cont)
	 (if (null regexp)
	     (funcall next-cont (funcall reducer nil) nil)
	     (lambda (c)
	       (labels
		   ((handle-single-char-with (predicate)
		      (if (funcall predicate c)
			  (regexp->scanner (cdr regexp) (concat-reducer reducer c) next-cont)
			  (funcall next-cont nil (funcall reducer `(,c))))))
		 (let ((exp (car regexp)))
		   (format t "read:~A ~A~%" c exp)
		   (etypecase exp
		     (cons
		      (regexp->scanner exp #'identity
				       (lambda (token-chars remain-chars)
					 (if token-chars
					     (regexp->scanner (cdr regexp)
							      (lambda (s) (funcall reducer (append token-chars s)))
							      next-cont)
					     (funcall next-cont nil (funcall reducer remain-chars))))))
		     (string
		      (regexp->scanner (cons (cons 'concat (coerce exp 'list)) (cdr regexp)) reducer next-cont))
		     (character
		      (handle-single-char-with (lambda (c) (equal exp c))))
		     (symbol
		      (ecase exp
			(letter
			 (handle-single-char-with #'alpha-char-p))
			(digit
			 (handle-single-char-with #'digit-char-p))
			(whitespace
			 (regexp->scanner (cons '(concat (or #\Space #\NewLine) (arbno (or #\Space #\NewLine))) (cdr regexp))
					  reducer next-cont))
			(any
			 (handle-single-char-with (lambda (c) t)))
			(not
			 (if (not (equal c (cadr regexp)))
			     (funcall next-cont (funcall reducer `(,c)) nil)
			     (funcall next-cont nil (funcall reducer `(,c)))))
			(or
			 (labels ((apply-scanners (scanners c)
				    (let ((scanners (mapcar
						     (lambda (scanner)
						       (if (functionp scanner)
							   (funcall scanner c)
							   scanner))
						     scanners)))
				      (if (some #'functionp scanners)
					  (lambda (c) (apply-scanners scanners c))
					  (let ((result (reduce
							 (lambda (&optional a b)
							   (if (and a b)
							       (if (> (length (if (vectorp (car a)) (svref (car a) 1) (car a)))
								      (length (if (vectorp (car b)) (svref (car b) 1) (car b))))
								   a
								   b)))
							 scanners)))
					    (funcall next-cont (car result) (cdr result)))))))
			   (apply-scanners (mapcar
					    (if (eql scanner-spec (cdr regexp))
						(lambda (spec)
						  (regexp->scanner
						   (cadr spec)
						   #'identity
						   (lambda (token-chars remain-chars)
						     (format t "scanned: ~A ~A~%" token-chars spec)
						     (cons (vector (car spec) token-chars (caddr spec)) remain-chars))))
						(lambda (sub-exp) (regexp->scanner
								   (if (consp sub-exp)
								       sub-exp
								       (cons sub-exp nil))
								   #'identity #'cons)))
					    (cdr regexp))
					   c)))
			(arbno
			 (regexp->scanner
			  (cdr regexp) #'identity
			  (lambda (token-chars remain-chars)
			    (if token-chars
				(regexp->scanner
				 regexp
				 #'identity
				 (lambda (tc2 rc2)
				   (if tc2
				       (funcall next-cont (funcall reducer (append token-chars tc2)) rc2)
				       (funcall next-cont (funcall reducer token-chars) rc2))))
				(funcall next-cont nil (funcall reducer remain-chars))))))
			(concat
			 (regexp->scanner (cdr regexp) reducer next-cont)))))))))))
    (lambda (text)
      (let ((src-scanner (regexp->scanner (cons 'or scanner-spec) #'identity #'cons)))
	(labels
	    ((iter-char (chars scanner tokens)
	       (if chars
		   (let ((r (funcall scanner (car chars))))
		     (if (functionp r)
			 (iter-char (cdr chars) r tokens)
			 (iter-char (append (cdr r) (cdr chars)) src-scanner (cons (car r) tokens))))
		   (reverse tokens))))
	  (iter-char (coerce text 'list) src-scanner nil))))))

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
	   "asdf  1234  -4321   // skdlajf "))

(defun scan1 (s)
  (funcall (make-string-scanner the-lexical-spec)
	   s))

(defun scan&parse1 (s)
  (funcall (make-string-parser the-lexical-spec the-grammar) s))

(defun test-parse ()
  (scan&parse1 "let x = y u1 = 321 in z "))
