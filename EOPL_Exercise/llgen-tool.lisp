;;Scanner-spec      ::= ({Regexp-and-action }∗ )
;;Regexp-and-action ::= (Name ({Regexp}∗ ) Action)
;;Name              ::= Symbol
;;Regexp            ::= String | letter | digit | whitespace | any
;;                  ::= (not Character) | (or {Regexp}∗ )
;;                  ::= (arbno Regexp) | (concat {Regexp} ∗ )
;;Action            ::= skip | symbol | number | string

(defun make-string-scanner (scanner-spec)
  (labels
      ((concat-reducer (reducer c)
	 (lambda (s) (funcall reducer (cons c s))))
       (result-length (r) (length (if (vectorp r) (svref r 1) r)))
       (re-read (scanner remain-chars)
	 (if remain-chars
	     (if (functionp scanner)
		 (re-read (funcall scanner (car remain-chars)) (cdr remain-chars))
		 (cons (car scanner) (append (cdr scanner) remain-chars)))
	     scanner))
       (regexp->scanner (regexp reducer next-cont)
	 (if (null regexp)
	     (funcall next-cont (funcall reducer nil) nil)
	     (lambda (c)
	       (if (null c)
		   (funcall next-cont (funcall reducer nil) nil)
		   (labels
		       ((handle-single-char-with (predicate)
			  (if (funcall predicate c)
			      (regexp->scanner (cdr regexp) (concat-reducer reducer c) next-cont)
			      (funcall next-cont nil (funcall reducer `(,c))))))
		     (let ((exp (car regexp)))
		       (etypecase exp
			 (cons (funcall
				(regexp->scanner
				 exp
				 #'identity
				 (lambda (token-chars remain-chars)
				   (if token-chars
				       (re-read (regexp->scanner
						 (cdr regexp)
						 (lambda (s) (funcall reducer (append token-chars s)))
						 next-cont)
						remain-chars)
				       (funcall next-cont nil (funcall reducer remain-chars)))))
				c))
			 (string (funcall (regexp->scanner `((concat ,@(coerce exp 'list)) ,@(cdr regexp)) reducer next-cont) c))
			 (character (handle-single-char-with (lambda (c) (equal exp c))))
			 (symbol
			  (ecase exp
			    (letter (handle-single-char-with #'alpha-char-p))
			    (digit (handle-single-char-with #'digit-char-p))
			    (whitespace (funcall
					 (regexp->scanner
					  (cons '(concat (or #\Space #\NewLine) (arbno (or #\Space #\NewLine))) (cdr regexp))
					  reducer next-cont)
					 c))
			    (any (handle-single-char-with (lambda (c) t)))
			    (not (if (not (equal c (cadr regexp)))
				     (funcall next-cont (funcall reducer `(,c)) nil)
				     (funcall next-cont nil (funcall reducer `(,c)))))
			    (or (labels
				    ((apply-scanners (scanners c)
				       (let ((scanners (mapcar
							(lambda (scanner)
							  (if (functionp scanner)
							      (funcall scanner c)
							      (append scanner `(,c))));;append need optimize
							scanners)))
					 (if (some #'functionp scanners)
					     (lambda (c) (apply-scanners scanners c))
					     (let ((result (reduce
							    (lambda (&optional a b)
							      (if (and a b)
								  (if (> (result-length (car a)) (result-length  (car b)))
								      a
								      b)))
							    scanners)))
					       (funcall next-cont (car result) (cdr result)))))))
				  (apply-scanners
				   (mapcar
				    (if (eql scanner-spec (cdr regexp))
					(lambda (spec)
					  (regexp->scanner
					   (cadr spec)
					   #'identity
					   (lambda (token-chars remain-chars)
					     (cons (vector (car spec) token-chars (caddr spec)) remain-chars))))
					(lambda (sub-exp) (regexp->scanner
							   (if (consp sub-exp) sub-exp (cons sub-exp nil))
							   #'identity #'cons)))
				    (cdr regexp))
				   c)))
			    (arbno (funcall
				    (regexp->scanner
				     (cdr regexp)
				     #'identity
				     (lambda (token-chars remain-chars)
				       (if token-chars
					   (regexp->scanner
					    regexp
					    #'identity
					    (lambda (tc2 rc2)
					      (if tc2
						  (funcall next-cont (funcall reducer (append token-chars tc2)) rc2)
						  (funcall next-cont (funcall reducer token-chars) rc2))))
					   (funcall next-cont nil (funcall reducer remain-chars)))))
				    c))
			    (concat
			     (funcall (regexp->scanner (cdr regexp) reducer next-cont) c))))))))))))
    (lambda (text)
      (let ((src-scanner (regexp->scanner (cons 'or scanner-spec) #'identity #'cons)))
	(labels
	    ((iter-char (chars scanner tokens)
	       (if chars
		   (let ((r (funcall scanner (car chars))))
		     (if (functionp r)
			 (iter-char (cdr chars) r tokens)
			 (if (svref (car r) 1)
			     (iter-char (append (cdr r) (cdr chars)) src-scanner (cons (car r) tokens))
			     (reverse tokens))))
		   (reverse (cons (car (funcall scanner nil)) tokens)))))
	  (iter-char (coerce text 'list) src-scanner nil))))))

;;Grammar    ::= ({Production}∗ )
;;Production ::= (Lhs ({Rhs-item}∗ ) Prod-name)
;;Lhs        ::= Symbol
;;Rhs-item   ::= Symbol | String
;;           ::= (arbno {Rhs-item}∗ )
;;           ::= (separated-list {Rhs-item}∗ String)
;;Prod-name  ::= Symbol

(defun make-token-parser (grammar-spec)
  (let ((parser-table (make-hash-table)))
    (labels
	((try-call-parser (parser token)
	   (if (functionp parser) (funcall parser token) (append parser `(,token))));;append need optimize
	 (re-read (parser tokens)
	   (if (null tokens)
	       parser
	       (if (functionp parser)
		   (re-read (funcall parser (car tokens)) (cdr tokens))
		   (cons (car parser) (append (cdr parser) tokens)))))
	 (rhs->parser (rhs-items reducer next-cont)
	   (if (null rhs-items)
	       (funcall next-cont (funcall reducer nil) nil)
	       (lambda (token)
		 (let ((rhs-item (car rhs-items)))
		   (etypecase rhs-item
		     (string (if (equal token rhs-item)
				 (rhs->parser (cdr rhs-items) (lambda (tokens) (funcall reducer (cons token tokens))) next-cont)
				 (funcall next-cont nil (funcall reducer `(,token)))))
		     (symbol
		      (case rhs-item
			(number (if (numberp token)
				    (rhs->parser (cdr rhs-items) (lambda (tokens) (funcall reducer (cons token tokens))) next-cont)
				    (funcall next-cont nil (funcall reducer `(,token)))))
			(identifier
			 (rhs->parser (cdr rhs-items) (lambda (tokens) (funcall reducer (cons token tokens))) next-cont))
			(otherwise
			 (let ((parser-list (gethash rhs-item parser-table)))
			   (if (null parser-list)
			       (error "unsupported symbol: ~A" rhs-item)
			       (labels
				   ((apply-parsers (token parsers)
				      (let ((parsers (mapcar #'try-call-parser parsers)))
					(if (some #'functionp parsers)
					    (lambda (token) (apply-parsers token parsers))
					    (let ((result (reduce (lambda (&optional a b)
								    (if (and a b)
									(if (< (length (cdr a)) (length  (cdr b)))
									    a
									    b)))
								  parsers)))
					      (funcall next-cont (car result) (cdr result)))))))
				 (apply-parsers token parser-list)))))))
		     (cons
		      (let ((start-sym (car rhs-item)))
			(case start-sym
 			  (arbno
			   (funcall
			    (rhs->parser
			     (cdr rhs-item)
			     #'identity
			     (lambda (pt rt)
			       (if (null pt)
				   (funcall next-cont nil (funcall reducer rt))
				   (re-read
				    (rhs->parser rhs-item #'identity
						 (lambda (pt2 rt2)
						   (if (null pt2)
						       (funcall next-cont (funcall reducer pt) rt2)
						       (funcall next-cont (funcall reducer (mapcar #'cons pt pt2)) rt2))))
				    rt))))
			    token))
			  (separated-list
			   (funcall
			    (rhs->parser
			     (butlast (cdr rhs-item))
			     #'identity
			     (lambda (pt rt)
			       (if (null pt)
				   (funcall next-cont nil (funcall reducer rt))
				   (re-read
				    (rhs->parser
				     (last rhs-item)
				     #'identity
				     (lambda (s srt)
				       (if (null s)
					   (funcall  next-cont (funcall reducer pt) srt)
					   (rhs->parser
					    rhs-item #'identity
					    (lambda (pt2 rt2)
					      (if (null pt2)
						  (funcall next-cont (funcall reducer pt) rt2)
						  (funcall next-cont (funcall reducer (mapcar #'cons pt pt2)) rt2)))))))
				    rt))))
			    token))
			  (otherwise (error "unexpected start symbol ~A" start-sym)))))))))))
      (lambda (token-list)
	(let ((parser-list (mapcar
			    (lambda (spec)
			      (destructuring-bind (lhs rhs-items prod-name) spec
				(let ((parser (rhs->parser rhs-items #'identity (lambda (tokens rt)
										  (cons (cons prod-name tokens) rt))))
				      (prev-parsers (gethash lhs parser-table)))
				  (setf (gethash lhs parser-table) (cons parser prev-parsers));;side effect
				  parser)))
			    grammar-spec)))
	  (labels
	      ((iter-token (tokens parsers reducer)
		 (if (some #'functionp parsers)
		     (iter-token (cdr tokens)
				 (mapcar (lambda (parser) (try-call-parser parser (car tokens))) parsers)
				 reducer)
		     (let ((r (reduce
			       (lambda (&optional p1 p2)
				 (if (and p1 p2)
				     (if (< (length (cdr p1)) (length (cdr p2)))
					 p1
					 p2)))
			       parsers)))
		       (if r
			   (iter-token tokens parser-list (lambda (rs) (funcall reducer (cons r rs))))
			   (funcall reducer nil))))))
	    (iter-token token-list parser-list)))))))

(setf scanner-spec-a
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

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
