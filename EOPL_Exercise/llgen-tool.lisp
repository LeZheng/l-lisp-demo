;;Scanner-spec      ::= ({Regexp-and-action }∗ )
;;Regexp-and-action ::= (Name ({Regexp}∗ ) Action)
;;Name              ::= Symbol
;;Regexp            ::= String | letter | digit | whitespace | any
;;                  ::= (not Character) | (or {Regexp}∗ )
;;                  ::= (arbno Regexp) | (concat {Regexp} ∗ )
;;Action            ::= skip | symbol | number | string

(defun make-string-scanner (scanner-spec)
  (format t "lexical spec:~A~%" scanner-spec)
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
				   (if (or token-chars (equal (car exp) 'arbno))
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
								  (if (>= (result-length (car a)) (result-length  (car b)))
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
					   (re-read
					    (regexp->scanner
					     regexp
					     #'identity
					     (lambda (tc2 rc2)
					       (if tc2
						   (funcall next-cont (funcall reducer (append token-chars tc2)) rc2)
						   (funcall next-cont (funcall reducer token-chars) rc2))))
					    remain-chars)
					   (funcall next-cont (funcall reducer nil) remain-chars))))
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
			 (progn
			   ;;(format t "iter token: ~A~%" r)
			   (if (svref (car r) 1)
			     (iter-char (append (cdr r) (cdr chars)) src-scanner (cons (car r) tokens))
			     (reverse tokens)))))
		   (mapcan (lambda (r)
			     (destructuring-bind (name chars action) (coerce r 'list)
			       (let ((token (coerce chars 'string)))
				 (ecase action
				   (symbol (list (make-symbol token)))
				   (number (list (read-from-string token)))
				   (string (list token))
				   (skip nil)))))
			   (reverse (cons (car (funcall scanner nil)) tokens))))))
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
	       (let ((rhs-item (car rhs-items)))
		 (labels
		     ((parse-string (token)
			(if (equal token rhs-item)
			    (rhs->parser (cdr rhs-items) (lambda (tokens) (funcall reducer (cons token tokens))) next-cont)
			    (funcall next-cont nil (funcall reducer `(,token)))))
		      (parse-symbol (token)
			(case rhs-item
			    (number (if (numberp token)
					(rhs->parser (cdr rhs-items) (lambda (tokens) (funcall reducer (cons token tokens))) next-cont)
					(funcall next-cont nil (funcall reducer `(,token)))))
			    (identifier
			     (if (not (numberp token))
				 (rhs->parser (cdr rhs-items) (lambda (tokens) (funcall reducer (cons token tokens))) next-cont)
				 (funcall next-cont nil (funcall reducer `(,token)))))
			    (otherwise
			     (let ((parser-list (gethash rhs-item parser-table)))
			       (if (null parser-list)
				   (error "unsupported symbol: ~A" rhs-item)
				   (labels
				       ((apply-parsers (token parsers)
					  (let ((parsers (mapcar (lambda (p) (try-call-parser p token)) parsers)))
					    (if (some #'functionp parsers)
						(lambda (token) (apply-parsers token parsers))
						(let ((result (reduce (lambda (&optional a b)
									(cond
									  ((null a) b)
									  ((null b) a)
									  (t (if (< (length (cdr a)) (length  (cdr b)))
										 a
										 b))))
								      parsers)))
						  (re-read
						   (rhs->parser (cdr rhs-items) (lambda (tokens) (funcall reducer (cons (car result) tokens))) next-cont)
						   (cdr result)))))))
				     (apply-parsers token parser-list)))))))
		      (parse-arbno (token)
			(labels
			    ((next-parser (arbno-cont)
			       (rhs->parser
				(cdr rhs-items)
			        #'identity
				(lambda (pt rt)
				  (if (null pt)
				      (funcall arbno-cont nil rt)
				      (re-read
				       (next-parser (lambda (pt2 rt2) (funcall arbno-cont (cons pt pt2) rt2)))
				       rt))))))
			  (funcall (next-parser next-cont) token)))
		      (parse-separated-list (token)
			(labels
			    ((next-parser (cont)
			       (rhs->parser
				(butlast (cdr rhs-items))
				#'identity
				(lambda (pt rt)
				  (if (null pt)
				      (funcall cont nil rt)
				      (re-read
				       (rhs->parser
					(last rhs-items)
					#'identity
					(lambda (s srt)
					  (if (null s)
					      (funcall cont pt srt)
					      (re-read
					       (next-parser (lambda (pt2 rt2) (funcall cont (cons pt (cons (car s) pt2)) rt2)))
					       srt))))
				       rt))))))
			  (funcall (next-parser next-cont) token))))
		   (lambda (token)
		     (if (null token)
			 (funcall next-cont nil (funcall reducer nil))
			 (let ((rhs-item (car rhs-items)))
			   (etypecase rhs-item
			     (simple-vector
			      (funcall
			       (rhs->parser (cons (svref rhs-item 1) (cdr rhs-items))
					    reducer next-cont)
			       token))
			     (string (parse-string token))
			     (symbol (case rhs-item
				       (arbno (parse-arbno token))
				       (separated-list (parse-separated-list token))
				       (otherwise (parse-symbol token))))
			     (cons
			      (funcall
			       (rhs->parser rhs-item #'identity
					   (lambda (pt rt)
					     (re-read
					      (rhs->parser (cdr rhs-items) (lambda (tokens) (funcall reducer (cons pt tokens))) next-cont)
					      rt)))
			       token)
			      ))))))))))
      (lambda (token-list)
	(let ((parser-list (mapcar
			    (lambda (spec)
			      (destructuring-bind (lhs rhs-items prod-name) spec
				(let ((parser (rhs->parser rhs-items #'identity (lambda (tokens rt)
										  (if tokens (cons (cons prod-name tokens) rt)))))
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
			       (lambda (&optional a b)
				 (cond
				   ((null a) b)
				   ((null b) a)
				   (t (if (< (length (cdr a)) (length  (cdr b)))
					  a
					  b))))
			       parsers)))
		       (if r
			   (iter-token (if (cdr r) (append (cdr r) tokens) tokens) parser-list (lambda (rs) (funcall reducer (cons (car r) rs))))
			   (funcall reducer nil))))))
	    (iter-token token-list parser-list #'identity)))))))

(defun make-string-parser (lexical-spec grammar)
  (labels ((walk (l) (let ((value (car l)))
		       (cond
			 ((simple-vector-p value) (append (walk (cons (svref value 1) nil)) (walk (cdr l))))
			 ((stringp value) (cons value (walk (cdr l))))
			 ((consp value) (append (walk value) (walk (cdr l))))
			 ((null value) '())
			 (t (walk (cdr l)))))))
    (let ((keywords '()))
      (macrolet ((def-datatype-from-grammar (grammar)
		   (let ((lhs-variants-map (make-hash-table))
			 (lhs-list '()))
		     (labels ((rhs-items->fields (rhs-items &optional (type-wrapper #'identity) (cont #'identity))
				(if (null rhs-items)
				    (funcall cont nil)
				    (let ((rhs-item (car rhs-items)))
				      (typecase rhs-item
					(simple-vector
					 (rhs-items->fields (cdr rhs-items) type-wrapper
							    (lambda (fields)
							      (cons (list (svref rhs-item 0) (funcall type-wrapper (svref rhs-item 1))) fields))))
					(cons
					 (rhs-items->fields rhs-item
							    (lambda (field) (funcall type-wrapper `(list-of ,field)))
							    (lambda (fields)
							      (rhs-items->fields (cdr rhs-items) type-wrapper
										 (lambda (fields2) (funcall cont (append fields fields2)))))))
					(otherwise (rhs-items->fields (cdr rhs-items) type-wrapper cont)))))))
		       
		       (dolist (prod grammar)
			 (let ((lhs (car prod)))
			   (pushnew lhs lhs-list)
			   (setf (gethash lhs lhs-variants-map) (cons prod (gethash lhs lhs-variants-map)))))
		       `(progn
			  ,@(mapcar (lambda (lhs)
				      (let ((variant-productions (gethash lhs lhs-variants-map)))
					`(define-datatype ,lhs ,(intern (concatenate 'string (symbol-name lhs) "-P"))
					   ,@(mapcar (lambda (production)
						       (destructuring-bind (lhs rhs-items prod-name) production
							 `(,prod-name
							   ,@(rhs-items->fields rhs-items))))
						     variant-productions))))
				    lhs-list)))))
		 (make-datatype-from-ast (ast)
		   (let ((builder-map (make-hash-table)))
		     (labels ((rhs-items->builder (rhs-items &optional (data-wrapper #'identity))
				(if (null rhs-items)
				    (funcall data-wrapper nil)
				    (lambda (ast)
				      (let (rhs-item (car rhs-items))
					(etypecase rhs-item ;;todo
					  (cons (if (consp (car ast))
						    (append
						     (funcall
						      (rhs-items->builder rhs-item (lambda (args) (cons 'list-of args)))
						      (car ast))
						     (funcall (rhs-items->builder (cdr rhs-items) data-wrapper) (cdr ast)))
						    (error "ast's head is not a cons ~A" ast)))
					  (symbol
					   (case rhs-item
					     (arbno )
					     (separated-list )
					     ( )))
					  (simple-vector
					   (cons (intern (symbol-name (svref rhs-item 0)) 'keyword)
						 (cons (funcall (gethash (caar ast) builder-map) (cdar ast))
						       (funcall (rhs-items->builder (cdr rhs-items) data-wrapper) (cdr ast)))))
					  (string (if (equal rhs-item (car ast))
						      (funcall (rhs-items->builder (cdr rhs-items) data-wrapper) (cdr ast))
						      (error "string is not equal:'~A' and '~A'" (car ast) rhs-item)))))))))
		       (dolist (production grammer)
			 (destructuring-bind (lhs rhs-items prod-name) production
			   (setf (gethash prod-name builder-map)
				 (rhs-items->builder rhs-items))))
		       (let* ((prod-name (car ast))
			      (builder (gethash prod-type builder-map)))
			 (if (null builder)
			     (error "Can not find builder for production: ~A~%" prod-name)
			     (funcall builder (cdr ast))))))))
	;;todo
	)
      (mapcar (lambda (k) (pushnew k keywords)) (walk grammar))
      (format t "keywords: ~A~%" keywords)
      (let ((scanner (make-string-scanner (cons (list 'keyword (cons 'or keywords) 'string)
						lexical-spec)))
	    (parser (make-token-parser grammar)))
	(lambda (text)
	  (let ((tokens (funcall scanner text)))
	    (format t "parsed tokens: ~A~%" tokens)
	    (funcall parser tokens)))))))

;;(defstruct (person (:constructor make-person (name age sex)) (:predicate person?)) name age sex)
;;(defstruct (student (:constructor make-student (name age sex school)) (:include person)) school)

(defmacro define-datatype (type-name predicate-name &rest variants)
  `(labels ((list-of (pred)
	      (lambda (val)
		(or (null val)
		 (and (consp val)
		  (funcall pred (car val))
		  (funcall (list-of pred) (cdr val)))))))
     (defstruct (,type-name (:predicate ,predicate-name)))
     ,@(mapcar (lambda (variant)
		 (destructuring-bind (variant-name &rest variant-slots) variant
		   `(defstruct (,variant-name)
		      ,@(mapcar
			 (lambda (slot)
			   (destructuring-bind (field-name predicate) slot
			     field-name
			     ;;todo handle slot
			     ))
			 variant-slots)))
		 )
	       variants)))

(setf scanner-spec-a
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

(setf the-lexical-spec
      '((whitespace (concat (or #\Space #\NewLine) (arbno (or #\Space #\NewLine))) skip)
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
	(expression
	 ("[" (separated-list expression ",") "]")
	 array-exp)
	(expression (identifier) var-exp)
	(expression
	 ("let" (arbno identifier "=" expression) "in" expression)
	 let-exp)))

(setf the-grammar
      '((program (#(content expression)) a-program)
	(expression (#(expr number)) const-exp)
	(expression
	 ("-" "(" #(minuend expression) "," #(subtrahend expression) ")")
	 diff-exp)
	(expression
	 ("zero?" "(" #(expr expression) ")")
	 zero?-exp)
	(expression
	 ("if" #(condition expression) "then" #(then-expr expression) "else" #(else-expr expression))
	 if-exp)
	(expression
	 ("[" (separated-list #(exprs expression) ",") "]")
	 array-exp)
	(expression (#(identifier identifier)) var-exp)
	(expression
	 ("let" (arbno #(vars identifier) "=" #(exprs expression)) "in" #(body expression))
	 let-exp)))

(defun test-scan ()
  (format t "---------------------- test scan --------------------------~%")
  (funcall (make-string-scanner the-lexical-spec)
	   "123 asdf  -432 iuo3u //asdf "))

(defun test-parse ()
  (format t "---------------------- test parse --------------------------~%")
  (let ((p (make-token-parser the-grammar)))
    (format t "~A~%" (funcall p (list "-" "(" 1 "," 2 ")")))
    (format t "~A~%" (funcall p (list "zero?" "(" 1 ")")))
    (format t "~A~%" (funcall p (list "let" 'x "=" 'y 'u1 "=" 321 'a "=" 33 "in" 'z)))
    (format t "~A~%" (funcall p (list "[" 'x "," 'y "," 1  "]")))
    ))

(defun scan1 (s)
  (funcall (make-string-scanner the-lexical-spec)
	   s))

(defun scan&parse1 (s)
  (funcall (make-string-parser the-lexical-spec the-grammar) s))

(defun test-scan-parse () 
 (scan&parse1 "let x = y u1 = 321 in z "))
