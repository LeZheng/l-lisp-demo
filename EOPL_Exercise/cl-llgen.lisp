(defun make-token (class-name data)
  (list class-name data))

(defun make-string-scanner (scanner-spec)
  (lambda (str)
    (with-input-from-string
     (stream str)
     (scan-stream stream scanner-spec))))

(defun make-stream-scanner (scanner-spec)
  (lambda (stream)
    (scan-stream stream scanner-spec)))

(defun scan-stream (stream scanner-spac)
  (let* ((token-list '())
	 (scanner-list (mapcar #'token-spec->scanner scanner-spac))
	 (temp-scanners scanner-list)
	 (buffer '()))
    (do ((c (read-char stream) (read-char stream nil 'end-of-stream)))
	((not (characterp c)) (reverse token-list))
      (push c buffer)
      (setf temp-scanners
	    (mapcar #'(lambda (temp)
			(if (functionp temp)
			    (funcall temp c)
			    temp))
		    temp-scanners))
      (when (notany #'functionp temp-scanners)
	(if (every #'null temp-scanners)
	    (error "scan error:~A not match any scanner~%" buffer)
	    (let ((scan-result (car (sort (mapcar (lambda (s) (if (vectorp s) (svref s 0) s)) temp-scanners)
					  #'> :key #'length)))
		  (buffered-str (coerce (reverse buffer) 'string)))
	      (destructuring-bind
		    (token-string name action) scan-result
		(ecase action
		  (skip nil)
		  (symbol (push (make-token name (make-symbol token-string)) token-list))
		  (number (push (make-token name (read-from-string token-string)) token-list))
		  (string (push (make-token name token-string) token-list)))
		(if (string> buffered-str token-string)
		    (setf stream (make-concatenated-stream
				  (make-string-input-stream (subseq buffered-str (length token-string)))
				  stream)))
		(setf temp-scanners scanner-list)
		(setf buffer '()))))))))

(defun token-spec->scanner (raa)
  (destructuring-bind (name regexp action) raa
    (labels
	((make-concat-receiver (regexp receiver)
	   (lambda (char-seq)
	     (if (cdr regexp)
		 (regexp->scanner (cdr regexp)
				  (lambda (char-seq2)
				    (funcall receiver (append char-seq char-seq2))))
		 (funcall receiver char-seq))))
	 (make-single-char-scanner (regexp receiver char-tester)
	   (lambda (c)
	     (if (funcall char-tester c)
		 (if (cdr regexp)
		     (regexp->scanner (cdr regexp) (lambda (s) (funcall receiver (cons c s))))
		     (funcall receiver (cons c nil)))
		 nil)))
	 (regexp->scanner (regexp receiver)
	   (let ((exp (car regexp)))
	     (cond
	       ((stringp exp)
		(regexp->scanner
		 (cons (cons 'concat (coerce exp 'list)) (cdr regexp))
		 receiver))
	       ((consp exp)
		(regexp->scanner exp (make-concat-receiver regexp receiver)))
	       ((characterp exp)
		(make-single-char-scanner regexp receiver (lambda (c) (eql c exp))))
	       ((eql exp 'letter)
		(make-single-char-scanner regexp receiver #'alpha-char-p))
	       ((eql exp 'digit)
		(make-single-char-scanner regexp receiver #'digit-char-p))
	       ((eql exp 'any)
		(make-single-char-scanner regexp receiver (constantly t)))
	       ((eql exp 'not)
		(make-single-char-scanner regexp receiver (lambda (c) (not (eql c (cadr regexp))))))
	       ((eql exp 'whitespace)
		(regexp->scanner
		 '((or #\Space #\NewLine) (arbno (or #\Space #\NewLine)))
		 (make-concat-receiver regexp receiver)))
	       ((eql exp 'or)
		(let ((scanners (mapcar (lambda (e) (regexp->scanner (cons e nil) #'identity))
					(cdr regexp))))
		  (labels
		      ((make-or-scan (or-scanners)
			 (labels
			     ((or-scan (c)
				(let ((scanners (mapcar (lambda (scanner)
							  (if (functionp scanner)
							      (funcall scanner c)
							      scanner))
							or-scanners)))
				  (if (some #'functionp scanners)
				      (make-or-scan scanners)
				      (let ((r (car (sort scanners #'>
							  :key (lambda (s)
								 (if (vectorp s)
								     (length (svref s 0))
								     (length s)))))))
					(etypecase r
					  (cons (funcall receiver r))
					  (vector (vector (funcall receiver (svref r 0)) (svref r 1)))
					  (null nil)))))))
			   #'or-scan)))
		    (make-or-scan scanners))))
	       ((eql exp 'arbno)
		(lambda (c)
		  (let ((r (funcall (regexp->scanner (cdr regexp) #'identity) c)))
		    (labels
			((handle-result (r)
			   (etypecase r
			     (cons
			      (regexp->scanner regexp (lambda (s) (funcall receiver (append r s)))))
			     (vector
			      (vector (funcall receiver (svref s 0)) (svref s 1)))
			     (function
			      (lambda (c)
			       (handle-result (funcall r c))))
			     (null
			      (vector (funcall receiver nil) c)))))
		      (handle-result r)))))
	       ((eql exp 'concat)
		(regexp->scanner (cdr regexp) receiver))
	       (t (error "Unknown expression:~A~%" exp))))))
      (regexp->scanner regexp (lambda (s) (list (coerce s 'string) name action))))))

(defun parse-token (grammar-spec token-list)
  (let ((prod-parser-table (make-hash-table)))
    (dolist (production grammar-spec)
      (destructuring-bind (lhs rhs-list prod-name) production
	(push (production->parser production prod-parser-table) (gethash lhs prod-parser-table))))
    (let ((parser-list '()))
      (maphash (lambda (k v) (setf parser-list (append parser-list v))) prod-parser-table)
      (labels
	  ((iter (token-list parsers receiver)
	     (format t "iter:~A~%" (car token-list))
	     (if (null token-list)
		 (funcall receiver nil)
		 (let ((temps (mapcan (lambda (r)
					(and (or (functionp r) (consp r) (and (vectorp r) (consp (svref r 0)))) (list r)))
				      (mapcar (lambda (parser)
						(etypecase parser
						  (function (funcall parser (car token-list)))
						  (cons (vector r (cons (car token-list) nil)))
						  (vector (vector (svref r 0) (nconc (svref r 1) (cons (car token-list) nil))))))
					      parsers))))
		   (format t "temps:~A ~A~%" temps parsers)
		   (if (some #'functionp temps)
		       (iter (cdr token-list) temps receiver)
		       (let ((r (car (sort temps #'< :key (lambda (p) (etypecase p
									(cons 0)
									(vector (length (svref p 1)))))))))
			 (etypecase r
			   (cons (iter (cdr token-list) parser-list (lambda (gs) (cons r gs))))
			   (vector (iter (append (svref r 1) (cdr token-list)) parser-list (lambda (gs) (cons (svref r 0) gs)))))))))))
	(iter token-list parser-list #'identity)))))

(defun production->parser (production parser-table)
  (destructuring-bind (lhs rhs-list prod-name) production
    (labels
	((rhs->parser (remain-rhs receiver backouter)
	   (let ((rhs (car remain-rhs)))
	     (cond
	       ((and (symbolp rhs) (not (equal rhs 'arbno)) (not (equal rhs 'separated-list)))
		(labels
		    ((parse-lhs (parsers buffer)
		       (lambda (token)
			 (format t "parse-lhs:~A ~A~%" token rhs)
			 (if (null parsers)
			     (if (equal rhs (car token))
				 (if (cdr remain-rhs)
				     (rhs->parser (cdr remain-rhs)
					      (lambda (p) (cons (cadr token) p))
					      (lambda (ts) (funcall backouter (append (reverse (cons token buffer)) ts))))
				     (funcall receiver (cons (cadr token) nil)))
				 (vector nil (funcall backouter (reverse (cons token buffer)))))
			     (let* ((temp (mapcar (lambda (parser)
						    (if (functionp parser)
							(funcall parser token)
							parser))
						  parsers))
				    (r (find-if #'consp temp)))
			       (if r
				   (if (cdr remain-rhs)
				       (rhs->parser (cdr remain-rhs)
						(lambda (p) (cons r p))
						(lambda (ts) (funcall backouter (append (reverse (cons token buffer)) ts))))
				       (funcall receiver r))
				   (if (some #'functionp temp)
				       (parse-lhs temp (cons token buffer))
				       (vector nil (funcall backouter (reverse (cons token buffer)))))))))))
		  (parse-lhs (gethash rhs parser-table) nil)))
	       ((stringp rhs)
		(lambda (token)
		  (if (string-equal rhs (cadr token))
		      (if (cdr remain-rhs)
			  (rhs->parser (cdr remain-rhs) #'identity (lambda (ts) (funcall backouter (cons token ts))))
			  (funcall receiver nil))
		      (vector nil (cons token nil)))))
	       ((consp rhs)
		(lambda (token)
		  (funcall (rhs->parser rhs
				       (lambda (r1)
					 (if (cdr remain-rhs)
					     (rhs->parser (cdr remain-rhs)
							  (lambda (r2)
							    (funcall receiver (append r1 r2)))
							  (lambda (ts) (funcall backouter (cons token ts))))
					     (funcall receiver r1)))
				       backouter)
			   token)))
	       ((eql 'arbno rhs)
		(lambda (token)
		  (let ((r (funcall (rhs->parser (cdr remain-rhs) #'identity #'identity) token)))
		    (labels
			((handle-result (r buffer)
			   (etypecase r
			     (cons
			      (rhs->parser remain-rhs (lambda (r2) (funcall receiver (mapcar #'cons r r2)))
					   (lambda (ts) (funcall backouter (append (reverse buffer) ts)))))
			     (function
			      (lambda (token)
			       (handle-result (funcall r token) (cons token buffer))))
			     (vector
			      (vector (funcall receiver (svref r 0)) (svref r 1)))
			     (null
			      (vector (funcall receiver nil) (funcall backouter (reverse buffer)))))))
		      (handle-result r (cons token nil))))))
	       ((eql 'separated-list rhs)
		(lambda (token)
		  (let ((r (funcall (rhs-parser (butlast (cdr remain-rhs)) #'identity backouter) token)))
		    (labels
			((handle-result (r buffer)
			   (etypecase r
			     (cons
			      (lambda (token)
				(if (string-equal (cadr token) (car (last remain-rhs)))
				    (rhs->parser remain-rhs (lambda (r2) (funcall receiver (mapcar #'cons r r2)))
						 (lambda (ts) (funcall backouter (append (reverse (cons token buffer)) ts))))
				    (vector (funcall receiver nil) (funcall backouter (append (reverse (cons token buffer)) ts))))))
			     (function
			      (lambda (token)
			       (handle-result (funcall r token) (cons token buffer))))
			     (vector
			      (vector (funcall receiver (svref r 0)) (svref r 1)))
			     (null
			      (vector (funcall receiver nil) (funcall backouter (reverse buffer)))))))
		      (handle-result r (cons token nil))))))
	       (t (error "Unexpected rhs:~A~%" rhs))))))
      (rhs->parser rhs-list (lambda (r) (list prod-name r)) #'identity))))

(defun make-string-parser (the-lexical-spec the-grammar)
  (labels ((find-keywords (rhs-items receiver)
	     (if (null rhs-items)
		 (funcall receiver nil)
		 (etypecase (car rhs-items)
		   (string (find-keywords (cdr rhs-items) (lambda (ks) (funcall receiver (cons (car rhs-items) ks)))))
		   (symbol (find-keywords (cdr rhs-items) receiver))
		   (cons (let ((cks (find-keywords (car rhs-items) #'identity)))
			   (find-keywords (cdr rhs-items) (lambda (ks) (funcall receiver (append cks ks))))))))))
    (let* ((ext-lexical-spec `((keyword
				(or ,@(reduce #'union
					      (mapcar
					       (lambda (production) (find-keywords (cadr production) #'identity))
					       the-grammar)))
				string)
			       ,@the-lexical-spec))
	   (scanner (make-string-scanner ext-lexical-spec)))
      (lambda (s)
	(let ((token-list (funcall scanner s)))
	  (format t "token-list:~A~%" token-list)
	  (parse-token the-grammar token-list))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; test ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf the-lexical-spec
  '((whitespace ((or #\Space #\NewLine) (arbno (or #\Space #\NewLine))) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

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
     let-exp)   

    ))

(defun test-scan ()
  (funcall (make-string-scanner the-lexical-spec)
	   "asdf  1234  -4321   % skdlajf"))

(defun scan&parse1 (s)
  (funcall (make-string-parser the-lexical-spec the-grammar) s))
