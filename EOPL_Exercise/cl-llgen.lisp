(defvar the-lexical-spec
  '((whitespace ((or #\Space #\NewLine) (arbno (or #\Space #\NewLine))) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(defvar the-grammar
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
     ("let" identifier "=" expression "in" expression)
     let-exp)   

    ))

(defun make-token (class-name data)
  (list class-name data))

(defun make-string-scanner (scanner-spec grammar)
  (lambda (str)
    (with-input-from-string
     (stream str)
     (scan-stream stream scanner-spec grammar))))

(defun make-stream-scanner (scanner-spec grammar)
  (lambda (stream)
    (scan-stream stream scanner-spec grammar)))

(defun scan-stream (stream scanner-spac grammar)
  (let* ((token-list '())
	 (scanner-list (mapcar #'token-spec->scanner scanner-spac))
	 (temp-scanners scanner-list)
	 (buffer '()))
    (format t "scanner-list:~A~%" scanner-list)
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
	      (format T "scan:~A ~A ~A~%" buffer scan-result temp-scanners)
	      (destructuring-bind
		    (token-string name action) scan-result
		(ecase action
		  (skip nil)
		  (symbol (push (make-token name (make-symbol token-string)) token-list))
		  (number (push (make-token name (read-from-string token-string)) token-list))
		  (string (push (make-token name token-string) token-list)))
		(format t "token-list updated:~A~%" token-list)
		(if (string> buffered-str token-string)
		    (setf stream (make-concatenated-stream
				  (make-string-input-stream (subseq buffered-str (length token-string)))
				  stream)))
		(setf temp-scanners scanner-list)
		(setf buffer '()))))))))

;;;scanner: (char) -> scanner | token | nil
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
		 (make-concat-receiver regexp receiver)))
	       ((consp exp)
		(regexp->scanner
		 exp
		 (make-concat-receiver regexp receiver)))
	       ((characterp exp)
		(make-single-char-scanner regexp receiver (lambda (c) (eql c exp))))
	       ((eql exp 'letter)
		(make-single-char-scanner regexp receiver #'alpha-char-p))
	       ((eql exp 'digit)
		(make-single-char-scanner regexp receiver #'digit-char-p))
	       ((eql exp 'any)
		(make-single-char-scanner regexp receiver (constantly t)))
	       ((eql exp 'not)
		(make-single-char-scanner regexp receiver (lambda (c) (not (eql c (cadr exp))))))
	       ((eql exp 'whitespace)
		(regexp->scanner
		 '((or #\Space #\NewLine) (arbno (or #\Space #\NewLine)))
		 (make-concat-receiver regexp receiver)))
	       ((eql exp 'or)
		(let ((scanners (mapcar (lambda (e)
					  (regexp->scanner (cons e nil) #'identity))
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
		  (let ((r (funcall 
			    (regexp->scanner (cdr regexp)
					     (lambda (s) s));;todo
			    c)))
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
		(regexp->scanner
		 (cdr regexp)
		 (make-concat-receiver regexp receiver)))
	       (t (error "Unknown expression:~A~%" exp))))))
      (regexp->scanner regexp (lambda (s) (list (coerce s 'string) name action))))))

(defun make-string-parser (scanner-spec grammar)
  ;;todo
  )



