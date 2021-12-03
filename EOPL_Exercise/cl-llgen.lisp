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
    (do ((c (read-char stream) (read-char stream nil 'end-of-stream)))
	((not (characterp c)) (reverse token-list))
	(push c buffer)
	(setf temp-scanners
	      (mapcan #'(lambda (temp)
			  (if (functionp temp)
			      (funcall temp c)
			    temp))
		      temp-scanners))
	(if (notany #'functionp temp-scanners)
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
	       (if (string> buffered-str token-list)
		   (setf stream (make-concatenated-stream
				 (make-string-input-stream (subseq buffered-str (length token-list)))
				 stream)))
	       (setf buffer '())))))))

;;;scanner: (char) -> scanner | token | nil
(defun token-spec->scanner (raa)
  (let ((name (car raa))
	(action (caddr raa)))
    (labels
     ((make-concat-receiver (regexp receiver)
			    (lambda (char-seq)
			      (if (cdr regexp)
				  (regexp->scanner (cdr regexp)
						   (lambda (char-seq2)
						     (funcall receiver (append char-seq char-seq2))))
				(funcall receiver char-seq))))
      (regexp->scanner (regexp receiver)
		       (let ((exp (car regexp)))
			 (cond
			  ((stringp exp)
			   (regexp->scanner
			    (cons (cons 'concat (coerce exp 'list)) (cdr regexp))
			    (make-concat-receiver regexp receiver)))
			  ((characterp exp)
			   (lambda (c)
			     (if (eql c exp)
				 (if (cdr regexp)
				     (regexp->scanner (cdr regexp) (lambda (s) (funcall receiver (cons c s))))
				   (funcall receiver (cons c nil)))
			       nil)))
			  ((eql exp 'letter)
			   (lambda (c)
			     (if (alpha-char-p c)
				 (if (cdr regexp)
				     (regexp->scanner (cdr regexp) (lambda (s) (funcall receiver (cons c s))))
				   (funcall receiver (cons c nil)))
			       nil)))
			  ((eql exp 'digit)
			   (lambda (c)
			     (if (digit-char-p c)
				 (if (cdr regexp)
				     (regexp->scanner (cdr regexp) (lambda (s) (funcall receiver (cons c s))))
				   (funcall receiver (cons c nil)))
			       nil)))
			  ((eql exp 'whitespace)
			   (regexp->scanner
			    '((or #\Space #\NewLine) (arbno (or #\Space #\NewLine)))
			    (make-concat-receiver regexp receiver)))
			  ((eql exp 'any)
			   (lambda (c)
			     (if (cdr regexp)
				 (regexp->scanner (cdr regexp) (lambda (s) (funcall receiver (cons c s))))
			       (funcall receiver (cons c nil)))))
			  ((not-exp-p exp)
			   (lambda (c)
			     (if (not (eql c (cadr exp)))
				 (if (cdr regexp)
				     (regexp->scanner (cdr regexp) (lambda (s) (funcall receiver (cons c s))))
				   (funcall receiver (cons c nil)))
			       nil)))
			  ((or-exp-p exp)
			   (let ((scanners (mapcar (lambda (e)
						     (regexp->scanner (cons e nil) #'identity))
						   (cdr exp))))
			     (labels ((make-or-scan
				       (or-scanners)
				       (labels
					((or-scan (c)
						  (let ((scanners (mapcan (lambda (scanner)
									    (if (functionp scanner)
										(funcall scanner c)
									      scanner))
									  or-scanners)))
						    (if (member-if #'functionp scanners)
							(make-or-scan scanners)
						      (if (null scanners)
							  nil
							(funcall receiver ;;todo vector的情况？？
								 (car (sort scanners #'> :key (lambda (s) (if (vectorp s)
													      (length (svref s 0))
													    (length s)))))))))))
					#'or-scan)))
				     (make-or-scan scanners))))
			  ((arbno-exp-p exp)
			   (lambda (c)
			     (let ((r (funcall 
				       (regexp->scanner (cadr exp)
							(lambda (s) (funcall receiver s)))
				       c)))
			       (etypecase r
					  (cons
					   (regexp->scanner regexp (lambda (s) (funcall receiver (append r s)))))
					  (vector;;todo
					   (svref r 1))
					  (function r);;todo
					  (null (if (cdr regexp)
						    #(nil
						      (funcall
						       (regexp->scanner (cdr regexp) receiver)
						       c))
						  nil)))
			       )))
			  ((concat-exp-p exp)
			   (regexp->scanner
			    (cdr exp)
			    (make-concat-receiver regexp receiver)))
			  (t (error "Unknown expression:~A~%" exp))))))
     (regexp->scanner (cadr raa)
		      #'(lambda (char-seq)
			  (list (coerce char-seq 'string) name action))))))

(defun make-string-parser (scanner-spec grammar)
  ;;todo
  )



