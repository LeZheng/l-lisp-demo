;;apply
(apply '+ '(1 2))
(apply #'+ '(1 2))
(apply (lambda (a b) (+ a b)) (list 1 2))
;;defun
(defun fun1 (a b &optional c)
  "this is fun1, contains optional."
  (list a b c))

(defun fun2 (a b &rest r)
  "this is fun2, contains rest."
  (list a b r))

(defun fun3 (a b &key name (age 0))
  (declare (number age))
  (list a b name age)); -> (fun3 1 2 :name "asdf" :age 2)

(defun fun4 (a b c)
  (check-type a integer)
  (locally (declare (integer a))
	   (list (1+ a) b c)))
;;fdefinition
(fdefinition '+)
(setf (fdefinition 'fun5) (lambda (a) (list a 1)))
;;fboundp
(fboundp 'fun5)
;;fmakunbound
(fmakunbound 'fun5)
;;flet
(flet ((add (a &rest r)
	    "flet add"
	    (apply '+ (cons a r))))
      (add 1 2 3))
;;labels
(labels ((add (numbers)
	      "label add"
	      (if (null numbers)
		  0
		(+ (car numbers) (add (cdr numbers))))))
	(print (documentation #'add 'function))
	(add (list 1 2 3)))
;;macrolet
(defun foo (x flag)
  (macrolet ((fudge (z)
		    `(if flag (* ,z x) ,z)))
	    (fudge 2)))
;;funcall
(funcall #'+ 1 2 3 4)
(flet ((cons (x y) (list 'fcons x y)))
      (funcall #'cons 1 2))
;;function
(flet ((cons (x y) (list 'fcons x y)))
      (function cons))
;;function-lambda-expression
(function-lambda-expression #'fun1)
(funcall (eval (function-lambda-expression #'fun2)) 1 2 3)
;;functionp
(functionp #'+)
;;compiled-function-p
(compiled-function-p #'+)
;;defconstant
(defconstant CONST-VAR 123 "this is a const variable")
;;defparameter
(defparameter *param-var* "pvarx" "This is a parameter")
;;defvar
(defvar *var-var* 123 "This is a var")
;;destructuring-bind
(destructuring-bind ((a) one two three)
		    (list '(1000) 1 2 3)
		    (list a one two three))
;;let let*
(let ((i 10)
      (j 11))
  (list i j))

(let* ((i 10)
       (j (1+ i)))
  (list i j))
;;progv
(progv '(a b c) '(1 2 3)
       (list a b c))
;;setq psetq
(let ((a 1) (b 1) (c 1))
  (setq a (1+ a) b (1+ a) c (1+ b))
  (list a b c))
(let ((a 1) (b 1) (c 1))
  (psetq a (1+ a) b (1+ a) c (1+ b))
  (list a b c))
;;block
(block b1
       (print 1)
       (print 2)
       (return-from b1 "return b1"))
;;catch throw
(catch 'dummy-tag 1 2 (throw 'dummy-tag 3) 4)
(catch 'dummy-tag 1 2 (throw 'dummy-tagx 3) 4)
(catch 'c
  (flet ((c1 () (throw 'c 1)))
    (catch 'c (c1) (print 'unreachable))
    2))
;;go tagbody
(tagbody
 (setf f 1)
 t1 (print "1")
 (incf f)
 (print "2")
 t2 (print "3")
 (if (> f 5)
     (go end)
   (go t1))
 end (print "end"))
;;return-from return
(block b1 (return-from b1 1))
(block nil (return 1))
;;unwind-protect
(block nil
       (unwind-protect
	   (return 1)
	 (print "asdf")))
;;eq
(eq 3 3.0)
(eql 3 3.0)
(eql "asdf" "asdf")
(equal "asdf" "asdf")
(equal 3 3.0)
(equalp 3 3.0)
;;complement constantly
(funcall (complement #'evenp) 1)
(funcall (constantly 1) 1 2 3 4)
;;every some notevery notany
(every #'evenp '(2 4 6))
(some #'evenp '(1 2 3))
(notevery #'evenp '(1 2 3))
(notany #'evenp '(1 2 3))
;;and
(and (atom p) (numberp p))
;;or
(or (evenp i) (oddp i))
;;cond
(let ((i 0))
  (cond
   ((evenp i) (print i) "even")
   ((oddp i) "odd")
   ((zerop i) "0")
   (t "else")))
;;if
(if (evenp i)
    "even"
  "odd")
;;when unless
(when (oddp i)
  (print "odd"))
(unless (evenp i)
  (print "odd"))
;;case ccase ecase
(case i
      ((1 2) "1-2")
      ((3 4) "3-4")
      (5 "5")
      (t "else"))
;;typecase ctypecase etypecase
(defun what-is-it (x)
  (format t "~&~S is ~A.~%"
          x (typecase x
              (float "a float")
              (null "a symbol, boolean false, or the empty list")
              (list "a list")
              (t (format nil "a(n) ~(~A~)" (type-of x))))))
;;multiple-value-bind
(multiple-value-bind (f r) (floor 130 11)
		     (list f r))
;;multiple-value-call
(multiple-value-call #'+ 1 (values 2 3))
(multiple-value-call #'list 1 (values 2 3 4) 0)
;;multiple-value-list
(multiple-value-list (floor 9 2))
;;multiple-value-prog1
(multiple-value-prog1 (values 1 2 3)
		      (print 1)
		      "asdf")
;;multiple-value-setq
(multiple-value-setq (a b) (values 1 2))
;;values
(values)
(values 1 2 3)
;;values-list
(values-list '(1 2 3))
;;nth-value
(nth-value  3 (values 1 2 3 4))
;;progn prog prog*
(progn
  (print 1)
  (print 2))
(prog
  (print 1)
  (print 2))

(prog ((a 1) (b 2))
       (print a)
       (print b))

(prog ((a 1) (b 2))
       t1 (print a)
       t2 (print b)
       (incf a)
       (cond
	((> a 3) a)
	(t (go t1))))
;;define-modify-macro
(define-modify-macro my-cons (&rest args) cons "my cons")
;;shiftf
(let ((a 1) (b 2) (c 3))
  (shiftf a b c 99)
  (list a b c))
;;rotatef
(let ((a 1) (b 2) (c 3))
  (rotatef a b c)
  (list a b c))
