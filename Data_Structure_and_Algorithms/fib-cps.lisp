(defun fib (n)
  (labels ((iter (n f)
	     (if (= n 1)
		 (funcall f 0 1)
		 (iter (- n 1)
		       (lambda (a b)
			 (funcall f b (+ a b)))))))
    (iter n (lambda (n1 n0) n0))))

(defun fib (n)
  (labels ((rec (n f)
	     (if (<= n 2)
		 (funcall f 1)
		 (rec (- n 1)
		      (lambda (a)
			(rec (- n 2)
			     (lambda (b)
			       (funcall f (+ a b)))))))))
    (rec n #'identity)))

