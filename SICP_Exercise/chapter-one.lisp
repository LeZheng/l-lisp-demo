;;; exercise 1.1
10 ;; => 10
(+ 5 3 4) ;; => 12
(- 9 1) ;; => 8
(/ 6 2) ;; => 3
(+ (* 2 4) (- 4 6)) ;; => 6
(setf a 3) ;; => 3
(setf b (+ a 1)) ;; => 4
(+ a b (* a b)) ;; => 19
(= a b) ;; => nil
(if (and (> b a) (< b (* a b)))
  b
  a);; => 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (t 25)) ; => 16
(+ 2 (if (> b a) b a)) ;; => 6
(* (cond ((> a b) a)
         ((< a b) b)
         (t -1))
   (+ a 1)) ;; => 16

;;exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) 
   (* 3 (- 6 2) (- 2 7)))

;;exercise 1.3
(defun max3 (a b c)
  (+ (if (> a b) a b)
     (if (> b c) b c)))

;;exercise 1.4
(defun a-plus-abs-b (a b)
  (funcall (if (> b 0) #'+ #'-) a b)) ;; => a 加上 b 的绝对值

;;exercise 1.5 ?? TODO
