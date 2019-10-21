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

;;;exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) 
   (* 3 (- 6 2) (- 2 7)))

;;;exercise 1.3
(defun max3 (a b c)
  (+ (if (> a b) a b)
     (if (> b c) b c)))

;;;exercise 1.4
(defun a-plus-abs-b (a b)
  (funcall (if (> b 0) #'+ #'-) a b)) ;; => a 加上 b 的绝对值

;;;exercise 1.5 ?? TODO

;;;example 1.1.7
(defun good-enough? (guess x)
  (< (abs (- (* guess guess) x)) 0.0001))

(defun sqrt-iter (guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (/ (+ guess (/ x guess)) 2)
               x)))

(defun my-sqrt (x)
  (sqrt-iter 1 x))

;;;exercise 1.6
;;cond 是应用序求值的情况下,new-if调用展开前then和else子句都会先求值

;;;exercise 1.7 TODO
(defun sqrt-iter-x (guess x &optional (last-guess 0))
  (if (< (abs (- guess last-guess)) 0.001)
    guess
    (sqrt-iter-x (float (/ (+ guess (/ x guess)) 2))
                 x
                 guess)))
(defun my-sqrt-x (x)
  (sqrt-iter-x 1 x))

;;;exercise 1.8
(defun good-enough?3 (guess x)
  (< (abs (- (* guess guess guess) x)) 0.001))

(defun cube-iter (guess x)
  (if (good-enough?3 guess x)
    guess
    (cube-iter (/ (+ (/ x (* guess guess)) (* 2 guess)) 3)
               x)))

(defun my-cube (x)
  (cube-iter 1 x))

;;;exercise 1.9
;;第一个加法是递归，第二个加法是迭代

;;;exercise 1.10
(defun aa (x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (t (aa (- x 1)
               (aa x (- y 1))))))
(aa 1 10);; => 1024
(aa 2 4) ;; => 65535
(aa 3 3) ;; => 65535
;;(f n) => 2n
;;(g n) => 2^(n^1)
;;(h n) => 2^(n^2)

;;;exercise 1.11
(defun fn-rec (n)
  (if (< n 3)
    n
    (+ (fn-rec (- n 1)) (* 2 (fn-rec (- n 2))) (* 3 (fn-rec (- n 3))))))
(defun fn-iter (n &optional (x 0) (y 1) (z 2))
  (cond ((< n 3) n)
        ((= n 3) (+ (* 3 x) (* 2 y) z))
        (t (fn-iter (- n 1) y z (+ (* 3 x) (* 2 y) z)))))

;;;exercise 1.12
(defun pascal-triangle (x y)
  (cond ((= x 1) 1)
        ((= x y) 1)
        (t (+ (pascal-triangle (- x 1) (- y 1)) (pascal-triangle x (- y 1))))))

;;;exercise 1.13 TODO 证明
(defun fib (n &optional (x 0) (y 1))
  (cond ((< n 2) n)
        ((= n 2) (+ x y))
        (t (fib (- n 1) y (+ x y)))))


