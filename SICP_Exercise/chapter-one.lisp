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

;;;exercise 1.5
;如果是应用序求值，test调用前对 (p) 求值会进入死循环
;如果是正则序求值，则返回0，这个过程中 (p) 没有被求值

;;;example 1.1.7
(defun good-enough? (guess x)
  (< (abs (- (* guess guess) x)) 0.0001))

(defun sqrt-iter (guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (float (/ (+ guess (/ x guess)) 2))
               x)))

(defun my-sqrt (x)
  (sqrt-iter 1 x))

;;;exercise 1.6
;;cond 是应用序求值的情况下,new-if调用展开前then和else子句都会先求值

;;;exercise 1.7
(defun sqrt-iter-x (guess x &optional (last-guess 0))
  (format t "~A~%" guess)
  (if (< (abs (- guess last-guess)) 0.001)
    guess
    (sqrt-iter-x (float (/ (+ guess (/ x guess)) 2))
                 x
                 guess)))
(defun my-sqrt-x (x)
  (sqrt-iter-x 1 x))
;过小的数不能检测，例如：(my-sqrt 0.00000000000001)
;过大的数不能检测，例如: (my-sqrt 1000000001)
;my-sqrt-x 可以，TODO 待检验

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
; (+ 4 5)
; (inc (+ 3 5))
; (inc (inc (+ 2 5)))
; (inc (inc (inc (+ 1 5))))
; (inc (inc (inc (inc (+ 0 5)))))
; (inc (inc (inc (inc 5))))
; ...

; (+ 4 5)
; (+ 3 6)
; (+ 2 7)
; (+ 1 8)
; (+ 0 9)
; 9
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

;;;exercise 1.14
(defun count-change (amount)
  (labels ((cc (rest-amount kinds-of-coins)
               (format t "cc ~A ~A~%" rest-amount kinds-of-coins)
               (cond ((= rest-amount 0) 1)
                     ((or (< rest-amount 0) (= kinds-of-coins 0)) 0)
                     (t (+ (cc rest-amount (1- kinds-of-coins))
                           (cc 
                             (- rest-amount (case kinds-of-coins 
                                         (1 1)
                                         (2 5)
                                         (3 10)
                                         (4 25)
                                         (t 50)))
                             kinds-of-coins))))))
    (cc amount 5)))
;(cc 11 5)
;(cc 11 4) (cc -39 5)
;(cc 11 3) (cc -14 4) 0
;(cc 11 2) (cc 1 3) 0
;(cc 11 1) (cc 6 2) (cc 1 2) (cc -9 3)
;(cc 11 0) (cc 10 1) (cc 6 1) (cc 1 2) (cc 1 1) (cc -4 2) 0
;0 (cc 10 0) (cc 9 1) (cc 6 0) (cc 5 1) (cc 1 1) (cc -4 2) (cc 1 0) (cc 0 1) 0
;0 (cc 9 0) (cc 8 1) 0 (cc 5 0) (cc 4 1) (cc 1 0) (cc 0 1) 0 0 1
;0 (cc 8 0) (cc 7 1) 0 (cc 4 0) (cc 3 1) 0 1 1
;0 (cc 7 0) (cc 6 1) 0 (cc 3 0) (cc 2 1) 1 1
;0 (cc 6 0) (cc 5 1) 0 (cc 2 0) (cc 1 1) 1 1
;0 (cc 5 0) (cc 4 1) 0 (cc 1 0) (cc 0 1) 1 1
;0 (cc 4 0) (cc 3 1) 0 1 1 1
;0 (cc 3 0) (cc 2 1) 1 1 1
;0 (cc 2 0) (cc 1 1) 1 1 1
;0 (cc 1 0) (cc 0 1) 1 1 1
;0 1 1 1 1
;4

;空间 TODO
;增长的阶 TODO

;;;exercise 1.15
(defun sine (angle)
  (flet ((p (x) (print "p...") (- (* 3 x) (* 4 x x x))))
    (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0))))))

;a) p 被使用了5次
;b) 增长的阶都是 O(log a)
