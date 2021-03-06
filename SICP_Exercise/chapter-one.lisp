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
  (flet ((p (x) (- (* 3 x) (* 4 x x x))))
    (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0))))))

;a) p 被使用了5次
;b) 增长的阶都是 O(log a)

;;;exercise 1.16
(defun fast-expt-iter (b n &optional (a 1))
  (cond ((= n 0) a)
        ((evenp n) (fast-expt-iter (* b b) (/ n 2) a))
        (t (fast-expt-iter b (1- n) (* a b)))))

;;;exercise 1.17
(defun fast-* (a b)
  (labels ((double (x) (* x 2))
           (halve (x) (/ x 2)))
    (cond ((= b 1) a)
          ((= b 0) 0)
          ((evenp b) (fast-* (double a) (halve b)))
          (t (+ a (fast-* a (1- b)))))))

;;;exercise 1.18
(defun fast-*-2 (a b &optional (product 0))
  (labels ((double (x) (* x 2))
           (halve (x) (/ x 2)))
    (cond ((= b 1) (+ a product))
          ((= b 0) product)
          ((evenp b) (fast-*-2 (double a) (halve b) product))
          (t (fast-*-2 a (1- b) (+ a product))))))

;;;exercise 1.19
(defun fib-19 (n &optional (a 1) (b 0) (p 0) (q 1))
  (cond ((= n 0) b)
        ((evenp n) (fib-19 (/ n 2) 
                           a
                           b
                           (+ (* p p) (* q q)) 
                           (+ (* 2 p q) (* q q))))
        (t (fib-19 (1- n) 
                   (+ (* b q) (* a q) (* a p))
                   (+ (* b p) (* a q))
                   p q))))

;;;exercise 1.20
(defun my-gcd (a b)
  (if (= b 0)
    a
    (my-gcd b (rem a b))))

;;1.正则序
; (my-gcd 206 40)
; (my-gcd 40 (rem 206 40))
; (if (= (rem 206 40) 0) 40 (my-gcd 40 (rem 40 (rem 206 40))))TODO


;;2.应用序
; (my-gcd 206 40)
; (my-gcd 40 6)
; (my-gcd 6 4)
; (my-gcd 4 2)
; (my-gcd 2 0)
; 执行四次 rem 操作

;;; exercise 1.21
(defun prime? (n)
  (= n (smallest-divisor n)))

(defun find-divisor (n test-divisor)
  (cond ((> (expt test-divisor 2) n) n)
        ((= (rem n test-divisor) 0) test-divisor)
        (t (find-divisor n (1+ test-divisor)))))

(defun smallest-divisor (n)
  (find-divisor n 2))
; 199 => 199
; 1999 => 1999
; 19999 => 19999

;;; exercise 1.22
(defun timed-prime-test (n)
  (format t "~%~A" n)
  (start-prime-test n (get-internal-real-time)))

(defun start-prime-test (n start-time)
  (if (prime? n)
    (progn (report-prime (- (get-internal-real-time) start-time)) n)
    nil))

(defun report-prime (elapsed-time)
  (format t " *** ~A" elapsed-time))

(defun search-for-primes (min-value)
  (loop for i from min-value 
        with c = 0 
        when (timed-prime-test i) do (incf c) until (= c 3)))

; 耗时并非是10的平方根，100000和1000000的情况也不是如此，时间正比于计算步数?? TODO

;;; exercise 1.23
(defun next-div (n)
  (if (= n 2)
    3
    (+ n 2)))

(defun find-divisor (n test-div)
  (cond ((> (expt test-div 2) n) n)
        ((= (rem n test-div) 0) test-div)
        (t (find-divisor n (next-div test-div)))))
; 事实情况并没有加快一倍，原因?? TODO

;;; exercise 1.24
(defun expmod (base exp m)
  (cond ((= exp 0) 1)
        ((evenp exp) (rem (expt (expmod base (/ exp 2) m) 2) m))
        (t (rem (* base (expmod base (- exp 1) m)) m))))

(defun fermat-test (n)
  (let ((a (+ 1 (random (- n 1)))))
    (= (expmod a n n) a)))

(defun fast-prime? (n &optional (times 10))
  (cond ((= times 0) t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (t nil)))

(defun timed-prime-test-2 (n)
  (format t "~%~A" n)
  (start-prime-test-2 n (get-internal-real-time)))

(defun start-prime-test-2 (n start-time)
  (if (fast-prime? n)
    (progn (report-prime (- (get-internal-real-time) start-time)) n)
    nil))

;;TODO 验证结果并解答

;;;exercise 1.25 TODO

;;;exercise 1.26 TODO

;;;exercise 1.27 TODO

;;;exercise 1.28 TODO

;;;exercise 1.29 TODO 

;;;exercise 1.30
(defun my-sum (term a next b)
  (labels ((iter (a result)
                 (if (> a b)
                   result
                   (iter (funcall next a) (+ result (funcall term a))))))
    (iter a 0)))

;;;exercise 1.31
(defun my-product (term a next b)
  (labels ((iter (a result)
                 (if (> a b)
                   result
                   (iter (funcall next a) (* result (funcall term a))))))
    (iter a 1)))

(defun my-product-2 (term a next b)
  (if (> a b)
    1
    (* (funcall term a) (my-product-2 term (funcall next a) next b))))

(defun factorial (n)
  (my-product #'identity 1 #'1+ n))

(defun cal-pi ()
  (float (* 4 (my-product 
                (lambda (n) (/ (* n (+ n 2)) (expt (1+ n) 2))) 
                2 
                (lambda (n) (+ n 2)) 
                1000))))

;;exercise 1.32
(defun accumulate-1 (combiner null-value term a next b)
  (if (> a b)
    null-value
    (funcall combiner 
             (funcall term a) 
             (accumulate-1 combiner null-value term (funcall next a) next b))))

(defun acc-sum (term a next b)
  (accumulate-1 #'+ 0 term a next b))

(defun acc-product (term a next b)
  (accumulate-1 #'* 1 term a next b))

(defun accumulate-2 (combiner null-value term a next b)
  (labels ((iter (a result)
                 (if (> a b)
                   result
                   (iter (funcall next a)
                            (funcall combiner result (funcall term a))))))
    (iter a null-value)))

;;;exercise 1.33
(defun filtered-accumulate (combiner null-value predicte term a next b)
  (if (> a b)
    null-value
    (if (funcall predicte (funcall term a))
      (funcall combiner 
               (funcall term a)
               (filtered-accumulate combiner null-value predicte term (funcall next a) next b))
      (filtered-accumulate combiner null-value predicte term (funcall next a) next b))))

(defun acc-prime (a b)
  (filtered-accumulate #'+ 0 #'prime? #'identity a #'1+ b))

;;;exercise 1.34
; 最后出现(2 2) 调用，而2不是一个过程，因此出错

;;;exercise 1.35
(defun fixed-point (f first-guess)
  (labels ((close-enough? (v1 v2)
                          (< (abs (- v1 v2)) 0.00001))
           (try (guess)
                (let ((next (funcall f guess)))
                  (if (close-enough? guess next)
                    next
                    (try next)))))
    (try first-guess)))
;(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0) => 1.6180328

;;;exercise 1.36
(defun fixed-point-2 (f first-guess)
  (labels ((close-enough? (v1 v2)
                          (< (abs (- v1 v2)) 0.00001))
           (try (guess)
                (format t "guess: ~A~%" guess)
                (let ((next (funcall f guess)))
                  (if (close-enough? guess next)
                    next
                    (try next)))))
    (try first-guess)))
;使用平均阻尼前后计算步数大概是 4:1

;;;exercise 1.37
(defun cont-frac (n d k &optional (i 1))
  (if (< i k)
    (/ (funcall n i) (+ (funcall d i) (cont-frac n d k (1+ i))))
    (/ (funcall n i) (funcall d i))))
; 具有4位精度时k为11

(defun cont-frac-2 (n d k)
  (labels ((iter (r i)
                 (if (> i 1)
                   (iter (/ (funcall n i) (+ (funcall d i) r)) (- i 1))
                   r)))
    (iter (/ (funcall n k) (funcall d k)) (- k 1))))

;;;exercise 1.38 TODO 结果不对
(cont-frac-2 
  (lambda (i) 1.0) 
  (lambda (i) 
    (if (< i 3) 
      i 
      (if (= (mod (- i 2) 3) 0)
        (* 2 (+ 1 (/ (- i 2) 3)))
        1))) 
  12)

;;;exercise 1.39 
(defun tan-cf (x k)
  (cont-frac-2 
    (lambda (n) (if (> n 1) (* x x) x))
    (lambda (d) (- (* d 2) 1))
    k))

;;;exercise 1.40
(defparameter dx 0.00001)

(defun deriv (g)
  (lambda (x) (/ (- (funcall g (+ x dx)) (funcall g x))
                 dx)))

(defun newtons-transform (g)
  (lambda (x) (- x (/ (funcall g x) (funcall (deriv g) x)))))

(defun newtons-method (g guess)
  (fixed-point (newtons-transform g) guess))

(defun cubix (a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

;;;exercise 1.41
(defun double (f)
  (lambda (x) (funcall f (funcall f x))))

;(funcall (funcall (double (double #'double)) #'1+) 5) => 21

;;;exercise 1.42
(defun compose (f g)
  (lambda (x) (funcall f (funcall g x))))

;;;exercise 1.43
(defun repeated (f n)
  (if (> n 1)
    (let ((cf f))
      (dotimes (i (- n 1) cf)
        (setf cf (compose cf f))))
    f))

;;;exercise 1.44
(defun smooth (f)
  (lambda (x) 
    (/ (+ (funcall f (- x dx)) 
          (funcall f x) 
          (funcall f (+ x dx))) 
       3)))

;;;exercise 1.45 TODO

;;;exercise 1.46 TODO
