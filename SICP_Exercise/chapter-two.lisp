(defun make-rat (n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(defun numer (x) (car x))

(defun denom (x) (cdr x))

(defun add-rat (x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defun sub-rat (x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defun mul-rat (x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defun div-rat (x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defun equal-rat? (x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(defun print-rat (x)
  (format t "~A/~A" (numer x) (denom x)))

;;;exercice 2.1
(defun make-rat (n d)
  (let ((g (gcd n d)))
    (cond 
      ((and (< n 0) (< d 0)) (cons (/ (- n) g) (/ (- d) g)))
      ((and (< n 0) (> d 0)) (cons (/ n g) (/ d g)))
      ((and (> n 0) (< d 0)) (cons (/ (- n) g) (/ (- d) g)))
      (t (cons (/ n g) (/ d g))))))

;;;exercice 2.2
(defun make-point (x y)
  (cons x y))

(defun x-point (point)
  (car point))

(defun y-point (point)
  (cdr point))

(defun make-segment (start end)
  (cons start end))

(defun start-segment (segment)
  (car segment))

(defun end-segment (segment)
  (cdr segment))

(defun midpoint-segment (segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))
    (make-point (/ (+ (x-point start) (x-point end)) 2)
                (/ (+ (y-point start) (y-point end)) 2))))

(defun print-point (point)
  (format t "(~A,~A)~%" (x-point point) (y-point point)))

;;;exercice 2.3
(defun make-rect (left-top-point right-bottom-point)
  (cons left-top-point right-bottom-point))

(defun left-top-rect (rect)
  (car rect))

(defun right-bottom-rect (rect)
  (cdr rect))

(defun width-rect (rect)
  (- (x-point (right-bottom-rect rect)) (x-point (left-top-rect rect))))

(defun height-rect (rect)
  (- (y-point (right-bottom-rect rect)) (y-point (left-top-rect rect))))

(defun perimeter-rect (rect)
  (+ (* 2 (width-rect rect))
     (* 2 (height-rect rect))))

(defun area-rect (rect)
  (* (width-rect rect)
     (height-rect rect)))

(defun make-rect (left-top-point width height)
  (cons left-top-point (cons width height)))

(defun width-rect (rect)
  (car (cdr rect)))

(defun height-rect (rect)
  (cdr (cdr rect)))

;;;exercice 2.4
(defun cons-1 (x y)
  (lambda (m) (funcall m x y)))

(defun car-1 (z)
  (funcall z (lambda (p q) p)))

(defun cdr-1 (z)
  (funcall z (lambda (p q) q)))

;;;exercice 2.5
(defun cons-2 (x y)
  (* (expt 2 x) (expt 3 y)))

(defun car-2 (z)
  (if (= 0 (rem z 2))
    (1+ (car-2 (/ z 2)))
    0))

(defun cdr-2 (z)
  (if (= 0 (rem z 3))
    (1+ (car-2 (/ z 3)))
    0))

;;;exercice 2.6
(defun zero ()
  (lambda (f) (lambda (x) x)))

(defun add-1 (n)
  (lambda (f) (lambda (x) (funcall f (funcall (funcall n f) x)))))

(defun one ()
  (lambda (f) (lambda (x) (funcall f x))))

(defun two ()
  (lambda (f) (lambda (x) (funcall f (funcall f x)))))

(defun add (x y)
  (lambda (f) (lambda (x) (funcall m f (funcall n f x)))))

;;;exercice 2.7
(defun add-interval (x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defun mul-interval (x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(defun div-interval (x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(defun make-interval (x y)
  (cons x y))

(defun upper-bound (x)
  (cdr x))

(defun lower-bound (x)
  (car x))

;;;exercice 2.8
(defun sub-interval (x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y1))))

;;;exercice 2.9 TODO

;;;exercice 2.10
(defun div-interval (x y)
  (if (and (>= (upper-bound y) 0) (<= (lower-bound y) 0))
    (error "y's upper-bound and lower-bound contains 0")
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))

;;;exercice 2.11
(defun mul-interval (x y)
  (cond
    ((and (plusp (lower-bound x)) 
          (plusp (lower-bound y))) 
     (make-interval (* (lower-bound x) (lower-bound y)
                       (upper-bound x) (upper-bound y))))
    ((and (minusp (upper-bound x)) 
          (minusp (upper-bound y)))
     (make-interval (* (lower-bound x) (lower-bound y)
                       (upper-bound x) (upper-bound y))))
    ((and (plusp (lower-bound x))
          (minusp (upper-bound y)))
     (make-interval (* (upper-bound x) (lower-bound y))
                    (* (lower-bound x) (upper-bound y))))
    ((and (minusp (upper-bound x))
          (plusp (lower-bound y)))
     (make-interval (* (lower-bound x) (upper-bound y))
                    (* (upper-bound x) (lower-bound y))))
    ;;;TODO 剩余5种情况
    ))


;;;exercice 2,12
(defun make-center-percent (c p)
  (make-interval (- c (* c p))
                 (+ c (* c p))))

(defun percent (i)
  (/ (/ (- (upper-bound i) (lower-bound i)) 2)
     (center i)))

(defun center (i)
  (/ (+ (upper-bound i) (lower-bound i)) 2))

;;;exercice 2.13 TODO

;;;exercice 2.14 TODO

;;;exercice 2.15 TODO

;;;exercice 2.16 TODO


;;;; 2.2
(defun list-ref (l n)
  (if (= n 0)
    (car l)
    (list-ref (cdr l) (- n 1))))

(defun my-length (l)
  (labels ((iter (l c)
                 (if (null l)
                   c
                   (iter (cdr l) (1+ c)))))
    (iter l 0)))

(defun my-append (l1 l2)
  (if (null l1)
    l2
    (cons (car l1) (my-append (cdr l1) l2))))

;;;exercice 2.17
(defun last-pair (l)
  (if (null (cdr l))
    (car l)
    (last-pair (cdr l))))

;;;exercice 2.18
(defun my-reverse (l)
  (labels ((iter (l r)
                 (if (null l)
                   r
                   (iter (cdr l) (cons (car l) r)))))
    (iter l nil)))

;;;exercice 2.19
(defconstant us-coins (list 50 25 10 5 1))
(defconstant uk-coins (list 100 50 20 10 5 2 1 0.5))

(defun cc (amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (t (+ (cc amount (exception-first-denomination coin-values))
              (cc (- amount (first-denomination coin-values))
                  coin-values)))))

(defun first-denomination (coins)
  (car coins))

(defun exception-first-denomination (coins)
  (cdr coins))

(defun no-more? (coins)
  (null coins))
; 调换顺序不影响，因为这个程序在枚举所有币种的组合

;;;exercice 2.20
(defun same-parity (&rest nums)
  (cond ((null nums) nil)
        (t (let ((first-odd? (oddp (first nums))) (result '()))
             (dolist (num (reverse nums) result);TODO dolist
               (if (equal first-odd? (oddp num))
                 (push num result)))))))

;;;exercice 2,21
(defun square-list (items)
  (if (null items)
    nil
    (cons (expt (car items) 2) (square-list (cdr items)))))

(defun square-list (items)
  (mapcar (lambda (x) (expt x 2)) items))

;;;exercice 2.22
; cons 的构造决定了

;;;exercice 2.23
(defun for-each (proc items)
  (when (not (null items))
    (funcall proc (car items))
    (for-each proc (cdr items))))
