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

