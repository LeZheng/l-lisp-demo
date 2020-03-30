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
(defvar us-coins (list 50 25 10 5 1))
(defvar uk-coins (list 100 50 20 10 5 2 1 0.5))

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

;;;; 2.2.2
(defun count-leaves (tree)
  (cond 
    ((null tree) 0)
    ((atom tree) 1)
    (t (+ (count-leaves (car tree)) (count-leaves (cdr tree))))))

;;;exercice 2.24 
;略

;;;exercice 2.25
(car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))
(car (car '((7))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))

;;;exercice 2.26
(setf x (list 1 2 3))
(setf y (list 4 5 6))
(append x y) ; => (1 2 3 4 5 6)
(cons x y) ; => ((1 2 3) 4 5 6)
(list x y) ; => ((1 2 3) (4 5 6))

;;;exercice 2.27
(defun deep-reverse (tree)
  (labels ((iter (l r)
                 (cond 
                   ((null l) r)
                   ((atom l) l)
                   (t (iter (cdr l) (cons (iter (car l) nil) r))))))
    (iter tree nil)))

;;;exercice 2.28
(defun fringe (tree)
  (cond
    ((null tree) nil)
    ((atom tree) (list tree))
    (t (labels ((iter (l r)
                      (cond
                        ((null l) r)
                        (t (iter (cdr l) (append r (fringe (car l))))))))
         (iter tree nil)))))

;;;exercice 2.29
(defun make-mobile (left right)
  (list left right))

(defun make-branch (length struct)
  (list length struct))

(defun left-branch (mobile)
  (first mobile))

(defun right-branch (mobile)
  (second mobile))

(defun branch-length (branch)
  (first branch))

(defun branch-struct (branch)
  (second branch))

(defun total-weight (mobile)
  (labels ((branch-weight (branch)
                          (if (numberp (branch-struct branch))
                            (branch-struct branch)
                            (total-weight (branch-struct branch)))))
    (+ (branch-weight (left-branch mobile))
       (branch-weight (right-branch mobile)))))

(defun balancep (mobile)
  (and
    (= (* (branch-length (left-branch mobile)) (total-weight (left-branch mobile)))
       (* (branch-length (right-branch mobile)) (total-weight (right-branch mobile))))
    (or (numberp (branch-struct (left-branch mobile)))
        (balancep (branch-struct (left-branch mobile))))
    (or (numberp (branch-struct (right-branch mobile)))
        (balancep (branch-struct (right-branch))))))

(defun make-mobile (left right)
  (cons left right))

(defun make-branch (length struct)
  (cons length struct))

(defun left-branch (mobile)
  (car mobile))

(defun right-branch (mobile)
  (cdr mobile))

(defun branch-length (branch)
  (car branch))

(defun branch-struct (branch)
  (cdr branch))

;;;exercice 2.30
(defun square-tree (tree)
  (cond
    ((null tree) nil)
    ((atom tree) (expt tree 2))
    (t (cons (square-tree (car tree))
             (square-tree (cdr tree))))))

(defun square-tree (tree)
  (mapcar (lambda (sub-tree)
            (if (atom sub-tree)
              (expt sub-tree 2)
              (square-tree sub-tree)))
          tree))

;;;exercice 2.31
(defun tree-map (tree operator)
  (cond
    ((null tree) nil)
    ((atom tree) (funcall operator tree))
    (t (cons (tree-map (car tree) operator)
             (tree-map (cdr tree) operator)))))

;;;exercice 2.32
(defun subsets (s)
  (if (null s)
    (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest (mapcar (lambda (x) (cons (car s) x)) rest)))))

;;;; 2.2.3
(defun filter (predicate seq)
  (cond 
    ((null seq) nil)
    ((funcall predicate (car seq)) (cons (car seq) (filter predicate (cdr seq))))
    (t (filter predicate (cdr seq)))))

(defun accumulate (op initial sequence)
  (if (null sequence)
    initial
    (funcall op (car sequence) (accumulate op initial (cdr sequence)))))

;;;exercice 2.33
(defun my-map (p seq)
  (accumulate (lambda (x y) (cons (funcall p x) y)) nil seq))

(defun my-append (seq1 seq2)
  (accumulate #'cons seq2 seq1))

(defun my-length (seq)
  (accumulate (lambda (x y) (1+ y))  0 seq))

;;;exercice 2.34
(defun horner-eval (x seq)
  (accumulate (lambda (this-coeff higher-terms) (+ (* x higher-terms) this-coeff))
              0
              seq))

;;;exercice 2.35
(defun count-leaves (tree)
  (accumulate 
    (lambda (x y) (+ x y)) 
    0 
    (my-map (lambda (x) 
              (if (atom x)
                1
                (count-leaves x))) 
            tree)))

;;;exercice 2.36
(defun accumulate-n (op initial seqs)
  (if (null (car seqs))
    nil
    (cons (accumulate op initial (my-map #'car seqs))
          (accumulate-n op initial (my-map #'cdr seqs)))))

;;;exercice 2.37 TODO 未消化
(defun dot-product (v w)
  (accumulate #'+ 0 (my-map #'* v w)))

(defun matrix-*-vector (m v)
  (my-map (lambda (col) (dot-product col v)) m))

(defun transpose (m)
  (accumulate-n #'cons '() m))

(defun matrix-*-matrix (m n)
  (let ((trans-n (transpose n)))
    (my-map (lambda (col-of-m)
              (matrix-*-vector trans-n col-of-m))
            m)))

;;;exercice 2.38
(defun fold-left (op initial seq)
  (labels ((iter (result rest)
                 (if (null rest)
                   result
                   (iter (funcall op result (car rest))
                         (cdr rest)))))
    (iter initial seq)))

(defun fold-right (op initial sequence)
  (if (null sequence)
    initial
    (funcall op (car sequence) (fold-right op initial (cdr sequence)))))

(fold-right #'/ 1 (list 1 2 3)) ;=> 3/2

(fold-left #'/ 1 (list 1 2 3))  ;=> 1/6

(fold-right #'list nil (list 1 2 3)) ;=>(1 (2 (3 1)))

(fold-left #'list nil (list 1 2 3))  ;=>(((1 1) 2) 3)
;;两个函数要产生一样的结果，那么op得符合结合率

;;;exercice 2.39
(defun my-reverse-1 (seq)
  (fold-right (lambda (x y) (append y (list x))) nil seq))

(defun my-reverse-2 (seq)
  (fold-left (lambda (x y) (cons y x)) nil seq))

;;;;嵌套映射
(defun enumerate-interval (i n)
  (if (> i n)
    nil
    (cons i (enumerate-interval (1+ i) n))))

(defun flatmap (proc seq)
  (accumulate #'append nil (my-map proc seq)))

(load "chapter-one.lisp")
(defun prime-sum? (pair)
  (prime? (+ (car pair) (cadr pair))))

(defun make-pair-sum (pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(defun prime-pair-sum (n)
  (my-map #'make-pair-sum
          (filter #'prime-sum?
                  (flatmap (lambda (i)
                             (my-map (lambda (j) (list i j))
                                  (enumerate-interval 1 (- i 1))))
                           (enumerate-interval 1 n)))))

(defun permutations (s)
  (if (null s)
    (list nil)
    (flatmap (lambda (x)
               (my-map (lambda (p) (cons x p)) (permutations (remove x s))))
             s)))

;;;exercice 2.40
(defun unique-pairs (n)
  (flatmap (lambda (x) 
             (my-map (lambda (i) (list x i))
                     (enumerate-interval 1 (- x 1))))
           (enumerate-interval 1 n)))

(defun prime-pair-sum (n)
  (my-map #'make-pair-sum
          (filter #'prime-sum?
                  (unique-pairs n))))

;;;exercice 2.41
(defun unique-triple (n)
  (flatmap (lambda (x)
             (my-map (lambda (i) (cons x i))
                     (unique-pairs (- x 1))))
           (enumerate-interval 1 n)))

(defun enum-3-tuple (n s &optional (index 0))
  (filter (lambda (a) (= s (+ (first a) (second a) (third a))))
          (unique-triple n)))

;;;exercice 2.42
(defvar empty-board '())

(defun adjoin-position (new-row k rest-of-queens)
  (if (null rest-of-queens)
    (list new-row)
    (append rest-of-queens (list new-row))))

(defun safe? (k positions)
  (let ((x (elt positions (1- k))))
    (dotimes (i (length positions))
      (if (and (/= i (1- k)) (equal x (elt positions i)))
        (return-from safe? nil)))
    t))

(defun queens (board-size)
  (labels ((queen-cols (k)
                       (if (= k 0)
                         (list empty-board)
                         (filter
                           (lambda (pos) (safe? k pos))
                           (flatmap 
                             (lambda (rest-of-queens)
                               (my-map (lambda (new-row)
                                         (adjoin-position new-row k rest-of-queens))
                                       (enumerate-interval 1 board-size)))
                             (queen-cols (- k 1)))))))
    (queen-cols board-size)))

;;;exercice 2.43
(defun queens (board-size)
  (labels ((queen-cols (k)
                       (if (= k 0)
                         (list empty-board)
                         (filter
                           (lambda (pos) (safe? k pos))
                           (flatmap
                             (lambda (new-row)
                               (format t "~A~%" new-row)
                               (my-map (lambda (rest-of-queens)
                                         (adjoin-position new-row k rest-of-queens))
                                       (queen-cols (- k 1))))
                             (enumerate-interval 1 board-size))))))
    (queen-cols board-size)))


;;;;example 2.2.4
(defun transform-painter (painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (funcall painter 
                 (make-frame new-origin
                             (sub-vert (funcall m corner1) new-origin)
                             (sub-vert (funcall m corner2) new origin)))))))

(defun beside (p1 p2)
  (let ((split-point (make-vert 0.5 0.0)))
    (let ((paint-left (transform-painter p1 (make-vert 0.0 0.0) split-point (make-vert 0.0 1.0)))
          (paint-right (transform-painter p2 split-point (make-vert 1.0 0.0) (make-vert 0.5 1.0))))
      (lambda (frame)
        (funcall paint-left frame)
        (funcall paint-right frame)))))

(defun below (p1 p2))   ;not implement

(defun flip-vert (painter)
  (transform-painter painter
                     (make-vert 0.0 1.0)
                     (make-vert 1.0 1.0)
                     (make-vert 0.0 0.0)))
(defun flip-horiz (painter)
  (transform-painter painter
                     (make-vert 1.0 0.0)
                     (make-vert 0.0 0.0)
                     (make-vert 1.0 1.0)))

;;;exercice 2.44
(defun right-split (painter n)
  (if (= n 0)
    painter
    (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))))

(defun up-split (painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))

(defun corner-split (painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
          (right (right-split painter (- n 1))))
      (let ((top-left (beside up up))
            (bottom-right (below right right))
            (corner (corner-split painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner))))))

(defun square-of-four (tl tr bl br)
  (lambda (painter)
    (let ((top (beside (funcall tl painter) (funcall tr painter)))
          (bottom (beside (funcall bl painter) (funcall br painter))))
      (below top bottom))))

(defun flipped-pairs (painter)
  (let ((combine4 (square-of-four #'identity #'flip-vert
                                  #'identity #'flip-vert)))
    (funcall combine4 painter)))

(defun rotate180 (painter)
  (flip-vert (flip-horiz painter)))

(defun squire-limit (painter n)
  (let ((combine4 (square-of-four #'flip-horiz #'identity
                                  #'rotate180 #'flip-vert)))
    (funcall combine4 (corner-split painter n))))

;;;exercice 2.45
(defun split (split-op combine-op)
  (labels ((new-split (painter n)
                      (if (= n 0)
                        painter
                        (let ((smaller (new-split painter (- n 1))))
                          (combine-op painter (split-op smaller smaller))))))))

(defun right-split ()
  (split #'beside #'below))

(defun up-split ()
  (split #'below #'beside))

;;;exercice 2.46
(defun frame-coord-map (frame)
  (lambda (v)
    (add-vert
      (origin-frame frame)
      (add-vert (scale-vert (xcor-vert v)
                            (edge1-frame frame))
                (scale-vert (ycor-vert v)
                            (edge2-frame frame))))))

(defun make-vert (x y)
  (cons x y))

(defun xcor-vert (v)
  (car v))

(defun ycor-vert (v)
  (cdr v))

(defun add-vert (v1 v2)
  (make-vert (+ (xcor-vert v1) (xcor-vert v2))
             (+ (ycor-vert v1) (ycor-vert v2))))

(defun sub-vert (v1 v2)
  (make-vert (- (xcor-vert v1) (xcor-vert v2))
             (- (ycor-vert v1) (ycor-vert v2))))

(defun scale-vert (v s)
  (make-vert (* s (xcor-vert v)) (* (ycor-vert v))))

;;;exercice 2.47
(defun make-frame (origin edge1 edge2)
  (list origin edge1 edge2))

(defun origin-frame (f)
  (first f))

(defun origin-frame (f)
  (second f))

(defun origin-frame (f)
  (third f))

(defun make-frame (origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(defun origin-frame (f)
  (car f))

(defun edge1-frame (f)
  (cadr f))

(defun edge2-frame (f)
  (caddr f))

;;;exercice 2.48
(defun make-segment (start-v end-v)
  (cons start-v end-v))

(defun start-segment (s)
  (car s))

(defun end-segment (s)
  (cdr s))

;;;exercice 2.49
(defun draw-line ())    ;not implement
(defun segment->painter (segment-list)
  (lambda (frame)
    (mapcar
      (lambda (segment)
        (draw-line
          (funcall (frame-coord-map frame) (start-segment segment))
          (funcall (frame-coord-map frame) (end-segment segment))))
      segment-list)))

(defun draw-frame-painter ()
  (lambda (frame)
    (let ((segment-list 
            (list 
              (make-segment (make-vert 0 0) 
                            (sub-vert (edge1-frame frame) (origin-frame frame)))
              (make-segment (make-vert 0 0)
                            (sub-vert (edge2-frame frame) (origin-frame frame)))
              (make-segment (sub-vert (edge1-frame frame) (origin-frame frame))
                            (sub-vert 
                              (add-vert (edge1-frame frame) (edge2-frame frame)) 
                              (origin-frame frame)))
              (make-segment (sub-vert (edge2-frame frame) (origin-frame frame))
                            (sub-vert 
                              (add-vert (edge1-frame frame) (edge2-frame frame))
                              (origin-frame frame))))))
      (funcall (segment->painter segment-list) frame))))

(defun draw-opposite-angle-painter ()
  (lambda (frame)
    (let ((segment-list 
            (list (make-segment (make-vert 0 0)
                                (sub-vert 
                                  (add-vert (edge1-frame frame) (edge2-frame frame))
                                  (origin-frame frame)))
                  (make-segment (edge1-frame frame)
                                (edge2-frame frame)))))
      (funcall (segment->painter segment-list) frame))))

(defun draw-middle-point-painter ()
  (lambda (frame)
    (let ((segment-list
            (list (make-segment (scale-vert (sub-vert (edge1-frame frame) (origin-frame frame)) 0.5)
                                (scale-vert (sub-vert (edge2-frame frame) (origin-frame frame)) 0.5))
                  (make-segment (scale-vert (sub-vert (edge1-frame frame) (origin-frame frame)) 0.5)
                                (sub-vert (add-vert (edge1-frame frame) 
                                                    (scale-vert (sub-vert (edge2-frame frame)) 0.5)) 
                                          (origin-frame frame)))
                  (make-segment (scale-vert (sub-vert (edge2-frame frame) (origin-frame frame)) 0.5)
                                (sub-vert (add-vert (edge2-frame frame)
                                                    (scale-vert (sub-vert (edge1-frame frame)) 0.5))
                                          (origin-frame frame)))
                  (make-segment (sub-vert (add-vert (edge1-frame frame) 
                                                    (scale-vert (sub-vert (edge2-frame frame)) 0.5)) 
                                          (origin-frame frame))
                                (sub-vert (add-vert (edge2-frame frame)
                                                    (scale-vert (sub-vert (edge1-frame frame)) 0.5))
                                          (origin-frame frame))))))
      (funcall (segment->painter segment-list) frame))))

(defun draw-wave-painter ()
  );not implement

;;;exercice 2.50
;flip-horiz 见上方
(defun rotate180 (painter)
  (transform-painter painter
                     (make-vert 1.0 1.0)
                     (make-vert 0.0 1.0)
                     (make-vert 1.0 0.0)))

(defun rotate270 (painter)
  (transform-painter painter
                     (make-vert 0.0 1.0)
                     (make-vert 0.0 0.0)
                     (make-vert 1.0 1.0)))

;;;exercice 2.51
(defun below (p1 p2)
  (let ((split-point (make-vert 0.0 0.5)))
    (let ((paint-bottom (transform-painter p1 (make-vert 0.0 0.0) (make-vert 1.0 0.0) split-point))
          (paint-top (transform-painter p2 split-point (make-vert 1.0 0.5) (make-vert 0.0 1.0))))
      (lambda (frame)
        (funcall paint-top frame)
        (funcall paint-bottom frame)))))

(defun rotate90 (painter)
  (transform-painter painter
                     (make-vert 1.0 0.0)
                     (make-vert 1.0 1.0)
                     (make-vert 0.0 0.0)))

(defun below (p1 p2)
  (rotate90 (beside p1 p2)))

;;;exercice 2.52
;TODO

;;;exercice 2.53
(defun memq (s symbols)
  (cond
    ((null symbols) nil)
    ((equal s (car symbols)) symbols)
    (t (memq s (cdr symbols)))))

(list 'a 'b 'c) ;=> (A B C)
(list (list 'george)) ;=> ((george))
(cdr '((x1 x2) (y1 y2))) ;=> ((Y1 Y2))
(cadr '((x1 x2) (y1 y2))) ;=> (Y1 Y2)
(memq 'red '((red shoes) (blue socks))) ;=> nil
(memq 'red '(red shoes blue socks)) ;=>(RED SHOES BLUE SOCKS) 

;;;exercice 2.54
(defun equal? (s1 s2)
  (cond 
    ((and (symbolp s1) (symbolp s2) (equal s1 s2)) t)
    ((and (listp s1) (listp s2) 
          (equal (car s1) (car s2)) (equal? (cdr s1) (cdr s2))) t)
    (t nil)))

;;;exercice 2.55
; ''abracadabra => (quote (quote abracadabra))


;;;;exercice 2.3.2
(defun variable? (x)
  (symbolp x))

(defun same-variable? (v1 v2)
  (and (variable? v1) (variable? v2) (eql v1 v2)))

(defun make-sum (a1 a2)
  (list '+ a1 a2))

(defun make-product (m1 m2)
  (list '* m1 m2))

(defun sum? (x)
  (and (consp x) (eql (car x) '+)))

(defun addend (s)
  (cadr s))

(defun augend (s)
  (caddr s))

(defun product? (x)
  (and (consp x) (eql (car x) '*)))

(defun multiplier (p)
  (cadr p))

(defun multiplicand (p)
  (caddr p))

(defun deriv (expr var)
  (cond ((numberp expr) 0)
        ((variable? expr)
         (if (same-variable? expr var) 1 0))
        ((sum? expr)
         (make-sum (deriv (addend expr) var)
                   (deriv (augend expr) var)))
        ((product? expr)
         (make-sum
           (make-product (multiplier expr)
                         (deriv (multiplicand expr) var))
           (make-product (deriv (multiplier expr) var)
                         (multiplicand expr))))
        (t (error "unknown expression type -- DERIV" expr))))

(defun make-sum (a1 a2)
  (cond ((equal a1 0) a2)
        ((equal a2 0) a1)
        ((and (numberp a1) (numberp a2)) (+ a1 a2))
        (t (list '+ a1 a2))))

(defun make-product (a1 a2)
  (cond ((or (equal a1 0) (equal a2 0)) 0)
        ((equal a1 1) a2)
        ((equal a2 1) a1)
        ((and (numberp a1) (numberp a2)) (* a1 a2))
        (t (list '* a1 a2))))

;;;exercice 2.56
(defun make-exponentiation (b e)
  (cond ((equal e 0) 1)
        ((equal e 1) b)
        (t (list '** b e))))

(defun base (expr)
  (cadr expr))

(defun exponent (expr)
  (caddr expr))

(defun exponentiation? (expr)
  (and (consp expr) (eql (car expr) '**)))

(defun deriv (expr var)
  (cond ((numberp expr) 0)
        ((variable? expr)
         (if (same-variable? expr var) 1 0))
        ((sum? expr)
         (make-sum (deriv (addend expr) var)
                   (deriv (augend expr) var)))
        ((product? expr)
         (make-sum
           (make-product (multiplier expr)
                         (deriv (multiplicand expr) var))
           (make-product (deriv (multiplier expr) var)
                         (multiplicand expr))))
        ((exponentiation? expr)
         (make-product 
           (exponent expr)
           (make-product
             (make-exponentiation (base expr) (- (exponent expr) 1))
             (deriv (base expr) var))))
        (t (error "unknown expression type -- DERIV" expr))))

;;;exercice 2.57
(defun make-sum (&rest as)
  (let ((sum-list '())
        (num-sum 0))
    (dolist (num as)
      (if (numberp num)
        (incf num-sum num)
        (push num sum-list)))
    (cond ((= num-sum 0) 
           (if (= 1 (length sum-list)) 
             (car sum-list) 
             (cons '+ sum-list)))
          ((null sum-list) num-sum)
          (t (append (list '+ num-sum) sum-list)))))

(defun addend (expr)
  (cadr expr))

(defun augend (expr)
  (if (= 3 (length expr))
    (caddr expr)
    (cons '+ (cddr expr))))

(defun make-product (&rest ms)
  (let ((product-list '())
        (num-product 1))
    (dolist (num ms)
      (if (numberp num)
        (setf num-product (* num-product num))
        (push num product-list)))
    (cond ((= num-product 0) 0)
          ((= num-product 1) (if (= 1 (length product-list)) 
                               (car product-list)
                               (cons '* product-list)))
          ((null product-list) num-product)
          (t (append (list '* num-product) product-list)))))

(defun multiplier (expr)
  (cadr expr))

(defun multiplicand (expr)
  (if (= 3 (length expr))
    (caddr expr)
    (cons '* (cddr expr))))

;;;exercice 2.58
;;a)
(defun make-sum (a1 a2)
  (cond ((equal a1 0) a2)
        ((equal a2 0) a1)
        ((and (numberp a1) (numberp a2)) (+ a1 a2))
        (t (list a1 '+ a2))))

(defun addend (expr)
  (car expr))

(defun augend (expr)
  (caddr expr))

(defun sum? (expr)
  (equal (cadr expr) '+))

(defun make-product (m1 m2)
  (cond ((or (equal m1 0) (equal m2 0)) 0)
        ((equal m1 1) m2)
        ((equal m2 1) m1)
        ((and (numberp m1) (numberp m2)) (* m1 m2))
        (t (list m1 '* m2))))

(defun multiplier (expr)
  (car expr))

(defun multiplicand (expr)
  (caddr expr))

(defun product? (expr)
  (equal (cadr expr) '*))
;;b)
(defun sum? (expr)
  (not (null (member '+ expr))))

(defun addend (expr)
  (let ((add-pos (position '+ expr)))
    (if (equal 1 add-pos)
      (first expr)
      (subseq expr 0 add-pos))))

(defun augend (expr)
  (let ((add-pos (position '+ expr)))
    (if (equal (- (length expr) 2) add-pos)
      (car (last expr))
      (subseq expr (1+ add-pos)))))

(defun make-sum (&rest as)
  (let ((sum-expr (list (car as))))
    (dolist (num (subseq as 1) sum-expr)
      (push '+ sum-expr)
      (push num sum-expr))))

(defun product? (expr)
  (and (null (member '+ expr))
       (some (lambda (x) (equal '* x)) expr)))

(defun multiplier (expr)
  (car expr))

(defun multiplicand (expr)
  (if (= (length expr) 3)
    (car (last expr))
    (subseq expr 2)))

(defun make-product (&rest ms)
  (let ((product-expr (list (car ms))))
    (dolist (num (subseq ms 1) product-expr)
      (push '* product-expr)
      (push num product-expr))))

;;;;2.3.3
(defun element-of-set? (x set)
  (cond ((null set) nil)
        ((equal (car set) x) t)
        (t (element-of-set? x (cdr set)))))

(defun adjoin-set (x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(defun intersection-set (set1 set2)
  (cond ((or (null set1) (null set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (t (intersection-set (cdr set1) set2))))

;;;exercice 2.59
(defun union-set (set1 set2)
  (cond ((null set1) set2)
        ((null set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (t (cons (car set1) (union-set (cdr set1) set2)))))

;;;exercice 2.60
;element-of-set? adjoin-set intersection-set  同上

(defun union-set (set1 set2)
  (append set1 set2))

;;;有序集合
(defun element-of-set? (x set)
  (cond ((null set) nil)
        ((= x (car set)) t)
        ((< x (car set)) nil)
        (t (element-of-set? x (cdr set)))))

(defun intersection-set (set1 set2)
  (if (or (null set1) (null set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2))))
            ((< x1 x2) (intersection-set (cdr set1) set2))
            ((> x1 x2) (intersection-set set1 (cdr set2)))))))

;;;exercice 2.61
(defun adjoin-set (x set)
  (cond ((null set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        ((> x (car set)) (cons (car set) (adjoin-set x (cdr set))))))

;;;exercice 2.62
(defun union-set (set1 set2)
  (cond ((null set1) set2)
        ((null set2) set1)
        (t (let ((x1 (car set1)) (x2 (car set2)))
             (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                   ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                   ((> x1 x2) (cons x2 (union-set set1 (cdr set2)))))))))

;;;;集合作为二叉树
(defun entry (tree) (car tree))
(defun left-branch (tree) (cadr tree))
(defun right-branch (tree) (caddr tree))
(defun make-tree (entry left right)
  (list entry left right))
(defun element-of-set? (x set)
  (cond ((null set) nil)
        ((= x (entry set)) t)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        ((> x (entry set)) (element-of-set? x (right-branch set)))))

(defun adjoin-set (x set)
  (cond ((null set) (make-tree x nil nil))
        ((= x (car set) set))
        ((< x (car set))
         (make-tree (car set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (car set))
         (make-tree (car set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;;;exercice 2.63
(defun tree->list-1 (tree)
  (if (null tree)
    '()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree) (tree->list-1 (right-branch tree))))))

(defun tree->list-2 (tree)
  (labels ((copy-to-list (tree result-list)
                         (if (null tree)
                           result-list
                           (copy-to-list (left-branch tree)
                                         (cons (entry tree)
                                               (copy-to-list (right-branch tree) result-list))))))
    (copy-to-list tree '())))
;a) 相同
;b) 不同 1是On2，而2是On

;;;exercice 2.64
(defun list->tree (elements)
  (car (partial-tree elements (length elements))))

(defun partial-tree (elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (floor (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-tree (cdr left-result))
              (right-size (- n (1+ left-size))))
          (let ((this-entry (car non-left-tree))
                (right-result (partial-tree (cdr non-left-tree) right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))
;a)结果为(5 (1 NIL (3 NIL NIL)) (9 (7 NIL NIL) (11 NIL NIL)))
;b)复杂度O(n)

;;;exercice 2.65
(defun union-tree (set1 set2)
  (list->tree
    (union-set (tree->list-2 set1) (tree->list-2 set2))))

(defun intersection-set (set1 set2)
  (list->tree
    (intersection-set (tree->list-2 set1) (tree->list-2 set2))))

;;;exercice 2.66
(defun lookup (given-key set-of-records)
  (cond ((null set-of-records) nil)
        ((= given-key (key (entry set-of-records))) (entry set-of-records))
        ((< given-key (key (entry set-of-records))) (lookup given-key (left-branch set-of-records)))
        (t (lookup given-key (right-branch set-of-records)))))


;;;;2.3.4
(defun make-leaf (symbol weight)
  (list 'leaf symbol weight))

(defun leaf? (obj)
  (eq (car obj) 'leaf))

(defun symbol-leaf (x)
  (cadr x))
(defun weight-leaf (x)
  (caddr x))

(defun make-code-tree (left right)
  (list left right (append (symbols left) (symbols right)) (+ (weight left) (weight right))))

(defun left-branch (tree)
  (car tree))
(defun right-branch (tree)
  (cadr tree))
(defun symbols (tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))
(defun weight (tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(defun decode (bits tree)
  (labels ((decode-1 (bits current-branch)
                     (if (null bits)
                       nil
                       (let ((next-branch (choose-branch (car bits) current-branch)))
                         (if (leaf? next-branch)
                           (cons (symbol-leaf next-branch)
                                 (decode-1 (cdr bits) tree))
                           (decode-1 (cdr bits) next-branch))))))
    (decode-1 bits tree)))

(defun choose-branch (bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (t (error "bad bit -- CHOOSE-BRANCH" bit))))

(defun adjoin-set (x set)
  (cond ((null set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (t (cons (car set) (adjoin-set x (cdr set))))))

(defun make-leaf-set (pairs)
  (if (null pairs)
    nil
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair) (cadr pair))
                  (make-leaf-set (cdr pairs))))))

;;;exercice 2.67
(defvar sample-tree (make-code-tree (make-leaf 'A 4)
                                    (make-code-tree
                                      (make-leaf 'B 2)
                                      (make-code-tree (make-leaf 'D 1)
                                                      (make-leaf 'C 1)))))

(defvar sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0)) ;=> (A D A B B C A)

;;;exercice 2.68
(defun encode (message tree)
  (if (null message)
    nil
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(defun encode-symbol (symbol tree)
  (if (leaf? tree)
    (if (equal (symbol-leaf tree) symbol)
      nil
      (error "symbol not support"))
    (if (position symbol (symbols (left-branch tree)))
      (cons 0 (encode-symbol symbol (left-branch tree)))
      (cons 1 (encode-symbol symbol (right-branch tree))))))

;;;exercice 2.69
(defun generate-huffman-tree (pairs)
  (successive-merge (make-leaf-set pairs)))

(defun successive-merge (leaf-set &optional (code-tree nil))
  (cond ((null leaf-set) nil)
        ((= 1 (length leaf-set)) (make-code-tree (car leaf-set) code-tree))
        (t (successive-merge (cdr leaf-set) 
                             (if (null code-tree) 
                               (car leaf-set)
                               (make-code-tree (car leaf-set) code-tree))))))

;;;exercice 2.70
(defvar rock-tree (generate-huffman-tree '((a 2) (na 16) (boom 1) (sha 3) (get 2) (yip 9) (job 2) (wah 1))))
;(encode '(get a job sha na na na na na na na na get a job sha na na na na na na na na wah yip yip yip yip yip yip yip yip yip sha boom) rock-tree )
; => length 87 ，如果用定长编码，需要108个二进制位

;;;exercice 2.71
;最频繁的用1个二进制位
;最不频繁的用n-1个

;;;exercice 2.72
;最频繁的符号是n
;最不频繁 (1+n) * n / 2 + n


;;;;2.4
(defun add-complex (z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(defun sub-complex (z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (real-part z2))))

(defun mul-complex (z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(defun div-complex (z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

(defun real-part (z) (car z))
(defun imag-part (z) (cdr z))
(defun magnitude (z)
  (sqrt (+ (expt (real-part z) 2) (expt (imag-part z) 2))))
(defun angle (z)
  (atan (imag-part z) (real-part z)))
(defun make-from-real-imag (x y)
  (cons x y))
(defun make-from-mag-ang (r a)
  (cons (* r (cos a)) (* r (sin a))))

(defun real-part (z)
  (* (magnitude z) (cos (angle z))))
(defun imag-part (z)
  (* (magnitude z) (sin (angle z))))
(defun magnitude (z) (car z))
(defun angle (z) (cdr z))
(defun make-from-real-imag (x y)
  (cons (sqrt (+ (expt x 2) (expt y 2)))
        (atan y x)))
(defun make-from-mag-ang (r a) (cons r a))

(defun attach-tag (type-tag contents)
  (cons type-tag contents))

(defun type-tag (datum)
  (if (consp datum)
    (car datum)
    (error "bad tagged datum -- TYPE-TAG" datum)))

(defun contents (datum)
  (if (consp datum)
    (cdr datum)
    (error "bad tagged datum -- TYPE-TAG" datum)))

(defun rectangular? (z)
  (eql (type-tag z) 'rectangular))

(defun polar? (z)
  (eql (type-tag z) 'polar))

(defun real-part-rectangular (z) (car z))
(defun imag-part-rectangular (z) (cdr z))
(defun magnitude-rectangular (z)
  (sqrt (+ (expt (real-part-rectangular z) 2)
           (expt (imag-part-rectangular z) 2))))
(defun angle-rectangular (z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))
(defun make-from-real-imag-rectangular (x y)
  (attach-tag 'rectangular (cons x y)))
(defun make-from-mag-ang-rectangular (r a)
  (attach-tag 'rectangular (cons (* r (cos a)) (* r (sin a)))))

(defun real-part-polar (z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(defun imag-part-polar (z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(defun magnitude-polar (z) (car z))
(defun angle-polar (z) (cdr z))
(defun make-from-real-imag-polar (x y)
  (attach-tag 'polar (cons (sqrt (+ (expt x 2) (expt y 2)))
                           (atan y x))))
(defun make-from-mag-ang-polar (r a)
  (attach-tag 'polar (cons r a)))

(defun real-part (z)
  (cond ((rectangular? z) (real-part-rectangular (contents z)))
        ((polar? z) (real-part-polar (contents z)))
        (t (error "Unknown type -- REAL-TYPE" z))))
(defun imag-part (z)
  (cond ((rectangular? z) (imag-part-rectangular (contents z)))
        ((polar? z) (imag-part-polar (contents z)))
        (t (error "Unknown type -- REAL-TYPE" z))))
(defun magnitude (z)
  (cond ((rectangular? z) (magnitude-rectangular (contents z)))
        ((polar? z) (magnitude-polar (contents z)))
        (t (error "Unknown type -- MAGNITUDE" z))))
(defun angle (z)
  (cond ((rectangular? z) (angle-rectangular (contents z)))
        ((polar? z) (angle-polar (contents z)))
        (t (error "Unknown type -- ANGLE" z))))
(defun make-from-real-imag (x y)
  (make-from-real-imag-rectangular x y))
(defun make-from-mag-ang (r a)
  (make-from-mag-ang-polar r a))


(defun install-rectangular-package ()
  (flet ((real-part (z) (car z))
         (imag-part (z) (cdr z))
         (make-from-real-imag (x y) (cons x y))
         (magnitude (z)
                    (sqrt (+ (expt (real-part z) 2)
                             (expt (imag-part z) 2))))
         (angle (z) (atan (imag-part z) (real-part z)))
         (make-from-mag-ang (r a) (cons (* r (cos a)) (* (sin a))))
         (tag (x) (attach-tag 'rectangular x)))
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'make-from-real-imag 'rectangular 
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular 
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

(defun install-polar-package ()
  (flet ((magnitude (z) (car z))
         (angle (z) (cdr z))
         (make-from-mag-ang (r a) (cons r a))
         (real-part (z) (* (magnitude z) (cos (angle z))))
         (imag-part (z) (* (magnitude z) (sin (angle z))))
         (make-from-real-imag (x y)
                              (cons (sqrt (+ (expt x 2) (expt x 2)))
                                    (atan y x)))
         (tag (x) (attach-tag 'polar x)))
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'make-from-real-imag 'polar 
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'polar 
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

(defun apply-generic (op &rest args)
  (let ((type-tags (mapcar #'type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (mapcar #'contents args))
        (error "No method for these types -- APPLY-GENERIC" (list op type-tags))))))

(defun real-part (z) (apply-generic 'real-part z))
(defun imag-part (z) (apply-generic 'imag-part z))
(defun magnitude (z) (apply-generic 'magnitude z))
(defun angle (z) (apply-generic 'angle z))

(defun make-from-real-imag (x y)
  (funcall (get 'make-from-real-imag 'rectangular) x y))
(defun make-from-mag-ang (r a)
  (funcall (get 'make-from-mag-ang 'polar) r a))

;;;exercice 2.73
;a) 根据代数运算符的类型找到对应的操作，将其应用到操作的参数上。因为数字和变量是一类符号，不能应用数据导向分派。

(defun deriv-1 (exp var)
  (cond ((numberp exp) 0)
        ((variable? exp) (if (equal exp var) 1 0))
        (t (funcall (get 'deriv (operator exp)) (operands exp) var))))

(defun operator (exp) (car exp))
(defun operands (exp) (cdr exp))

(defun install-sum ()
  (labels ((deriv-add (args var)
                      (make-sum (deriv-1 (first args) var)
                                (deriv-1 (second args) var))))
    (put 'deriv '+ deriv-add)))
(defun install-product ()
  (labels ((deriv-product (args var)
                          (make-sum 
                            (make-product (first args) (deriv-1 (second exp) var))
                            (make-product (deriv-1 (first exp) var) (second exp)))))
    (put 'deriv '* deriv-product)))

;c) 略
;d) 在安装的时候就是 (put '* 'deriv deriv-add) 这样了。

;;;exercice 2.74 TODO

;;;exercice 2.75
(defun make-from-mag-ang-1 (r a)
  (Lambda (op)
          (cond ((eql op 'real-part) (* r (cos a)))
                ((eql op 'imag-part) (* r (sin a)))
                ((eql op 'magnitude) r)
                ((eql op 'angle) a)
                (t (error "Unknown op -- MAKE-FROM-REAL-IMAG" op)))))

;;;exercice 2.76
;数据导向适合经常加入操作的系统
;消息传递适合经常加入类型的系统


;;;;2.5
(defun add (x y) (apply-generic 'add x y))
(defun sub (x y) (apply-generic 'sub x y))
(defun mul (x y) (apply-generic 'mul x y))
(defun div (x y) (apply-generic 'div x y))

(defun install-scheme-number-package ()
  (flet ((tag (x) (attach-tag 'scheme-number x)))
    (put 'add '(scheme-number scheme-number)
         (lambda (x y)(tag (+ x y))))
    (put 'sub '(scheme-number scheme-number)
         (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number)
         (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number)
         (lambda (x y) (tag (/ x y))))
    (put 'make 'scheme-number
         (lambda (x) (tag x)))
    (put 'equ? '(scheme-number scheme-number)
         (lambda (x y) (= x y)))
    (put '=zero? 'scheme-number
         (lambda (x) (= x 0)))
    'done))

(defun make-scheme-number (n)
  (funcall (get 'make 'scheme-number) n))

(defun install-rational-package ()
  (flet ((numer (x) (car x))
         (denom (x) (cdr x))
         (make-rat (n d) (let ((g (gcd n d)))
                           (cons (/ n g) (/ d g))))
         (add-rat (x y) (make-rat (+ (* (numer x) (denom y))
                                     (* (numer y) (denom x)))
                                  (* (denom x) (denom y))))
         (sub-rat (x y) (make-rat (- (* (numer x) (denom y))
                                     (* (numer y) (denom x)))
                                  (* (denom x) (denom y))))
         (mul-rat (x y) (make-rat (* (numer x) (numer y))
                                  (* (denom x) (denom y))))
         (div-rat (x y) (make-rat (* (numer x) (denom y))
                                  (* (denom x) (numer y))))
         (tag (x) (attach-tag 'rational x)))
    (put 'add '(rational rational)
         (lambda (x y) (tag (add-rat x y))))
    (put 'sub '(rational rational)
         (lambda (x y) (tag (sub-rat x y))))
    (put 'mul '(rational rational)
         (lambda (x y) (tag (mul-rat x y))))
    (put 'div '(rational rational)
         (lambda (x y) (tag (div-rat x y))))
    (put 'make 'rational
         (lambda (n d) (tag (make-rat n d))))
    (put 'equ? '(rational rational)
         (lambda (x y) (and (= (numer x) (numer y))
                            (= (denom x) (denom y)))))
    (put '=zero? 'rational
         (lambda (x) (and (= (numer x) 0)
                          (= (denom y) 0))))
    'done))
(defun make-rational (n d)
  (funcall (get 'make 'rational) n d))

(defun install-complex-package ()
  (flet ((make-from-real-imag (x y)
                              (funcall (get 'make-from-real-imag 'rectangular) x y))
         (make-from-mag-ang (r a)
                            (funcall (get 'make-from-mag-ang 'polar) r a))
         (add-complex (z1 z2)
                      (make-from-real-imag (+ (real-part z1) (real-part z2))
                                           (+ (imag-part z1) (imag-part z2))))
         (sub-complex (z1 z2)
                      (make-from-real-imag (- (real-part z1) (real-part z2))
                                           (- (imag-part z1) (real-part z2))))
         (mul-complex (z1 z2)
                      (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                                         (+ (angle z1) (angle z2))))
         (div-complex (z1 z2)
                      (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                                         (- (angle z1) (angle z2))))
         (tag (z) (attach-tag 'complex z)))
    (put 'add '(complex complex)
         (lambda (z1 z2) (tag (add-complex z1 z2))))
    (put 'sub '(complex complex)
         (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (put 'mul '(complex complex)
         (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (pub 'div '(complex complex)
         (lambda (z1 z2) (tag (div-complex z1 z2))))
    (put 'make-from-real-imag 'complex
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex
         (lambda (r a) (tag (make-from-mag-ang r a))))
    (put 'equ? '(complex complex)
         (lambda (x y) (and (= (real-part x) (real-part y))
                            (= (imag-part x) (imag-part y)))))
    (put '=zero? 'complex
         (lambda (x y) (and (= (real-part x) 0)
                            (= (imag-part x) 0))))
    'done))

(defun make-complex-from-real-imag (x y)
  (funcall (get 'make-from-real-imag 'complex) x y))
(defun make-complex-from-mag-ang (r a)
  (funcall (get 'make-from-mag-ang 'complex) r a))

;;;exercice 2.77
; 因为这样会脱去最外层的complex，用rectangular 去调用magnitude 
; 过程 TODO

;;;exercice 2.78
(defun attach-tag (type-tag contents)
  (if (numberp contents)
    contents
    (cons type-tag contents)))

(defun type-tag (datum)
  (if (consp datum)
    (car datum)
    datum))

(defun contents (datum)
  (if (consp datum)
    (car datum)
    datum))

;;;exercice 2.79
;安装见上方
(defun equ? (x y)
  (apply-generic 'equ? x y))

;;;exercice 2.80
;安装见上方
(defun equ? (x)
  (apply-generic '=zero? x))


;;;; 2.5.2
(defun apply-generic (op &rest args)
  (let ((type-tags (mapcar #'type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map #'contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (let ((t1->t2 (get-coercion type1 type2))
                  (t2->t1 (get-coercion type2 type1)))
              (cond 
                (t1->t2 (apply-generic op (t1->t2 a1) a2))
                (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                (t (error "No method for these types" (list op type-tags))))))
          (error "No method for these types" (list op type-tags))))))

;;;exercice 2.81
;a) 找不到相应操作时会出现无限递归
;b) 没有纠正，不能像原来那样正常工作
;c)
(defun apply-generic (op &rest args)
  (let ((type-tags (mapcar #'type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (mapcar #'contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (if (equal type1 type2)
              (error "No method for these types" (list op type-tags))
              (let ((t1->t2 (get-coercion type1 type2))
                    (t2->t1 (get-coercion type2 type1)))
                (cond 
                  (t1->t2 (apply-generic op (funcall t1->t2 a1) a2))
                  (t2->t1 (apply-generic op a1 (funcall t2->t1 a2)))
                  (t (error "No method for these types" (list op type-tags)))))))
          (error "No method for these types" (list op type-tags))))))

;;;exercice 2.82
(defun apply-generic-n (op &rest args)
  (labels ((apply-inner (op i &rest args)
                        (let ((n-args (mapcar (lambda (arg)
                                                (let ((t->tn (get-coercion (type-tag arg)
                                                                        (type-tag (elt args i)))))
                                                  (if t->tn
                                                    (apply t->tn arg)
                                                    nil)))
                                              args)))
                          (let ((type-tags (mapcar #'type-tag n-args)
                                           (let ((proc (get op type-tags)))
                                             (if proc
                                               (apply proc (mapcar #'contents n-args))
                                               (if (= i (length args))
                                                 (error "No Method for these type" (list op type-tags))
                                                 (apply-inner op (1+ i) args))))))))))
    (apply-inner op 0 args)))
;;TODO 例外情况

;;;exercice 2.83
(defun install-raise ()
  (put-coercion 'scheme-number 'rational 
                (lambda (n) (make-rat (contents n) 1)))
  (put-coercion 'rational 'complex
                (lambda (n) (make-complex-from-real-imag (/ (numer (contents n)) (denom (contents n))) 0))))
;;TODO raise

;;;exercice 2.84 TODO
;;;exercice 2.85 TODO
;;;exercice 2.86 TODO
