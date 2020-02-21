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
