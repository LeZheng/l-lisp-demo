;;LcExp ::= Identifier
;;      ::= (lambda (Identifier) LcExp)
;;      ::= (LcExp LcExp)

;;;1.1
;;TODO
;;;1.2
;;TODO
;;;1.3
;;TODO

;;;1.4
;; List-of-Int
;; => (Int . List-of-Int)
;; => (-7  . List-of-Int)
;; => (-7  . (Int . List-of-Int))
;; => (-7  . (3   . List-of-Int))
;; => (-7  . (3   . (Int . List-of-Int)))
;; => (-7  . (3   . (14 . List-of-Int)))
;; => (-7  . (3   . (14 . ())))

;;;1.5
;;如果LcExp是一个Identifier，那么左括号数和有括号数都是0，相等。
;;如果LcExp是(lambda (Identifier) LcExp)，左边和右边括号数都是 LcExp 的括号数+2，相等。
;;如果LcExp是(LcExp LcExp)，左边的和右边的括号数都是 两个LcExp的括号数+1，相等。

(defun list-length (l)
  (if (null l)
      0
    (+ 1 (list-length (cdr l)))))

(defun report-list-too-short (n)
  (error "List to short by ~A elements.~%" (+ n 1)))

(defun nth-element (lst n)
  (if (null lst)
      (report-list-too-short n)
    (if (zerop n)
	(car lst)
      (nth-element (cdr lst) (- n 1)))))

(defun remove-first (s los)
  (if (null los)
      '()
    (if (eql s (car los))
	(cdr los)
      (cons (car los) (remove-first s (cdr los))))))

(defun occurs-free? (s lcexp)
  (cond
   ((atom lcexp) (eql s lcexp))
   ((eql 'lambda (car lcexp)) (and (not (eql s (caadr lcexp)))
				   (occurs-free? s (caddr lcexp))))
   (t (or (occurs-free? s (car lcexp))
	  (occurs-free? s (cadr lcexp))))))

;;;1.10
;;TODO

;;eopl subst
(defun subst-in-s-list (new old slist)
  (if (null slist)
      '()
    (cons (subst-in-s-exp new old (car slist))
	  (subst-in-s-list new old (cdr slist)))))

(defun subst-in-s-exp (new old sexp)
  (cond
   ((atom sexp) (if (eql sexp old) new sexp))
   (t (subst-in-s-list new old sexp))))

;;;1.11
;;因为 subst 在调用 subst 或 subst-in-s-exp 时都在缩小结构

;;;1.12
(defun subst-without-s-exp (new old slist)
  (if (null slist)
      '()
    (cons (cond
	   ((atom (car slist)) (if (eql (car slist) old) new (car slist)))
	   (t (subst-without-s-exp new old (car slist))))
	  (subst-without-s-exp new old (cdr slist)))))

;;;1.13
(defun subst-with-map (new old slist)
  (mapcar (lambda (s-exp)
	    (cond
	     ((atom s-exp) (if (eql s-exp old) new s-exp)
	     (t (subst-with-map new old s-exp)))))
	  slist))

(defun number-elements-from (lst n)
  (if (null lst)
      '()
    (cons
     (list n (car lst))
     (number-elements-from (cdr lst) (+ 1 n)))))
(defun number-elements (lst)
  (number-elements-from lst 0))

(defun list-sum (loi)
  (if (null loi)
      0
    (+ (car loi)
       (list-sum (cdr loi)))))

(defun partial-vector-sum (v n)
  (if (zerop n)
      (svref v 0)
    (+ (svref v n)
       (partial-vector-sum v (- n 1)))))

(defun vector-sum (v)
  (let ((n (length v)))
    (if (zerop n)
	0
      (partial-vector-sum v (- n 1)))))

;;;1.14
;;如果向量没有元素，那么它的和为0；
;;如果向量只有一个元素，那么它的和就是该元素；
;;如果向量不止一个元素，那么它的和就是最后一个元素加上其他元素的和。

;;;1.15
(defun duple (n x)
  (if (zerop n 0)
      '()
    (cons x (duple (- n 1) x))))

;;;1.16
(defun invert (lst)
  (if (null lst)
      '()
    (cons (list (cadar lst) (caar lst))
	  (invert (cdr lst)))))

;;;1.17
(defun down (lst)
  (if (null lst)
      '()
    (cons (cons (car lst) nil)
	  (down (cdr lst)))))

;;;1.18
(defun swapper (s1 s2 slist)
  (cond
   ((null slist) '())
   ((atom (car slist))
    (cons (cond
	   ((eql s1 (car slist)) s2)
	   ((eql s2 (car slist)) s1)
	   (t (car slist)))
	  (swapper s1 s2 (cdr slist))))
   (t (cons (swapper s1 s2 (car slist))
	    (swapper s1 s2 (cdr slist))))))

;;;1.19
(defun list-set (lst n x)
  (if (null lst)
      '()
    (if (zerop n)
	(cons x (cdr lst))
      (cons (car lst)
	    (list-set (cdr lst) (- n 1) x)))))

;;;1.20
(defun count-occurrences (s slist)
  (cond
   ((null slist) 0)
   ((atom (car slist))
    (+ (if (eql s (car slist)) 1 0)
       (count-occurrences s (cdr slist))))
   (t (+ (count-occurrences s (car slist))
	 (count-occurrences s (cdr slist))))))

;;;1.21
(defun product-s (s sos)
  (if (null sos)
      '()
    (cons (list s (car sos))
	  (product-s s (cdr sos)))))

(defun product (sos1 sos2)
  (cond
   ((or (null sos1) (null sos2)) '())
   (t (append (product-s (car sos1) sos2)
	      (product (cdr sos1) sos2)))))

;;;1.22
(defun filter-in (pred lst)
  (if (null lst)
      '()
    (if (funcall pred (car lst))
	(cons (car lst)
	      (filter-in pred (cdr lst)))
      (filter-in pred (cdr lst)))))

;;;1.23
(defun list-index-from (n pred lst)
  (if (null lst)
      nil
    (if (funcall pred (car lst))
        n
      (list-index-from (+ n 1) pred (cdr lst)))))
(defun list-index (pred lst)
  (list-index-from 0 pred lst))

;;;1.24
(defun every? (pred lst)
  (if (null lst)
      t
    (if (funcall pred (car lst))
	(every? pred (cdr lst))
      nil)))

;;;1.25
(defun exists? (pred lst)
  (if (null lst)
      nil
    (if (funcall pred (car lst))
	t
      (exist? pred (cdr lst)))))

;;;1.26
(defun up (lst)
  (if (null lst)
      '()
    (if (atom (car lst))
	(cons (car lst)
	      (up (cdr lst)))
      (append (car lst) (up (cdr lst))))))

;;;1.27
(defun flatten (slist)
  (cond
   ((null slist) nil)
   ((null (car slist)) (flatten (cdr slist)))
   ((atom (car slist)) (cons (car slist)
			     (flatten (cdr slist))))
   (t (append (flatten (car slist))
	      (flatten (cdr slist))))))

;;;1.28
(defun loi-merge (loi1 loi2)
  (cond
   ((null loi1) loi2)
   ((null loi2) loi1)
   (t (if (> (car loi1) (car loi2))
	  (cons (car loi2)
		(loi-merge loi1 (cdr loi2)))
	(cons (car loi1)
	      (loi-merge (cdr loi1) loi2))))))

;;;1.29
(defun insert (i loi)
  (if (null loi)
      (cons i nil)
    (if (> i (car loi))
	(cons (car loi) (insert i (cdr loi)))
      (cons i loi))))
(defun loi-sort (loi)
  (if (null loi)
      '()
    (insert (car loi)
	    (loi-sort (cdr loi)))))

;;;1.30
(defun insert-predicate (pred i loi)
  (if (null loi)
      (cons i nil)
    (if (funcall pred i (car loi))
	(cons i loi)
      (cons (car loi) (insert-predicate pred i (cdr loi))))))
(defun sort-predicate (pred loi)
  (if (null loi)
      '()
    (insert-predicate pred (car loi)
		      (sort-predicate pred (cdr loi)))))

;;;1.31
(defun leaf (i)
  (cons i nil))

(defun interior-node (s t1 t2)
  (cons s (cons t1 (cons t2 nil))))

(defun leaf? (tree)
  (null (cdr tree)))

(defun lson (tree)
  (cadr tree))

(defun rson (tree)
  (caddr tree))

(defun contents-of (tree)
  (car tree))

;;;1.32
(defun double-tree (tree)
  (cond
   ((null tree) '())
   ((leaf? tree) (leaf (* (contents-of tree) (contents-of tree))))
   (t (interior-node (contents-of tree)
		     (double-tree (lson tree))
		     (double-tree (rson tree))))))

;;;1.33
(defun mark-leaves-with-n (n tree)
  (cond
   ((null tree) '())
   ((leaf? tree) (leaf n))
   (t (if (eql 'red (contents-of tree))
	  (interior-node (contents-of tree)
			 (mark-leaves-with-n (+ n 1) (lson tree))
			 (mark-leaves-with-n (+ n 1) (rson tree)))
	(interior-node (contents-of tree)
		       (mark-leaves-with-n n (lson tree))
		       (mark-leaves-with-n n (rson tree)))))))
(defun mark-leaves-with-red-depth (tree)
  (if (null tree)
      '()
    (mark-leaves-with-n 0 tree)))

;;;1.34
(defun tree-path (n bst)
  (if (null bst)
      'not-found
    (cond
     ((> n (car bst)) (cons 'right (tree-path n (caddr bst))))
     ((< n (car bst)) (cons 'left (tree-path n (cadr bst))))
     (t '()))))

;;;1.35
(defun number-tree (n tree)
  (cond
   ((leaf? tree) (cons (+ n 1) (leaf n)))
   (t (let ((l-temp (number-tree n (lson tree))))
	(let ((r-temp (number-tree (car l-temp) (rson tree))))
	  (cons (car r-temp)
		(interior-node (contents-of tree)
			       (cdr l-temp)
			       (cdr r-temp))))))))

(defun number-leaves (tree)
  (cond
   ((null tree) '())
   (t (cdr (number-tree 0 tree)))))

;;;1.36
(defun number-elements (lst)
    (if (null lst) '()
      (g (list 0 (car lst)) (number-elements (cdr lst)))))
(defun g (l lst)
  (cons l (mapcar #'(lambda (l) (list (+ 1 (car l)) (cadr l))) lst)))
			



      
