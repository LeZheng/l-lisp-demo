;;1,冒泡排序
(defun bubble-sort (arr)
   (dotimes (i (1- (length arr)) arr)
     (dotimes (j (- (length arr) i 1))
       (if (> (elt arr j) (elt arr (1+ j)))
         (rotatef (elt arr j) (elt arr (1+ j)))))))

;;2,插入排序
(defun insert-sort (arr)
  (dotimes (i (1- (length arr)) arr)
    (loop for j from (1+ i) downto 1 do (if (< (elt arr j) (elt arr (1- j)))
                                          (rotatef (elt arr j) (elt arr (1- j)))
                                          (return)))))

;;3,选择排序
(defun select-sort (arr)
  (dotimes (i (1- (length arr)) arr)
    (let ((min-index i))
      (loop for j from i to (1- (length arr)) do (if (< (elt arr j) (elt arr min-index)) (setf min-index j)))
      (rotatef (elt arr i) (elt arr min-index)))))

;;4,希尔排序
(defun shell-sort (arr)
  (do ((gap (floor (length arr) 2) (floor gap 2)))
    ((< gap 1) arr)
    (dotimes (i gap)
      (dotimes (k (1- (floor (length arr) gap)))
        (loop for j from (+ i (* gap k)) downto 0 by gap do (if (< (elt arr (+ j gap)) (elt arr j))
                                                                 (rotatef (elt arr j) (elt arr (+ j gap)))
                                                                 (return)))))))

;;5,归并排序
(defun merge-sort (arr)
  (if (< (length arr) 2)
    arr
    (let ((arr-1 (merge-sort (subseq arr 0 (floor (length arr) 2))))
          (arr-2 (merge-sort (subseq arr (floor (length arr) 2)))))
      (merge 'vector arr-1 arr-2 #'<))))

