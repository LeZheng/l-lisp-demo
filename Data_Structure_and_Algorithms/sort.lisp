;;1,冒泡排序
(defun bubble-sort (arr)
   (dotimes (i (1- (length arr)) arr)
     (dotimes (j (- (length arr) i 1))
       (if (> (elt arr j) (elt arr (1+ j)))
         (rotatef (elt arr j) (elt arr (1+ j)))))))

(defun bubble-sort-2 (arr &optional (start 0))
  (if (>= start (1- (length arr)))
    arr
    (progn (loop for i from (1- (length arr)) above start
                      do (if (< (elt arr i) (elt arr (1- i))) (rotatef (elt arr i) (elt arr (1- i)))))
           (bubble-sort-2 arr (1+ start)))))

;;2,插入排序
(defun insert-sort (arr)
  (dotimes (i (1- (length arr)) arr)
    (loop for j from (1+ i) downto 1 do (if (< (elt arr j) (elt arr (1- j)))
                                          (rotatef (elt arr j) (elt arr (1- j)))
                                          (return)))))

(defun insert-sort-2 (arr &optional (start 0))
  (if (= start (length arr))
    arr
    (loop for i from start downto 1 until (> (elt arr i) (elt arr (1- i))) do (rotatef (elt arr i) (elt arr (1- i)))
          finally (return (insert-sort-2 arr (1+ start))))))

;;3,选择排序
(defun select-sort (arr)
  (dotimes (i (1- (length arr)) arr)
    (let ((min-index i))
      (loop for j from i to (1- (length arr)) do (if (< (elt arr j) (elt arr min-index)) (setf min-index j)))
      (rotatef (elt arr i) (elt arr min-index)))))

(defun select-sort-2 (arr &optional (start 0))
  (if (>= start (1- (length arr)))
    arr
    (loop with min-index = start for i from start to (1- (length arr)) do (if (< (elt arr i) (elt arr min-index)) (setf min-index i))
          finally (progn (rotatef (elt arr min-index) (elt arr start))
                         (return (select-sort-2 arr (1+ start)))))))

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

;;6,快速排序
(defun quick-sort (arr)
  (labels ((q-sort (seq start end)
                 (if (>= start end)
                   seq
                   (let ((key (elt seq start))
                         (i start)
                         (j end))
                     (loop (if (= i j)
                             (progn (q-sort seq start (1- i)) 
                                    (q-sort seq (1+ i) end)
                                    (return seq))
                             (progn (loop for index from j downto i do (setf j index) until (< (elt seq index) key) finally (if (>= index i) (rotatef (elt seq index) (elt seq i))))
                                    (loop for index from i to j do (setf i index) until (> (elt seq index) key) finally (if (<= index j) (rotatef (elt seq index) (elt seq j)))))))))))
    (q-sort arr 0 (1- (length arr)))))
