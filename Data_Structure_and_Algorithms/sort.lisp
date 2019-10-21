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
