;;1,冒泡排序
(defun bubble-sort (arr)
   (dotimes (i (1- (length arr)) arr)
     (dotimes (j (- (length arr) i 1))
       (if (> (svref arr j) (svref arr (1+ j)))
         (rotatef (svref arr j) (svref arr (1+ j)))))))

(defun bubble-sort-2 (arr &optional (start 0))
  (if (>= start (1- (length arr)))
    arr
    (progn (loop for i from (1- (length arr)) above start
                      do (if (< (svref arr i) (svref arr (1- i))) (rotatef (svref arr i) (svref arr (1- i)))))
           (bubble-sort-2 arr (1+ start)))))

;;2,插入排序
(defun insert-sort (arr)
  (dotimes (i (1- (length arr)) arr)
    (loop for j from (1+ i) downto 1 do (if (< (svref arr j) (svref arr (1- j)))
                                          (rotatef (svref arr j) (svref arr (1- j)))
                                          (return)))))

(defun insert-sort-2 (arr &optional (start 0))
  (if (= start (length arr))
    arr
    (loop for i from start downto 1 until (> (svref arr i) (svref arr (1- i))) do (rotatef (svref arr i) (svref arr (1- i)))
          finally (return (insert-sort-2 arr (1+ start))))))

;;3,选择排序
(defun select-sort (arr)
  (dotimes (i (1- (length arr)) arr)
    (let ((min-index i))
      (loop for j from i to (1- (length arr)) do (if (< (svref arr j) (svref arr min-index)) (setf min-index j)))
      (rotatef (svref arr i) (svref arr min-index)))))

(defun select-sort-2 (arr &optional (start 0))
  (if (>= start (1- (length arr)))
    arr
    (loop with min-index = start for i from start to (1- (length arr)) do (if (< (svref arr i) (svref arr min-index)) (setf min-index i))
          finally (progn (rotatef (svref arr min-index) (svref arr start))
                         (return (select-sort-2 arr (1+ start)))))))

;;4,希尔排序
(defun shell-sort (arr)
  (do ((gap (floor (length arr) 2) (floor gap 2)))
    ((< gap 1) arr)
    (dotimes (i gap)
      (dotimes (k (1- (floor (length arr) gap)))
        (loop for j from (+ i (* gap k)) downto 0 by gap do (if (< (svref arr (+ j gap)) (svref arr j))
                                                                 (rotatef (svref arr j) (svref arr (+ j gap)))
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
  (labels ((q-sort (vec l r)
                   (let ((i l)
                         (j r)
                         (p (svref vec (round (+ l r) 2))))
                     (loop while (<= i j) 
                           do (progn
                                (loop while (< (svref vec i) p) do (incf i))
                                (loop while (> (svref vec j) p) do (decf j))
                                (when (<= i j)
                                  (rotatef (svref vec i) (svref vec j))
                                  (incf i)
                                  (decf j))))
                     (if (>= (- j l) 1) (q-sort vec l j))
                     (if (>= (- r i) 1) (q-sort vec i r)))
                   vec))
    (q-sort arr 0 (1- (length arr)))))

;;7,基数排序
(defun radix-sort (arr &optional (radix 0) (max-radix nil))
  (let ((bucket (make-array 16 :initial-element nil))
        (max-radix (or max-radix (reduce #'max arr :key #'integer-length))))
    (loop for e across arr do (push e (aref bucket (ldb (byte 4 (* radix 4)) e))))
    (let ((bucket-seq (coerce (reduce #'nconc bucket :key #'reverse) 'vector)))
      (if (<= max-radix radix)
        bucket-seq
        (radix-sort bucket-seq (1+ radix) max-radix)))))

;;8,堆排序
(defun heap-sort (arr)
  (labels ((heapify (seq current-index size)
                    (let ((left (+ (* 2 current-index) 1))
                          (right (+ (* 2 current-index) 2))
                          (max current-index))
                      (if (and (< left size) (> (svref arr left) (svref arr max))) (setf max left))
                      (if (and (< right size) (> (svref arr right) (svref arr max))) (setf max right))
                      (when (/= current-index max)
                        (rotatef (svref arr max) (svref arr current-index))
                        (heapify arr max size)))))
    (dotimes (i (length arr) arr)
      (let ((max-size (- (length arr) i)))
        (loop for j from (1- max-size) downto 0
              do (heapify arr j max-size))
        (rotatef (svref arr 0) (svref arr (1- max-size)))))))


(defvar *test-seq* #(1 4 0 2 3 8 5 3 33 77 88 9 11))

(defun test-correctness ()
  (let ((funs (list 'bubble-sort 'bubble-sort-2 'insert-sort 'insert-sort-2 
                    'select-sort 'select-sort-2 'quick-sort 'heap-sort 
                    'radix-sort 'shell-sort 'merge-sort)))
    (dolist (fun funs)
      (if (equalp (sort (copy-seq *test-seq*) #'<) (funcall (symbol-function fun) (copy-seq *test-seq*)))
        (format t "~A test ok~%" (symbol-name fun))
        (format t "~A test failed~%" (symbol-name fun))))))

(defun test-random ()
  (let ((funs (list 'bubble-sort 'bubble-sort-2 'insert-sort 'insert-sort-2 
                    'select-sort 'select-sort-2 'quick-sort 'heap-sort 
                    'radix-sort 'shell-sort 'merge-sort))
        (random-seq (coerce (loop for i from 1 to 10000 collect (random 10000)) 'vector)))
    (dolist (fun funs)
      (if (not (typep (symbol-function fun) 'compiled-function)) (compile fun))
      (format t "-----------------~%test ~A ...~%" (symbol-name fun))
      (if (equalp (sort (copy-seq random-seq) #'<) (time (funcall (symbol-function fun) (copy-seq random-seq))))
        (format t "~A test ok~%" (symbol-name fun))
        (format t "~A test failed~%" (symbol-name fun))))))

;(test-random)
