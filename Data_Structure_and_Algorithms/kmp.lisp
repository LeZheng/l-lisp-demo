(defun index-kmp (str p)
  (let ((next (loop for i from 1 below (length p)
                    with arr = (make-array (length p) :initial-element 0)
                    with j = 0
                    do (cond 
                         ((= i 1) (setf (aref arr i) 1))
                         ((equal (elt p (1- i)) (elt p j)) (psetf j (1+ j) (aref arr i) (1+ (aref arr (1- i)))))
                         (t (setf j 0 (aref arr i) 1)))
                    finally (return arr))))
    (loop with i = 0 and j = 0
          while (and (< i (length str)) (< j (length next)))
          when (equal (elt str i) (elt p j)) do (if (= j (1- (length next))) 
                                                    (return (- i j)) 
                                                    (progn (incf i) (incf j)))
          else do (setf i (1+ i) j (max 0 (1- (aref next j)))))))
