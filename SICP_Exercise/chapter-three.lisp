;;;;3.1
;;;exercise 3.1
(defun make-accumulator (num)
  (lambda (x)
    (setf num (+ num x))
    num))

;;;exercise 3.2
(defun make-monitored (f)
  (let ((count 0))
    (lambda (x)
      (if (equal 'how-many-calls x) 
        count
        (progn
          (incf count)
          (funcall f x))))))

;;;exercise 3.3 3.4
(defun make-account (balance psd)
  (labels ((withdraw (amount)
                     (if (>= balance amount)
                       (progn (setf balance (- balance amount))
                              balance)
                       (warn "Insufficient Funds")))
           (deposit (amount)
                    (setf balance (+ balance amount))
                    balance)
           (call-the-cops () (error "Call the cops")))
    (let ((error-count 0))
      (lambda (password m)
        (if (equal password psd)
          (progn 
            (setf error-count 0)
            (cond ((equal m 'withdraw) #'withdraw)
                  ((equal m 'deposit) #'deposit)
                  ((equal m 'check-password) t)
                  (t (error "Unknown request -- MAKE_ACCOUNT" m))))
          (progn 
            (incf error-count)
            (if (>= error-count 7)
              (call-the-cops)
              "Incorrect Password")))))))

;;;exercise 3.5
(defun estimate-po (trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(defun cesaro-test ()
  (= (gcd (random 1000) (random 1000)) 1))

(defun monte-carlo (trials experiment)
  (labels ((iter (trials-remaining trials-passwd)
                (cond ((= trials-remaining 0) (/ trials-passwd trials))
                      ((experiment) (iter (- trials-remaining 1) (+ trials-passwd 1)))
                      (t (iter (- trials-remaining 1) trials-passwd)))))
    (iter trials 0)))

(defun random-in-range (low high)
  (+ low (random (- high low))))

(defun estimate-integral (trials p x1 x2 y1 y2)
  (* 4 (monte-carlo trials (lambda () 
                             (funcall p 
                                      (random-in-range x1 x2) 
                                      (random-in-range y1 y2))))))

;;;exercise 3.6
(defvar random-init 1008611)
(defun rand ()
  (let ((state random-init))
    (lambda (mode)
      (cond ((equal mode 'generate) (random state))
            ((equal mode 'rest) 
             (lambda (new-value) 
               (setf state new-value) 
               state))
            (t (error "Unknown mode --RAND" mode))))))

;;;exercise 3.7
(defun make-joint (acc psd new-password)
  (if (equal t (funcall acc psd 'check-password))
    (lambda (password m)
      (if (equal password new-password)
        (funcall acc psd m)
        (error "Incorrect Password")))
    (error "Origin Incorrect Password")))

;;;exercise 3.8
(defvar last-n 0)
(defun f (n)
  (let ((r last-n))
    (setf last-n n)
    r))
