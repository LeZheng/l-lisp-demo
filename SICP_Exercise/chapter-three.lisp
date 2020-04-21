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

;;;exercise 3.9 3.10 3.11
;;略


;;;exercise 3.12
(defun last-pair (x)
  (if (null (cdr x))
    x
    (last-pair (cdr x))))

(defun append! (x y)
  (setf (cdr (last-pair x)) y)
  x)
;;第一个是(B)
;;第二个是(B C D)

;;;exercise 3.13
(defun make-cycle (x)
  (setf (cdr (last-pair x)) x)
  x)
;;死循环

;;;exercise 3.14
(defun mystery (x)
  (labels ((loop (x y)
                 (if (null x)
                   y
                   (let ((temp (cdr x)))
                     (setf (cdr x) y)
                     (loop temp x)))))
    (loop x '())))
;;w => (d c b a)
;;v => (a)

;;exercise 3.15 略

;;;exercise 3.16
;没有考虑共享和循环的情况，因此是错误的
; '((1 3) 2 3) => 4 其中3共享
; '((1 2 3) 2 3) => 7 其中 2 3 共享

;;;exercise 3.17
(defun count-pairs (x)
  (let ((pairs '()))
    (labels ((count-p (x)
                      (if (position x pairs)
                        0
                        (progn
                          (push x pairs)
                          (if (consp x)
                            (1+ (count-p (cdr x)))
                            0)))))
      (count-p x))))

;;;exercise 3.18
(defun is-loop (x)
  (let ((pairs '()))
    (labels ((has-loop? (x)
                        (cond
                          ((null x) nil)
                          ((position x pairs) t)
                          (t (progn
                               (push x pairs)
                               (has-loop? (cdr x)))))))
      (has-loop? x))))

;;;exercise 3.19
(defun is-loop-1 (x)
  (labels ((iter (x1 x2)
                 (if (and (not (null x1)) (eq x1 x2))
                   t
                   (if (null x2)
                     nil
                     (iter (cdr x1) (cddr x2))))))
    (iter x (cdr x))))

;;;exercise 3.20 略
