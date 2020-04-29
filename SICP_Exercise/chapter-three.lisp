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


;;;;3.3.2
(defun front-ptr (queue)
  (car queue))
(defun rear-ptr (queue)
  (cdr queue))
(defun set-front-ptr! (queue item)
  (setf (car queue) item))
(defun set-rear-ptr! (queue item)
  (setf (cdr queue) item))
(defun empty-queue? (queue)
  (null (front-ptr queue)))
(defun make-queue ()
  (cons '() '()))
(defun front-queue (queue)
  (if (empty-queue? queue)
    (error "FRONT called with an empty queue" queue)
    (car (front-ptr queue))))

(defun insert-queue! (queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (t
            (setf (cdr (rear-ptr queue)) new-pair)
            (set-rear-ptr! queue new-pair)
            queue))))

(defun delete-queue! (queue)
  (if (empty-queue? queue)
    (error "DELETE called with an empty queue" queue)
    (progn 
      (set-front-ptr! queue (cdr (front-ptr queue)))
      queue)))

;;;exercise 3.21
;front-ptr开始的都清空了，但是rear-ptr没有清空
(defun print-queue (queue)
  (format t "~A~%" (front-ptr queue)))

;;;exercise 3.22
(defun make-queue ()
  (let ((front-ptr '())
        (rear-ptr '()))
    (labels ((empty-queue? () (null front-ptr))
             )
      (lambda (m)
        (case m
          ('front-ptr front-ptr)
          ('rear-ptr rear-ptr)
          ('empty-queue? (empty-queue?))
          ('insert-queue! (lambda (item) 
                            (let ((new-pair (cons item '())))
                              (cond ((empty-queue?)
                                     (setf front-ptr new-pair)
                                     (setf rear-ptr new-pair)
                                     front-ptr)
                                    (t 
                                      (setf (cdr rear-ptr) new-pair)
                                      (setf rear-ptr new-pair)
                                      front-ptr)))))
          ('delete-queue! (if (empty-queue?)
                            (error "DELETE called with an empty queue")
                            (progn
                              (setf front-ptr (cdr front-ptr))
                              front-ptr))))))))

;;;exercise 3.23
(defun make-deque ()
  (let ((front-ptr '())
        (rear-ptr '()))
    (labels ((empty-deque? () (null front-ptr)))
      (lambda (m)
        (case m
          ('front-deque front-ptr)
          ('rear-deque rear-ptr)
          ('empty-deque? (empty-deque?))
          ('front-insert-deque! (lambda (item)
                                  (let ((new-pair (cons item (cons nil front-ptr))))
                                    (cond ((empty-deque?)
                                           (setf front-ptr new-pair)
                                           (setf rear-ptr new-pair)
                                           (setf (cadr rear-ptr) new-pair)
                                           (setf (cddr front-ptr) new-pair)
                                           front-ptr)
                                          (t
                                            (setf (cadr front-ptr) new-pair)
                                            (setf front-ptr new-pair)
                                            front-ptr)))))
          ('rear-insert-deque! (lambda (item)
                                 (let (new-pair (cons item (cons rear-ptr nil)))
                                   (cond ((empty-deque?)
                                          (setf front-ptr new-pair)
                                          (setf rear-ptr new-pair)
                                          (setf (cadr rear-ptr) new-pair)
                                          (setf (cddr front-ptr) new-pair)
                                          front-ptr)
                                         (t 
                                           (setf (cddr rear-ptr) new-pair)
                                           (setf rear-ptr new-pair)
                                           front-ptr)))))
          ('front-delete-deque! (when (not (empty-deque?))
                                  (setf front-ptr (cddr front-ptr))
                                  (setf (cadr front-ptr) nil)
                                  front-ptr))
          ('rear-delete-deque! (when (not (empty-deque?))
                                 (setf rear-ptr (cadr rear-ptr))
                                 (setf (cddr rear-ptr) nil)
                                 front-ptr)))))))

;;;;3.3.3
(defun lookup (key table)
  (let ((record (my-assoc key (cdr table))))
    (if record
      (cdr record)
      nil)))
(defun my-assoc (key records)
  (cond ((null records) nil)
        ((equal key (caar records)) (car records))
        (t (my-assoc key (cdr records)))))
(defun insert! (key value table)
  (let ((record (my-assoc key (cdr table))))
    (if record
      (setf (cdr record) value)
      (setf (cdr table) (cons (cons key value) (cdr table))))))
(defun make-table ()
  (list '*table*))

(defun lookup (key-1 key-2 table)
  (let ((subtable (my-assoc key-1 (cdr table))))
    (if subtable
      (let ((record (my-assoc key-2 (cdr subtable))))
        (if record
          (cdr record)
          nil))
      nil)))
(defun insert! (key-1 key-2 value table)
  (let ((subtable (my-assoc key-1 (cdr table))))
    (if subtable
      (let ((record (my-assoc key-2 (cdr subtable))))
        (if record
          (setf (cdr record) value)
          (setf (cdr subtable) (cons (cons key-2 value) (cdr subtable)))))
      (setf (cdr table)
            (cons (list key-1 (cons key-2 value))
                  (cdr table))))))

(defun make-table ()
  (let ((local-table (list '*table*)))
    (labels ((lookup (key-1 key-2)
                     (let ((subtable (my-assoc key-1 (cdr local-table))))
                       (if subtable
                         (let ((record (my-assoc key-2 (cdr subtable))))
                           (if record
                             (cdr record)
                             nil))
                         (nil))))
             (insert! (key-1 key-2 value)
                      (let ((subtable (my-assoc key-1 (cdr local-table))))
                        (if subtable
                          (let ((record (my-assoc key-2 (cdr subtable))))
                            (if record
                              (setf (cdr record) value)
                              (setf (cdr subtable) (cons (cons key-2 value)
                                                         (cdr subtable)))))
                          (setf (cdr local-table) (cons (list key-1 (cons key2 value))
                                                        (cdr local-table)))))))
      (lambda (m)
        (cond ((eq m 'lookup-proc) #'lookup)
              ((eq m 'insert-proc #'insert!))
              (t (error "Unknown operation -- TABLE" m)))))))

;;;exercise3.24
(defun make-table-1 (same-key?)
  (let ((local-table (list '*table*)))
    (labels ((my-assoc (key records)
                       (cond ((null records) nil)
                             ((funcall same-key? key (caar records)) (car records))
                             (t (my-assoc key (cdr records))))))
      (labels ((lookup (key-1 key-2)
                       (let ((subtable (my-assoc key-1 (cdr local-table))))
                         (if subtable
                           (let ((record (my-assoc key-2 (cdr subtable))))
                             (if record
                               (cdr record)
                               nil))
                           nil)))
               (insert! (key-1 key-2 value)
                        (let ((subtable (my-assoc key-1 (cdr local-table))))
                          (if subtable
                            (let ((record (my-assoc key-2 (cdr subtable))))
                              (if record
                                (setf (cdr record) value)
                                (setf (cdr subtable) (cons (cons key-2 value)
                                                           (cdr subtable)))))
                            (setf (cdr local-table) (cons (list key-1 (cons key2 value))
                                                          (cdr local-table)))))))
        (lambda (m)
          (cond ((eq m 'lookup-proc) #'lookup)
                ((eq m 'insert-proc #'insert!))
                (t (error "Unknown operation -- TABLE" m))))))))

;;;exercise 3.25
(defun make-table-n ()
  (let ((local-table (list '*table*)))
    (labels ((lookup (keys table)
                     (let ((record (my-assoc (car keys) (cdr table))))
                       (if record
                         (if (null (cdr keys))
                           record
                           (lookup (cdr keys) record))
                         nil)))
             (insert! (keys value table)
                      (let ((record (my-assoc (car keys) (cdr table))))
                        (if record
                          (insert! (cdr keys) value (cdr record))
                          (if (null (cdr keys))
                            (setf (cdr table) (cons (cons (car keys) value) (cdr table)))
                            (setf (cdr table) (cons (cons (car keys) (insert! (cdr keys) value (cdr table)))
                                                    (cdr table))))))))
      (lambda (m)
        (cond ((eq m 'lookup-proc) #'lookup)
              ((eq m 'insert-proc) #'insert!)
              (t (error "Unknown operation -- TABLE" m)))))))

;;;exercise 3.26
;TODO 

;;;exercise 3.27
;对于每个n只计算一次
;不能正常工作因为调用的是fib
