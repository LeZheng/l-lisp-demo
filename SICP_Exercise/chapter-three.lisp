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
(defun cesaro-test ()
  (= (gcd (random 1000) (random 1000)) 1))

(defun estimate-po (trials)
  (sqrt (/ 6 (monte-carlo trials #'cesaro-test))))

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
  (labels ((my-loop (x y)
                 (if (null x)
                   y
                   (let ((temp (cdr x)))
                     (setf (cdr x) y)
                     (my-loop temp x)))))
    (my-loop x '())))
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
                                 (let ((new-pair (cons item (cons rear-ptr nil))))
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
                         nil)))
             (insert! (key-1 key-2 value)
                      (let ((subtable (my-assoc key-1 (cdr local-table))))
                        (if subtable
                          (let ((record (my-assoc key-2 (cdr subtable))))
                            (if record
                              (setf (cdr record) value)
                              (setf (cdr subtable) (cons (cons key-2 value)
                                                         (cdr subtable)))))
                          (setf (cdr local-table) (cons (list key-1 (cons key-2 value))
                                                        (cdr local-table)))))))
      (lambda (m)
        (cond ((eq m 'lookup-proc) #'lookup)
              ((eq m 'insert-proc) #'insert!)
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
                            (setf (cdr local-table) (cons (list key-1 (cons key-2 value))
                                                          (cdr local-table)))))))
        (lambda (m)
          (cond ((eq m 'lookup-proc) #'lookup)
                ((eq m 'insert-proc) #'insert!)
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


;;;;3.3.4
(defun make-wire ()
  (let ((signal-value 0) (action-procedures '()))
    (labels ((set-my-signal! (new-value)
                             (if (not (= signal-value new-value))
                               (progn (setf signal-value new-value)
                                      (call-each action-procedures))
                               'done))
             (accept-action-procedure! (proc)
                                       (setf action-procedures (cons proc action-procedures))
                                       (funcall proc))
             (dispatch (m)
                       (cond ((eql m 'get-signal) #'signal-value)
                             ((eql m 'set-signal) #'set-my-signal!)
                             ((eql m 'add-action!) #'accept-action-procedure!)
                             (t (error "Unknown operation -- WIRE" m)))))
      #'dispatch)))

(defun call-each (procedures)
  (if (null procedures)
    'done
    (progn
      (funcall (car procedures) (call-each (cdr procedures))))))

(defun get-signal (wire)
  (funcall wire 'get-signal))
(defun set-signal! (wire new-value)
  (funcall (funcall wire 'set-signal!) new-value))
(defun add-action! (wire action-procedure)
  (funcall (funcall wire 'add-action!) action-procedure))

(defun logical-or (a1 a2)
  (cond ((or (= a1 1) (= a2 1)) 1)
	((and (= a1 0) (= a2 0)) 0)
        (t (error "Invalid signal" a1 a2))))

(defun logical-and (a1 a2)
  (cond ((and (= a1 1) (= a2 1) 1))
        ((or (= a1 0) (= a2 0)) 0)
        (t (error "Invalid signal" a1 a2))))

(defun logical-not (s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (t (error "Invalid signal" s))))

(defun inverter (input output)
  (labels ((invert-input ()
                         (let ((new-value (logical-not (get-signal input))))
                           (after-delay inverter-delay
                                        (lambda ()
                                          (set-signal! output new-value))))))
    (add-action! input invert-input)
    'ok))

(defun and-gate (a1 a2 output)
  (labels ((and-action-procedure ()
                                 (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
                                   (after-delay and-gate-delay
                                                (lambda ()
                                                  (set-signal! output new-value))))))
    (add-action! a1 and-action-procedure)
    (add-action! a2 and-action-procedure)
    'ok))

;;;exercise 3.28
(defun or-gate (a1 a2 output)
  (labels ((or-action-procedure ()
                                (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
                                  (after-delay or-gate-delay
                                               (lambda ()
                                                 (set-signal! output new-value))))))
    (add-action! a1 or-action-procedure)
    (add-action! a2 or-action-procedure)
    'or))

;;;exercise 3.29
(defun or-gate-1 (a1 a2 output)
  (let ((invert-1 (make-wire))
        (invert-2 (make-wire))
        (and-invert-1-invert-2 (make-wire)))
    (inverter a1 invert-1)
    (inverter a2 invert-2)
    (and-gate a1 a2 and-invert-1-invert-2)
    (inverter and-invert-1-invert-2))
  'ok)
;时间为 3*invert-delay + and-gate-delay

;;;exercise 3.30
(defun half-adder (a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(defun full-adder (a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(defun ripple-carry-adder (as bs ss c)
  (let ((c-out (make-wire)))
    (full-adder (car as) (car bs) c (car ss) c-out)
    (if (null (cdr as))
      'ok
      (ripple-carry-adder (cdr as) (cdr bs) (cdr ss) c-out))))

(defun after-delay (delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(defun propagate ()
  (if (empty-agenda? the-agenda)
    'done
    (let ((first-item (first-agenda-item the-agenda)))
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (propagate))))

(defun probe (name wire)
  (add-action! wire
               (lambda ()
                 (format t "~% ~A ~A New-value = ~A" 
                   name 
                   (l-current-time the-agenda)
                   (get-signal wire)))))

(defun make-time-segment (time queue)
  (cons time queue))

(defun segment-time (s) (car s))
(defun segment-queue (s) (cdr s))

(defun make-agenda ()
  (list 0))
(defun l-current-time (agenda) (car agenda))
(defun set-current-time! (agenda time)
  (setf (car agenda) time))
(defun segments (agenda) (cdr agenda))
(defun set-segments! (agenda segments)
  (setf (cdr agenda) segments))

(defun first-segment (agenda) (car (segments agenda)))
(defun rest-segments (agenda) (cdr (segments agenda)))

(defun empty-agenda? (agenda)
  (null (segments agenda)))

;TODO uncomplete labels 
(defun add-to-agenda! (time action agenda)
  (labels ((belongs-before? (segments)
                            (or (null segments) (< time (segment-time (car (segments))))))
           (make-new-time-segment (time action)
                                  (let ((q (make-queue)))
                                    (insert-queue! q action)
                                    (make-time-segment time q)))
           (add-to-segments! (segments)
                             (if (= (segment-time (car segments)) time)
                               (insert-queue! (segment-queue (car segments)) action)
                               (let ((rest (cdr segments)))
                                 (if (belongs-before? rest)
                                   (setf (cdr segments) (cons (make-new-time-segment time action) (cdr segments)))
                                   (add-to-segments! rest))))))
    (let ((segments (segments agenda)))
      (if (belongs-before? segments)
        (set-segments! agenda (cons (make-new-time-segment time action) segments))
        (add-to-segments! segments)))))

(defun remove-first-agenda-item! (agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))

(defun first-agenda-item (agenda)
  (if (empty-agenda? agenda)
    (error "agenda is empty -- FIRST-AGENDA-ITEM")
    (let ((first-seg (first-segment agenda)))
      (set-current-time! agenda (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))

;;;exercise 3.32 TODO

;;;;3.3.5

(defun celsius-fahrenheit-conerter (c f)
  (let ((u (make-connector))
	(v (make-connector))
	(w (make-connector))
	(x (make-connector))
	(y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(defun adder (a1 a2 sum)
  (let ((me nil))
    (labels ((process-new-value ()
	       (cond
		 ((and (has-value? a1) (has-value? a2))
		  (set-value! sum
			      (+ (get-value a1) (get-value a2))
			      me))
		 ((and (has-value? a1) (has-value? sum))
		  (set-value! a2
			      (- (get-value sum) (get-value a1))
			      me))
		 ((and (has-value? a2) (has-value? sum))
		  (set-value! a1
			      (- (get-value sum) (get-value a2))
			      me)))))
      (labels ((process-forget-value ()
		 (forget-value! sum me)
		 (forget-value! a1 me)
		 (forget-value! a2 me)
		 (process-new-value)))
	(setf me (lambda (request)
		   (cond ((eql request 'I-have-a-value)
			  (process-new-value))
			 ((eql request 'I-lost-my-value)
			  (process-forget-value))
			 (t (error "Unknown request -- ADDER" request)))))
	(connect a1 me)
	(connect a2 me)
	(connect sum me)
	me))))

(defun inform-about-value (constraint)
  (funcall constraint 'I-have-a-vlaue))
(defun inform-about-no-value (constraint)
  (funcall constraint 'I-lost-my-value))

(defun multiplier (m1 m2 product)
  (let ((me nil))
    (labels ((process-new-value ()
	       (cond ((or (and (has-value? m1) (= (get-value m1) 0))
			  (and (has-value? m2) (= (get-value m2) 0)))
		      (set-value! product 0 me))
		     ((and (has-value? m1) (has-value? m2))
		      (set-value! product
				  (* (get-value m1) (get-value m2))
				  me))
		     ((and (has-value? product) (has-value? m1))
		      (set-value! m2
				  (/ (get-value product) (get-value m1))
				  me))
		     ((and (has-value? product) (has-value? m2))
		      (set-value! m1
				  (/ (get-value product) (get-value m2))
				  m2)))))
      (labels ((process-forget-value ()
		 (forget-value! product me)
		 (forget-value! m1 me)
		 (forget-value! m2 me)
		 (process-new-value)))
	(setf me (lambda (request)
		   (cond ((eql request 'I-have-a-vlaue) (process-new-value))
			 ((eql request 'I-lost-my-value) (process-forget-value))
			 (t (error "Unknown request -- MULTIPLIER" request)))))
	(connect m1 me)
	(connect m2 me)
	(connect product me)
	me))))

(defun constraint (value connector)
  (let ((me (lambda (request)
	      (error "Unknown request -- CONSTANT" request))))
    (connect connector me)
    (set-value! connector value me)
    me))

(defun probe (name connector)
  (labels ((print-probe (value)
	     (format t "~% Probe:~A = ~A" name value)))
    (let ((me nil))
      (labels ((process-new-value ()
		 (print-probe (get-value connector)))
	       (process-forget-value ()
		 (print-probe "?")))
	(setf me (lambda (request)
		   (cond ((eql request 'I-have-a-vlaue) (process-new-value))
			  ((eql request 'I-lost-my-value) (process-forget-value))
			  (t (error "Unknown request -- PROBE" request)))))
	(connect connector me)
	me))))

(defun make-connector ()
  (let ((value false) (informant false) (constraints '()) (me nil))
    (labels ((set-my-value (newval setter)
	       (cond ((not (has-value? me))
		      (setf value newval)
		      (setf informant setter)
		      (for-each-except setter #'inform-about-value constraints))
		     ((not (- value newval))
		      (error "Contradiction" (list value newval)))
		     (t 'ignored)))
	     (forget-my-value (retractor)
	       (if (eql retractor informant)
		   (progn (setf informant nil)
			  (for-each-except retractor #'inform-about-no-value constraints))
		   'ignored))
	     (connect (new-constraint)
	       (if (not (member new-constraint constraints)) ;member memq ?
		   (setf constraints
			 (cons new-constraint constraints)))
	       (if (has-value? me)
		   (inform-about-value new-constraint))
	       'done))
      (setf me (lambda (request)
		 (cond ((eql request 'has-value?)
			(if informant t nil))
		       ((eql request 'value) value)
		       ((eql request 'set-value!) (lambda (n s) (set-my-value n s)))
		       ((eql request 'forget) (lambda (r) (forget-my-value r)))
		       ((eql request 'connect) (lambda (n) (connect n)))
		       (t (error "Unknown operation -- CONNECTOR" request)))))
      me)))

(defun for-each-except (exception procedure list)
  (labels ((inner-loop (items)
	     (cond ((null items) 'done)
		   ((eql (car items) exception) (inner-loop (cdr items)))
		   ((t (funcall procedure (car items))
		       (inner-loop (cdr items)))))))
    (inner-loop list)))

(defun has-value? (connector)
  (funcall connector 'has-value?))
(defun get-value (connector)
  (funcall connector 'value))
(defun set-value! (connector new-value informant)
  (funcall (funcall connector 'set-value!) new-value informant))
(defun forget-value! (connector retractor)
  (funcall (funcall connector 'forget) retractor))
(defun connect (connector new-constraint)
  (funcall (funcall connector 'connect) new-constraint))

;;;exercise 3.33
(defun averager (a b c)
  (let ((sum (make-connector))
	(d (make-connector)))
    (adder a b sum)
    (multiplier sum d c)
    (constant (/ 1 2) d)
    'ok))

;;;exercise 3.34
;可以从a得到b，但是无法从b得到a，只有b有值的情况下，multiplier无法得到它的两个a引线的值。

;;;exercise 3.35
(defun squarer (a b)
  (let ((me nil))
    (labels ((process-new-value ()
	       (if (has-value? b)
		   (if (< (get-value b) 0)
		       (error "square less than 0 -- SQUARER" (get-value b))
		       (set-value! a (sqrt (get-value b)) me))
		   (if (has-value? a)
		       (set-value! b (expt (get-value a) 2) me)
		       (error "Neither a nor b has value"))))
	     (process-forget-value ()
	       (forget-value! a me)
	       (forget-value! b me)))
      (setf me (lambda (request)
		 (cond ((eql request 'I-have-a-vlaue) (process-new-value))
		       ((eql request 'I-lost-my-value) (process-forget-value))
		       (t (error "Unknown request -- MULTIPLIER" request)))))
      (connect a me)
      (connect b me)
      me)))

;;;exercise 3.36 3.37 TODO

;;;;3.4
;;;exercise 3.38
;a 60 35 50 45
;b 略

;;;exercise 3.39
;100还会出现

;;;exercise 3.40
;;a) 1000000 100000 10000
;;b) 1000000

;;;exercise 3.41
;;不同意，不存在

;;;exercise 3.42-3.49
;;TODO

;;;;3.5

(defparameter the-empty-stream '())

(defun memo-proc (proc)
  (let ((already-run? nil) (result nil))
    (lambda ()
      (if (not already-run?)
	  (progn (setf result (funcall proc))
		 (setf already-run? t)
		 result)
	  result))))

(defun stream-null? (stream)
  (null stream))

(defmacro delay (exp)
  `(memo-proc (lambda () ,exp)))
(defun force (delayed-object)
  (funcall delayed-object))
(defmacro cons-stream (a b)
  `(cons ,a (delay ,b)))
(defun stream-car (stream)
  (car stream))
(defun stream-cdr (stream)
  (force (cdr stream)))

(defun stream-ref (s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(defun stream-filter (pred stream)
  (cond ((stream-null? stream) the-empty-stream)
	((funcall pred (stream-car stream))
	 (cons-stream (stream-car stream)
		      (stream-filter pred
				     (stream-cdr stream))))
	(t (stream-filter pred (stream-cdr stream)))))

(defun stream-for-each (proc s)
  (if (stream-null? s)
      'done
      (progn (funcall proc (stream-car s))
	     (stream-for-each proc (stream-cdr s)))))

(defun display-stream (s)
  (stream-for-each #'print s))

(defun stream-enumerate-interval (low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

;;;exercise 3.50
(defun stream-map (proc &rest argstreams)
  (if (null (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (mapcar #'stream-car argstreams))
       (apply #'stream-map (cons proc (mapcar #'stream-cdr argstreams))))))

;;;exercise 3.51
;;0 1 2 3 4 5 6 7 8 9 10
;;5
;;7

;;;exercise 3.52
;;seq执行完是 210
;;y 执行完是 210
;;z 执行完是 210
;;stream-ref 执行完是210 ，执行打印136
;;display-stream 打印 10 15 45 55 105 120 190 210 DONE
;;并不总是210

;;;exercise 3.53
;;1 2 4 8 16 ...

(defun add-streams (s1 s2)
  (stream-map #'+ s1 s2))

(defun mul-streams (s1 s2)
  (stream-map #'* s1 s2))

(setf ones (cons-stream 1 ones))

(setf integers (cons-stream 1 (add-streams ones integers)))
;;;exercise 3.54
(setf factorials (cons-stream 1 (mul-streams (stream-cdr integers) factorials)))

;;;exercise 3.55
(defun partial-sums (s)
  (cons-stream (stream-car s) (add-streams (partial-sums s) (stream-cdr s))))

;;;exercise 3.56
(defun scale-stream (s factor)
  (stream-map (lambda (x) (* x factor)) s))

(defun merge-stream (s1 s2)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(t
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
	   (cond ((< s1car s2car)
		  (cons-stream s1car (merge-stream (stream-cdr s1) s2)))
		 ((> s1car s2car)
		  (cons-stream s2car (merge-stream s1 (stream-cdr s2))))
		 (t
		  (cons-stream s1car
			       (merge-stream (stream-cdr s1)
				      (stream-cdr s2)))))))))

(setf S (cons-stream 1 (merge-stream (scale-stream S 2) (merge-stream (scale-stream S 3)
								      (scale-stream S 5)))))

;;;exercise 3.57
;;带有记忆过程，所以是n次加法。不带记忆过程的时候就是指数。

;;;exercise 3.58
(defun expand (num den radix)
  (cons-stream
   (floor (* num radix) den)
   (expand (mod (* num radix) den) den radix)))
;;expand 1 7 10 => 1 4 2 8 5 7 ...
;;expand 3 8 10 => 7 5 0 0 0 0 ...

;;;exercise 3.59
;;a)
(defun integrate-series (a)
  (mul-streams a (stream-map #'/ ones integers)))
;;b) TODO

;;;exercise 3.60 3.61 3.62 TODO

;;;;3.5.3
(defun average (a b)
  (/ (+ a b) 2))

(defun sqrt-improve (guess x)
  (average guess (/ x guess)))

(defun sqrt-stream (x)
  (let ((guesses nil))
    (setf guesses (cons-stream 1.0
			       (stream-map (lambda (guess)
					     (sqrt-improve guess x))
					   guesses)))
    guesses))

(defun pi-summands (n)
  (cons-stream (/ 1.0 n)
	       (stream-map #'- (pi-summands (+ n 2)))))

(setf pi-stream (scale-stream (partial-sums (pi-summands 1)) 4))

;;;exercise 3.63
;;一个是沿用前面的结果来计算流的下一个元素，而另一个是每次都是新的流，复杂度分别为O(n)和O(n^2)
;;不带memo-proc的话效率一样。

;;;exercise 3.64
(defun stream-limit (s tolerance)
  (let ((a (stream-car s))
	(b (stream-car (stream-cdr s))))
    (if (< (abs (- x y)) tolerance)
	b
	(stream-limit (stream-cdr s) tolerance)))) 

;;;exercise 3.65
(defun ln2-stream (n)
  (cons-stream (/ 1 n)
	       (stream-map #'- (ln2-stream (+ n 1)))))
(setf ln2 (partial-sums (ln2-stream 1)))

;;;;
(defun interleave (s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
		   (interleave s2 (stream-cdr s1)))))

(defun pairs (s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
		(stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

;;;exercise 3.66 TODO

;;;exercise 3.67
(defun pairs-2 (s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave
     (stream-map (lambda (x) (list (stream-car s) x))
		 (stream-cdr t))
     (stream-map (lambda (x) (list (stream-car t) x))
		 (stream-cdr s)))
    (pairs (stream-cdr s) (stream-cdr t)))))

;;;exercise 3.68 TODO

;;;exercise 3.69
(defun triples (s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) (car x) (cadr x)))
		(pairs (stream-cdr t) (stream-cdr u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))
;;TODO 毕达哥拉斯三元组

;;;exercise 3.70 TODO

;;;exercise 3.71 TODO

;;;exercise 3.72 TODO

;;;exercise 3.73
(defun integral (integrand initial-value dt)
  (let ((int nil))
    (setf int (cons-stream initial-value
			   (add-streams (scale-stream integrand dt) int)))
    int))

(defun rc (r c dt)
;;TODO
  )

;;;exercise 3.74
;;(setf zero-crossings (stream-map #'sign-change-detector sense-data (cons-stream 0 sense-data)))

;;;exercise 3.75 TODO

;;;exercise 3.76 TODO

;;;exercise 3.77
(defun integral-1 (delay-integrand initial-value dt)
  (cons-stream
   initial-value
   (if (stream-null? (force delay-integrand))
       the-empty-stream
       (integral-1 (stream-cdr (force delay-integrand))
		   (+ (* dt (stream-car (force delay-integrand)))
		      initial-value)
		   dt))))

;;;exercise 3.78
(defun solve-2nd (a b dt y0 dyo dydt)
  ;;TODO
  )

;;;exercise 3.79 TODO

;;;exercise 3.80 TODO
  
