;;function-keywords
(defmethod gf1 ((a integer)
		&optional (b 2)
		&key (c 3) ((:dee d) 4) e ((eff f)))
  (list a b c d e f))
(function-keywords (find-method #'gf1 '() (list (find-class 'integer))))

;;change-class
(defclass my-position() ())
(defclass x-y-position (my-position)
  ((x :initform 0 :initarg :x)
   (y :initform 0 :initarg :y)))
(defclass r-t-position (my-position)
  ((rho :initform 0)
   (theta :initform 0)))
(defmethod update-instance-for-different-class :before ((old x-y-position)
							(new r-t-position)
							&key)
  (let ((x (slot-value old 'x))
	(y (slot-value old 'y)))
    (setf (slot-value new 'rho) (sqrt (+ (* x x) (* y y)))
	  (slot-value new 'theta) (atan y x))))
(setq p1 (make-instance 'x-y-position :x 2 :y 0))
(change-class p1 'rho-theta-position)
;;slot-boundp
(slot-boundp p1 'rho)
;;slot-exists-p
(slot-exists-p p1 'rhoxxx)
;;slot-makunbound
(slot-makunbound p1 'rho)
;;slot-value
(setf (slot-value p1 'rho) 33)
(print (slot-value p1 'rho))
;;make-instance
(defclass person ()
  ((name :initform "" :initarg :name)
   (age :initform 0 :initarg :age)))
(make-instance 'person :name "colin" :age 11)
;;make-load-form
(defclass pos ()
  ((x :initarg :x :accessor pos-x)
   (y :initarg :y :accessor pos-y)))
(defmethod make-load-form ((self pos) &optional env)
  (declare (ignore env))
  `(make-instance ',(class-of self)
		  :x ',(pos-x self) :y ',(pos-y self)))
(setf p1 (make-instance 'pos :x 3 :y 4))
(make-load-form p1)
;;with-accessors
(with-accessors ((x pos-x) (y pos-y)) p1
		(setf x 6 y 8)
		(list x y))
;;with-slots
(with-slots (x y) p1
  (setf x 33 y 44)
  (list x y))
;;defclass
(defclass point ()
  ((x :initarg :x :accessor p-x
      :documentation "This is x of point" :allocation :instance :type number)
   (y :initarg :y :accessor p-y :initform 0
      :documentation "This is y of point" :allocation :instance :type number)
   (name :initarg :name :initform "unknown" :reader get-name :writer set-name)
   (count :initform 0 :accessor p-count :allocation :class))
  (:documentation "This is a point")
  (:default-initargs . (:x 0 :y 0 :name "default")))
(defmethod initialize-instance :after ((instance point) &rest initargs &key &allow-other-keys)
  (with-slots (count) instance
	     (incf count)))
