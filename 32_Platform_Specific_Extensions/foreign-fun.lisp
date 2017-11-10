;;;

(FFI:OPEN-FOREIGN-LIBRARY "libpthread.so.0");;open a shared library

(FFI:DEF-C-STRUCT foo ;;define a c struct
                  (a ffi:int) 
                  (b (ffi:c-array (ffi:c-ptr foo) 100)))

(declare (type foo f))
(foo-a (aref (foo-b f) 7))

(FFI:DEF-C-STRUCT bar
  (x ffi:short)
  (y ffi:short)
  (a ffi:char)
  (b ffi:char)     ; or (b character) if it represents a character, not a number
  (z ffi:int)
  (n (ffi:c-ptr bar)))

(FFI:DEF-C-VAR my_struct (:type (ffi:c-ptr bar)))
(setq my_struct (let ((s my_struct)) (incf (slot-value s 'x)) s))     ; or
(incf (slot my_struct 'x))
(setq my_struct (let ((s my_struct)) (setf (slot-value s 'a) 5) s))     ; or
(setf (slot my_struct 'a) 5)
(setq my_struct (slot-value my_struct 'n))     ; or
(setq my_struct (deref (slot my_struct 'n)))
