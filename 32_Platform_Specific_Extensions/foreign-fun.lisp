;;;

(FFI:OPEN-FOREIGN-LIBRARY "libpthread.so.0");;open a shared library

(FFI:DEF-C-STRUCT foo ;;define a c struct
                  (a ffi:int) 
                  (b (ffi:c-array (ffi:c-ptr foo) 100)))


