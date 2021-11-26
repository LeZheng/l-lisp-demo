(make-package 'temp :use nil)
(use-package 'temp)
(intern "TEMP-SYM" 'temp)
(find-symbol "TEMP-SYM")
(export (find-symbol "TEMP-SYM" 'temp) 'temp)
(find-symbol "TEMP-SYM")

(find-package "COMMON-LISP")
(find-package 'common-lisp)

(find-all-symbols 'car)

(intern "CAR" 'temp)
(find-all-symbols 'car)

(import 'common-lisp::car 'temp)
(find-symbol "CAR" 'temp)
(find-symbol "CDR" 'temp)

(delete-package 'temp)

(make-package 'temporary :nicknames '("TEMP"))
(rename-package 'temp 'ephemeral '("ephe"))

(make-package 'temp)
(package-shadowing-symbols 'temp)
(shadow 'car 'temp)
(find-symbol "CAR" 'temp)
(package-shadowing-symbols 'temp)

(in-package "COMMON-LISP-USER") ;=>  #<PACKAGE "COMMON-LISP-USER">
(setq sym (intern "CONFLICT")) ;=>  CONFLICT
(intern "CONFLICT" (make-package 'temp)) ;=>  TEMP::CONFLICT, NIL
(package-shadowing-symbols 'temp) ; =>  NIL
(shadowing-import sym 'temp); =>  T 
(package-shadowing-symbols 'temp) ;=>  (CONFLICT)

(make-package 'temporary :nicknames '("TEMP1" "temp1") :use "CL-USER")

(defun print-all-symbols (package) 
  (with-package-iterator (next-symbol (list package)
                          :internal :external)
    (loop
      (multiple-value-bind (more? symbol) (next-symbol)
        (if more? 
            (print symbol)
          (return))))))

(export (intern "CONTRABAND" 'temp) 'temp)
(unexport 'contraband 'temp)

(setq temps-unpack (intern "UNPACK" 'temp))
(unintern temps-unpack 'temp)
(find-symbol "UNPACK" 'temp)

(defpackage "MY-PACKAGE"
  (:nicknames "MYPKG" "MY-PKG")
  (:use "COMMON-LISP")
  (:shadow "CAR" "CDR")
;  (:shadowing-import-from "VENDOR-COMMON-LISP"  "CONS")
;  (:import-from "VENDOR-COMMON-LISP"  "GC")
  (:export "EQ" "CONS" "FROBOLA"))

(let ((lst ()))
  (do-symbols (s (find-package "MY-PACKAGE")) (push s lst))
  lst)

(let ((lst ()))
  (do-external-symbols (s (find-package 'my-package) lst) (push s lst))
  lst) 

(let ((lst ()))                                                     
  (do-all-symbols (s lst)
    (when (eq (find-package 'my-package) (symbol-package s)) (push s lst)))
  lst)
