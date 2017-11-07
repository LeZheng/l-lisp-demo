;;;
(EXT:GETENV) ;;;get all environment variables

(EXT:GETENV "PATH") ;;get PATH environment variable 

(SETF (EXT:GETENV "TIME") (format nil "~A" (get-universal-time)))

(EXT:GETENV "TIME")
