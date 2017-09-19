;;;31.1. Customizing CLISP Process Initialization and Termination demo
(defun start ()
  (format t "start...~%"))

(defun end ()
  (format t "end...~%"))

(push #'start CUSTOM:*INIT-HOOKS*)
(push #'end CUSTOM:*FINI-HOOKS*)

(format t "hello~%")
(ext:saveinitmem "init-fini.mem" 
                 :executable t)

;;this file can be loaded in clisp to create init-fini.mem,and do ./init-fini.mem to test start and end
