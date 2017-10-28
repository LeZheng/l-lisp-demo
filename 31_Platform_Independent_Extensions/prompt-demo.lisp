;;;clisp prompt demo
;;;Please load this file in clisp to create memory file,then use ./prompt-demo.mem to start it

(setf CUSTOM:*PROMPT-START* "admin")
(setf CUSTOM:*PROMPT-FINISH* "->")
(setf CUSTOM:*PROMPT-BODY* #'(lambda () (format nil "[~A]" (get-internal-real-time))))

(ext:saveinitmem "prompt-demo.mem" :executable t)

(exit)
