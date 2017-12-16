;;;This demo is to run a shell command and get result to string
(defun sh (cmd)
  (let ((res-str nil) 
        (res-code nil)
        (buf-stream (ext:run-shell-command cmd :output :stream)))
    (loop for c = (read-char buf-stream nil) while c do (push c res-str))
    (values (concatenate 'string (reverse res-str)) res-code)))
