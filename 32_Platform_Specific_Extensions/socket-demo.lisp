;;;There are some socket examples
(defun start-read-tcp-server (port) 
  (LET ((server (SOCKET:SOCKET-SERVER port)))
       (FORMAT t "~&Waiting for a connection on ~S:~D~%"
               (SOCKET:SOCKET-SERVER-HOST server) (SOCKET:SOCKET-SERVER-PORT server))
       (UNWIND-PROTECT 
         (LOOP (WITH-OPEN-STREAM (socket (SOCKET:SOCKET-ACCEPT server))
                                 (MULTIPLE-VALUE-BIND (local-host local-port) (SOCKET:SOCKET-STREAM-LOCAL socket)
                                                      (MULTIPLE-VALUE-BIND (remote-host remote-port) (SOCKET:SOCKET-STREAM-PEER socket)
                                                                           (FORMAT T "~&Connection: ~S:~D -- ~S:~D~%"
                                                                                   remote-host remote-port local-host local-port)))
                                 (LOOP (WHEN (EQ :eof (SOCKET:SOCKET-STATUS (cons socket :input))) (RETURN))
                                       (format t "~A~%" (read-line socket)))))
         (SOCKET:SOCKET-SERVER-CLOSE server)))) 
