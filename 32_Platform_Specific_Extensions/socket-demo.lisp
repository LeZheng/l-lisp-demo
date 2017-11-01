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

(DEFUN wget-text (host page file &OPTIONAL (port 80))
       (WITH-OPEN-STREAM (socket (SOCKET:SOCKET-CONNECT port host :EXTERNAL-FORMAT :DOS))
                         (FORMAT socket "GET ~A HTTP/1.0~2%" page)
                         (WITH-OPEN-FILE (out file :direction :output)
                                         (LOOP :for line = (READ-LINE socket nil nil) :while line
                                               :do (WRITE-LINE line out)))))

(DEFUN wget-binary (host page file &OPTIONAL (port 80))
       (WITH-OPEN-STREAM 
         (socket (SOCKET:SOCKET-CONNECT port host :EXTERNAL-FORMAT :DOS))
         (FORMAT socket "GET ~A HTTP/1.0~2%" page)
         (LOOP :with content-length :for line = (READ-LINE socket nil nil)
               :until (ZEROP (LENGTH line)) :do
               (WHEN (STRING= line #1="Content-length: " :end1 #2=#.(LENGTH #1#))
                     (SETQ content-length (PARSE-INTEGER line :start #2#))
                     :finally (RETURN (LET ((data (MAKE-ARRAY content-length
                                                              :element-type '(UNSIGNED-BYTE 8))))
                                           (SETF (STREAM-ELEMENT-TYPE socket) '(UNSIGNED-BYTE 8))
                                           (EXT:READ-BYTE-SEQUENCE data socket)
                                           (WITH-OPEN-FILE (out file :direction :output
                                                                :ELEMENT-TYPE '(UNSIGNED-BYTE 8))
                                                           (EXT:WRITE-BYTE-SEQUENCE data out))
                                           data))))))

(DEFUN wput (host page file &OPTIONAL (port 80))
       (WITH-OPEN-STREAM 
         (socket (SOCKET:SOCKET-CONNECT port host :EXTERNAL-FORMAT :DOS))
         (WITH-OPEN-FILE 
           (in file :direction :inptut :ELEMENT-TYPE '(UNSIGNED-BYTE 8))
           (LET* ((length (FILE-LENGTH in))
                  (data (MAKE-ARRAY length :element-type '(UNSIGNED-BYTE 8))))
                 (FORMAT socket "PUT ~A HTTP/1.0~%Content-length: ~D~2%" page length)
                 (SETF (STREAM-ELEMENT-TYPE socket) '(UNSIGNED-BYTE 8))
                 (EXT:READ-BYTE-SEQUENCE data in)
                 (EXT:WRITE-BYTE-SEQUENCE data socket)))
         (SOCKET:SOCKET-STREAM-SHUTDOWN socket :output)
         (LOOP :for line = (READ-LINE socket nil nil) :while line :collect line)))
