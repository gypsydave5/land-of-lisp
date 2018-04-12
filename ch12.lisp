(output-stream-p *standard-output*)
;; T

(write-char #\x *standard-output*)
;; => x
;;
;;    #\x

(input-stream-p *standard-input*)
;; => T

;; (read-char *standard-input*)

;; FILES

(with-open-file (my-stream "data.txt" :direction :output)
  (print "my data" my-stream))
;; => "my data"

(with-open-file (my-stream "data.txt" :direction :input)
  (read my-stream))
;; => "my data"

;; storing data
(let ((animal-noises '((dog . woof)
                       (cat . meow))))
  (with-open-file (my-stream "animal-noises.txt" :direction :output)
    (print animal-noises my-stream)))
;; =>
;; ((DOG . WOOF) (CAT . MEOW))

(with-open-file (my-stream "animal-noises.txt" :direction :input)
  (read my-stream))
;; =>
;; ((DOG . WOOF) (CAT . MEOW))

(with-open-file (my-stream "animal-noises.txt" :direction :input)
  (caar (read my-stream)))
;; => DOG

(ignore-errors
  (with-open-file (my-stream "data.txt" :direction :output :if-exists :error)
    (print "my data" my-stream)))
;; => NIL
;;    #<SYSTEM::SIMPLE-FILE-ERROR #x00000002003CF7A9>

(with-open-file (my-stream "data.txt" :direction :output
                           :if-exists :supersede)
  (print "my data" my-stream))
;; => "my data"

;; SOCKETS
;; (using the clisp implementation)
;; every socket within a network must have a socket address, which has two components:
;; 1. an IP address - that uniquely identifies a computer on the network (i.e. 19.168.1.1)
;; 2. A Port number - unique port on the computer that no other process is using
;; IP + Port Number = Socket Address
;; messages on the network are routed using the socket address
;; computer gets message, looks at port, sends to that port
;; socket is a way for a program to say 'anything for port 123, send to me'

;; socket listener = server (opens socket and waits)
;; socket connection creator = client (opens socket and uses it to communicate)

;; (defparameter my-stream (socket-accept my-socket))
;; (defparameter my-stream (socket-accept my-socket))
