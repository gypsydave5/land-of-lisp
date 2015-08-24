                                        ; printing
(print "Hello, world!") ;; "Hello, world!" "Hello, world!"

;; with line returns
(progn
  (print "This")
  (print "is")
  (print "a")
  (print "test"))
;; "This"
;; "is"
;; "a"
;; "test"
;; without line returns
(progn
  (prin1 "This")
  (prin1 "is")
  (prin1 "a")
  (prin1 "test")) ;; "This""is""a""test"
;; and inside functions
(defun say-hello ()
  (print "What's your name?")
  (let ((name (read)))
    (print "nice to meet you,")
    (print name)))

(defun add-five ()
  (print "Give me a number: ")
  (let ((number (read)))
    (print "plus five equals...")
    (print (+ 5 number))))

(defun add-five ()
  (print "Give me a number: ")
  (flet ((add5 (num) (+ num 5)))
	 (let ((number (read)))
	   (print "plus five equals")
	   (print (add5 number)))))

(print '3)
;; 3 3
(print '3.4)
;; 3.4 3.4
(print '"bob")
;; "bob" "bob"
(print '#\a)
;; #\a #\a
(print #\a)
;; #\a #\a

					; princ
(progn
  (princ "We interrupt this sentence")
  (princ #\newline)
  (princ "for an important newline character"))
;; We interrupt this sentence
;; for an important newline character
                                        ; read-line
(defun say-hello ()
  (princ "Please type your name: ")
  (let ((name (read-line)))
    (princ "Nice to meet you, ")
    (princ name))) ;; SAY-HELLO
;; ah, it reads a line - the whole damn line, spaces and all

                                        ; the repl
(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

;; game-read
(defun game-read ()
  (let ((cmd (read-from-string
              (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
             (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(concatenate 'string "(" "bob" ")") ;; "(bob)"
(list 'quote "(bob)") ;; '"(bob)"
(defun ((quote-it (x)
                  (list 'quote x))))
(cons (car "(go bob)") (mapcar #'quote-it (cdr cmd))) ;;
