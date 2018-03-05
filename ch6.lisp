;; PRINTING

(print "foo")
;; =>
;;    "foo"
;;
;;    "foo"

(progn (print "this")
       (print "is")
       (print "a")
       (print "test"))
;; =>
;;    "this"
;;    "is"
;;    "a"
;;    "test"
;;
;;    "test"

(progn (prin1 "this")
       (prin1 "is")
       (prin1 "a")
       (prin1 "test"))
;; => "this""is""a""test"
;;
;;    "test"

(defun say-hello ()
  (print "Please type your name:")
  (let ((name (read)))
    (print "Nice to meet you, ")
    (print name)))

;; compare (read-line)

(defun |oh really?| () "Yes, really")
(|oh really?|)
;; => "Yes, really"

;; print
(print #\a)
;; =>
;;    #\a
;;
;;    #\a
(print "hello")
;; =>
;;    "hello"
;;
;;    "hello"

;; princ
(princ #\a)
;; => a
;;
;;    #\a
(princ "hello")
;; => hello
;;
;;    "hello"

(progn (princ "Start")
       (princ #\newline)
       (princ "Middle"))
;; => Start
;;    Middle
;;
;;    "Middle"

(defun say-hello ()
  (princ "Please type your name: ")
  (let ((name (read-line)))
    (princ "Nice to meet you, ")
    (princ name)))

;; eval
(defparameter *foo* '(+ 1 2))
*foo*
;; => (+ 1 2)
(eval *foo*)
;; => 3

;; CARRY ON WITH THE GAME

(defun game-read ()
  (let ((cmd (read-from-string
              (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
             (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eq item #\space)
             (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.))
             (cons item (tweak-text rest t lit)))
            ((eq item #\")
             (tweak-text rest caps (not lit)))
            (lit
             (cons item (tweak-text rest nil lit)))
            ((or caps lit)
             (cons (char-upcase item) (tweak-text rest nil lit)))
            (t
             (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
  (princ (coerce (tweak-text
                  (coerce (string-trim "() " (prin1-to-string lst))
                          'list)
                  t nil)
                 'string))
  (fresh-line))

(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

