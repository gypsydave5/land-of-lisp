;; FUNCTIONAL PROGRAMMING

;; the world's simplest functional program

;; the clean bit
(defun add-widget (database widget)
  (cons widget database))

;; the dirty bit
(defparameter *database* nil)

(defun main-loop ()
  (loop (princ "Please enter the name of a new widget:")
     (setf *database* (add-widget *database* (read)))
     (format t "The database contains the following: ~A~%" *database*)))
;; NB play with format codez here

;; Actually more efficient than it seems as the cons is reusing the old 'database'

;; Higher Order Programming

;; add two to each number of my list
(defparameter *my-list* '(4 7 2 3))

;; imperative version
(loop for n below (length *my-list*)
   do (setf (nth n *my-list*) (+ (nth n *my-list*) 2)))
*my-list*                               ;; (6 9 4 5)

;; better, functional
(defun add-two (list)
  (when list
    (cons (+ 2 (car list))
          (add-two (cdr list)))))
(add-two '(4 7 2 3))
;; => (6 9 4 5)

;; go see SICP for some more on this... but ultimately pass config using higher order function

(mapcar (lambda (x) (+ x 2)) '(4 7 2 3))
;; => (6 9 4 5)

;; hooray for map
