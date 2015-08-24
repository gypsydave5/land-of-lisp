(defparameter *small* 1)
(defparameter *big* 100)
(defun guess-my-number ()
  (ash (+ *small* *big*) -1))
(defun smaller ()
  (setf *big* (1- (guess-my-number)))
  (guess-my-number))
(defun bigger ()
  (setf *small* (1+ (guess-my-number)))
  (guess-my-number))
(defun start-over ()
  (defparameter *small* 1)
  (defparameter *big* 100))

;;; The let command, to define local variables
;;; (local to the let's body that is)
(let ((a 5)
      (b 2))
  (+ a b))
