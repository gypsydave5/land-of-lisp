;; Simple Error - 'Signalling a Condition' - i.e. something is wrong!
;; (error "foo")

;; Custom 'types' of condition
(define-condition foo () ()
  (:report (lambda (condition stream)
             (princ "Stop FOOing around, numbskull!" stream))))

;; This is triggered with
;; (error 'foo)
;; 

;; Intercepting Conditions (i.e. 'catch')
(defun baaad-function ()
  (error 'foo))

(handler-case (baaad-function)
  (foo () "somebody signaled foo!")
  (bar () "somebody signaled bar!"))
;; => "somebody signaled foo!"

;; unwind-protect 'catches' the errors

(unwind-protect (/ 1 0)
  (princ "I need to say 'flubyduby' no matter what"))
