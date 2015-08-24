                                        ; lambda
                                        ; the ultimate

(defun a-half (n)
  (/ n 2)) ;; A-HALF

(a-half 4) ;; 2

#'a-half ;;
;; #<FUNCTION A-HALF (N) (DECLARE (SYSTEM::IN-DEFUN A-HALF))
;;   (BLOCK A-HALF (/ N 2))>

(lambda (n) (/ n 2)) ;; #<FUNCTION :LAMBDA (N) (/ N 2)>

;; lambda is not a function - lambda is a macro that returns a function, which is anonymous

(mapcar (lambda (n) (/ n 2)) '(2 4 8 16 24 48)) ;; (1 2 4 8 12 24)

(defparameter *just-my-kinda-fun* (lambda (n) (cons n '(is my kinda thing)))) ;; *JUST-MY-KINDA-FUN*

(mapcar *just-my-kinda-fun* '(this)) ;; ((THIS IS MY KINDA THING))
