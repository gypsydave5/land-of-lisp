;; let for local bindings
(let ((a 1)
      (b 2))
  (+ a b))
;; => 3

;; flet for function let
(flet ((f (n)
         (+ n 10))
       (g (n)
         (- n 3)))
  (g (f 5)))
;; => 12

;; labels to refer to the functions in other functions:
(labels ((a (n)
           (+ n 5))
         (b (n)
           (+ (a n) 6)))
  (b 10))
;; => 21
