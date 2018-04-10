(loop for i
   below 5
   sum i)
;; => 10

(loop for i
   from 5
   to 10
   sum i)
;; => 45

(loop for i
   in '(100 20 3)
   sum i)
;; => 123

(loop for i
   below 5
   do (print i))
;; => 
;;    0
;;    1
;;    2
;;    3
;;    4
;;
;;    NIL

(loop for i
   below 10
   when (oddp i)
   sum i)
;; => 25

(loop for i
   from 0
   do (print i)
   when (= i 5)
   return 'falafel)
;; => 
;;    0
;;    1
;;    2
;;    3
;;    4
;;    5
;;
;;    FALAFEL

(loop for i
   in '(2 3 4 5 6)
   collect (* i i))
;; => (4 9 16 25 36)

(loop
   for x below 10
   for y below 10
   collect (+ x y))
;; => (0 2 4 6 8 10 12 14 16 18)

(loop
   for x below 10
   collect (loop
              for y below 10
              collect (cons x y)))
;; =>
;; (((0 . 0) (0 . 1) (0 . 2) (0 . 3) (0 . 4) (0 . 5) (0 . 6) (0 . 7) (0 . 8)
;;   (0 . 9))
;;  ((1 . 0) (1 . 1) (1 . 2) (1 . 3) (1 . 4) (1 . 5) (1 . 6) (1 . 7) (1 . 8)
;;   (1 . 9))
;;  ((2 . 0) (2 . 1) (2 . 2) (2 . 3) (2 . 4) (2 . 5) (2 . 6) (2 . 7) (2 . 8)
;;   (2 . 9))
;;  ((3 . 0) (3 . 1) (3 . 2) (3 . 3) (3 . 4) (3 . 5) (3 . 6) (3 . 7) (3 . 8)
;;   (3 . 9))
;;  ((4 . 0) (4 . 1) (4 . 2) (4 . 3) (4 . 4) (4 . 5) (4 . 6) (4 . 7) (4 . 8)
;;   (4 . 9))
;;  ((5 . 0) (5 . 1) (5 . 2) (5 . 3) (5 . 4) (5 . 5) (5 . 6) (5 . 7) (5 . 8)
;;   (5 . 9))
;;  ((6 . 0) (6 . 1) (6 . 2) (6 . 3) (6 . 4) (6 . 5) (6 . 6) (6 . 7) (6 . 8)
;;   (6 . 9))
;;  ((7 . 0) (7 . 1) (7 . 2) (7 . 3) (7 . 4) (7 . 5) (7 . 6) (7 . 7) (7 . 8)
;;   (7 . 9))
;;  ((8 . 0) (8 . 1) (8 . 2) (8 . 3) (8 . 4) (8 . 5) (8 . 6) (8 . 7) (8 . 8)
;;   (8 . 9))
;;  ((9 . 0) (9 . 1) (9 . 2) (9 . 3) (9 . 4) (9 . 5) (9 . 6) (9 . 7) (9 . 8)
;;   (9 . 9)))

(loop
   for i from 0
   for day in '(monday tuesday wednesday thursday friday saturday sunday)
   collect (cons i day))
;; =>
;; ((0 . MONDAY) (1 . TUESDAY) (2 . WEDNESDAY) (3 . THURSDAY) (4 . FRIDAY)
;;  (5 . SATURDAY) (6 . SUNDAY))

