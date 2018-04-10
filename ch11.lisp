(format t "Add onion rings for only ~$ dollars more!" 1.5)
;; => Add onion rings for only 1.50 dollars more!
;;
;;    NIL

(format nil "Add onion rings for only ~$ dollars more!" 1.5)
;; => "Add onion rings for only 1.50 dollars more!"

(format *standard-output* "Add onion rings for only ~$ dollars more!" 1.5)
;; => Add onion rings for only 1.50 dollars more!
;;
;;    NIL

(prin1 "foo")
;; => "foo"
;;
;;    "foo"
(format t "I am printing ~s in the middle of this sentence." "foo")
;; => I am printing "foo" in the middle of this sentence.
;;
;;    NIL

(princ "foo")
;; => foo
;;
;;    "foo"
(format t "I am printing ~a in the middle of this sentence." "foo")
;; => I am printing foo in the middle of this sentence.
;;
;;    NIL

;; right padding
(format t "I am printing ~10a in the middle of this sentence." "foo")
;; => I am printing foo        in the middle of this sentence.
;;
;;    NIL

;; left padding
(format t "I am printing ~10@a in the middle of this sentence." "foo")
;; => I am printing        foo in the middle of this sentence.
;;
;;    NIL

;; left padding
(format t "I am printing ~,,10a in the middle of this sentence." "foo")
;; => I am printing foo           in the middle of this sentence.
;;
;;    NIL

(format t "The word ~,,4,'!a feels very important." "foo")
;; => The word foo!!!! feels very important.
;;
;;    NIL

(format t "The word ~,,4,'!@a feels very important." "foo")
;; => The word !!!!foo feels very important.
;;
;;    NIL

;; Number Formatting

(format t "The number 1000 in hexadecimal is ~x" 1000)
;; => The number 1000 in hexadecimal is 3E8
;;
;;    NIL

(format t "The number 1000 in binary is ~x" 1000)
;; => The number 1000 in binary is 3E8
;;
;;    NIL

(format t "The number 1000 in decimal is ~d" 1000)
;; => The number 1000 in decimal is 1000
;;
;;    NIL

(format t "Numbers with commas in are ~:d times better." 1000000)
;; => Numbers with commas in are 1,000,000 times better.
;;
;;    NIL

(format t "I am printing ~10d within ten spaces of room" 1000000)
;; => I am printing    1000000 within ten spaces of room
;;
;;    NIL

(format t "I am printing ~10,'xd within ten spaces of room" 1000000)
;; => I am printing xxx1000000 within ten spaces of room
;;
;;    NIL

;; Floating point

(format t "PI can be estimated as ~4f" 3.141593)
;; => PI can be estimated as 3.14
;;
;;    NIL

(format t "PI can be estimated as ~,4f" 3.141593)
;; => PI can be estimated as 3.1416
;;
;;    NIL

(format t "PI can be estimated as ~,4f" pi)
;; => PI can be estimated as 3.1416
;;
;;    NIL

(format t "Percentages are ~,,2f percent better than fractions" 0.77)
;; => Percentages are 77.0 percent better than fractions
;;
;;    NIL

(format t "I wish I had $~$ in my pocket" 10.2)
;; => I wish I had $10.20 in my pocket
;;
;;    NIL

;; Multiple lines
(progn (princ 22)
       (terpri)
       (princ 33))
;; => 22
;;    33
;;
;;    33

(progn (princ 22)
       (fresh-line)
       (princ 33))
;; => 22
;;    33
;;
;;    33

(progn (princ 22)
       (fresh-line)
       (fresh-line)
       (fresh-line)
       (fresh-line)
       (princ 33))
;; => 22
;;    33
;;
;;    33

;; ~% is like terpri
(progn (format t "this is on one line ~%")
       (format t "~%this is on another line"))
;; => this is on one line
;;
;;    this is on another line
;;
;;    NIL

;; ~& is like fresh-line
(progn (format t "this is on one line ~&")
       (format t "~&this is on another line"))
;; => this is on one line
;;    this is on another line
;;
;;    NIL

(format t "this will print ~5%on two lines spread 5 lines apart")
;; => this will print
;;
;;
;;
;;
;;    on two lines spread 5 lines apart
;;
;;    NIL

(defun random-animal ()
  (nth (random 5) '("dog" "tick" "tiger" "walrus" "kangaroo")))
;; => RANDOM-ANIMAL

(loop
   repeat 10
   do (format t "~t~a ~15t~a ~25t~a~%"
              (random-animal)
              (random-animal)
              (random-animal)))
;; =>  walrus        kangaroo  kangaroo
;;     walrus        walrus    tiger
;;     dog           kangaroo  dog
;;     kangaroo      dog       dog
;;     kangaroo      tick      dog
;;     dog           dog       kangaroo
;;     tiger         tiger     tiger
;;     dog           tiger     kangaroo
;;     tick          dog       walrus
;;     kangaroo      walrus    dog
;;
;;
;;    NIL

;; evenly spaced across 30 characters
(loop
   repeat 10
   do (format t "~30<~a~;~a~;~a~>~%"
              (random-animal)
              (random-animal)
              (random-animal)))
;; => walrus        dog       walrus
;;    walrus      kangaroo     tiger
;;    tick      walrus      kangaroo
;;    walrus        tiger       tick
;;    dog         tiger        tiger
;;    tick         tick         tick
;;    dog         walrus        tick
;;    tiger       walrus      walrus
;;    walrus        walrus       dog
;;    tick         tick        tiger
;;
;;
;;    NIL

(loop
   repeat 10
   do (format t "~15:@<~a~>~15:@<~a~>~15:@<~a~>~%"
              (random-animal)
              (random-animal)
              (random-animal)))
;; =>       tick          walrus          dog
;;          tick         kangaroo       kangaroo
;;         walrus          dog           tiger
;;        kangaroo         tick          tiger
;;        kangaroo        walrus        kangaroo
;;         tiger         kangaroo        tiger
;;         tiger          walrus          tick
;;         tiger          tiger           tick
;;          dog           walrus         walrus
;;          tick         kangaroo        tiger
;;
;;
;;    NIL

(defparameter *animals* (loop
                           repeat 10
                           collect (random-animal)))
(format t "~{I see a ~a! ~}" *animals*)
;; => I see a tiger! I see a kangaroo! I see a tick! I see a tiger! I see a walrus! I see a tiger! I see a dog! I see a kangaroo! I see a walrus! I see a tiger!
;;
;;    NIL

(format t "~{I see a ~a... or was it a ~a?~%~}" *animals*)
;; => I see a tiger... or was it a kangaroo?
;;    I see a tick... or was it a tiger?
;;    I see a walrus... or was it a tiger?
;;    I see a dog... or was it a kangaroo?
;;    I see a walrus... or was it a tiger?
;;
;;
;;    NIL

(format t "|~{~<|~%|~,33:;~2d ~>~}|" (loop for x below 100 collect x))
;; => | 0  1  2  3  4  5  6  7  8  9 |
;;    |10 11 12 13 14 15 16 17 18 19 |
;;    |20 21 22 23 24 25 26 27 28 29 |
;;    |30 31 32 33 34 35 36 37 38 39 |
;;    |40 41 42 43 44 45 46 47 48 49 |
;;    |50 51 52 53 54 55 56 57 58 59 |
;;    |60 61 62 63 64 65 66 67 68 69 |
;;    |70 71 72 73 74 75 76 77 78 79 |
;;    |80 81 82 83 84 85 86 87 88 89 |
;;    |90 91 92 93 94 95 96 97 98 99 |
;;
;;    NIL


(defun robots
    (loop named main
       with directions = '((q . -65) (w . -64) (e . -63) (a . -1)
                           (d . 1) (z . 63) (x . 64) (c . 65))
       for pos = 544
       then (progn (format t "~%qwe/asd/zxc to move, (t)eleport, (l)eave:")
                   (force-output)
                   (let* ((c (read))
                          (d (assoc c directions)))
                     (cond (d (+ pos (cdr d)))
                           ((eq 't c) (random 1024))
                           ((eq 'l c) (return-from-mail 'bye))
                           (t pos))))
       for monsters = (loop repeat 10 collect (random 1024))
       then (loop for mpos in monsters
               collect (if (> (count mpos monsters) 1)
                           mpos
                           (cdar (sort (loop
                                          for (k . d) in directions
                                          for new-mpos = (+ mpos d)
                                          collect (cons (+ (abs (- (mod new-mpos 64)
                                                                   (mod pos 64)))
                                                           (abs (- (ash new-mpos -6)
                                                                   (ash pos -6))))
                                                        new-mpos))
                                       '<
                                       :key #'car))))
       when (loop
               for mpos
               in monsters
               always (> (count mpos monsters) 1))
       return 'player-wins
       do (format t
                  "~%|~{~<|~%|~,65:;~A~>~}|"
                  (loop for p
                     below 1024
                     collect (cond ((member p monsters)
                                    (cond ((= p pos) (return-from main 'player-loses))
                                          ((> (count p monsters) 1) #\#)
                                          (t #\A)))
                                   ((= p pos) #\@)
                                   (t #\ ))))))