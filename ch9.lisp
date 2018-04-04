(defpackage chapter-9
  (:use #:common-lisp))
(in-package #:chapter-9)

;; ARRAYS
(defparameter x (make-array 3))
;; => X
(aref x 1)
;; => 0
(setf (aref x 1) 'foo)
;; => FOO
(aref x 1)
;; => FOO

;; GENERIC SETTERS
(setf foo '(a b c))
;; => (A B C)
(second foo)
;; => B
(setf (second foo) 'z)
;; => Z
foo
;; => (A Z C)

;; GENERALISED REFERENCE
(setf foo (make-array 4))
;; => #(0 0 0 0)
(setf (aref foo 2) '(x y z))
;; => (X Y Z)
foo
;; => #(0 0 (X Y Z) 0)
(setf (car (aref foo 2)) (make-hash-table))
;; => #S(HASH-TABLE :TEST FASTHASH-EQL)
(setf (gethash 'zoink (car (aref foo 2))) 5)
;; => 5
foo
;; => #(0 0 (#<HASH-TABLE :TEST EQL :COUNT 1 {1005AD8F33}> Y Z) 0)
(car (aref foo 2))
;; => #S(HASH-TABLE :TEST FASTHASH-EQL (ZOINK . 5))

;; HASH TABLES
(make-hash-table)
;; => #S(HASH-TABLE :TEST FASTHASH-EQL)
(defparameter x (make-hash-table))
;; => X
(gethash 'yup x)
;; => NIL
;;    NIL
;; ;; multiple return values
(setf (gethash 'yup x) '25)
;; => 25
(gethash 'yup x)
;; => 25
;;    T
;; ;; multiple return values!

;; Returning Multiple Values
(round 2.4)
;; => 2
;;    0.4000001
(defun foo ()
  (values 3 7))
;; => FOO
(foo)
;; => 3
;;    7
(+ (foo) 5)
;; => 8
(multiple-value-bind (a b) (foo) (* a b))
;; => 21

;; Structures

(defstruct person
  name
  age
  waist-size
  favorite-color)
;; => PERSON
(defparameter *bob* (make-person :name "Bob"
                                 :age 35
                                 :waist-size 32
                                 :favorite-color "blue"))
;; => *BOB*
(person-age *bob*)
;; => 35
(person-favorite-color *bob*)
;; => "blue"
(setf (person-age *bob*) 36)
;; => 36
(person-age *bob*)
;; => 36

;; struct literals
(defparameter *that-guy* #S(person :name "Bob"
                                   :age 35
                                   :waist-size 32
                                   :favorite-color "blue"))
;; => *THAT-GUY*
(princ *that-guy*)
;; =>
;; #S(PERSON :NAME Bob :AGE 35 :WAIST-SIZE 32 :FAVORITE-COLOR blue)
;;
;; #S(PERSON :NAME "Bob" :AGE 35 :WAIST-SIZE 32 :FAVORITE-COLOR "blue")

;; GENERICS

(length '(a b c))
;; => 3
(length "blub")
;; => 4
(length (make-array 5))
;; => 5

(find-if #'numberp '(a b 5 d))
;; => 5
(find-if (lambda (x) (equal x #\s)) "has")
;; => #\s
(count #\s "mississippi")
;; => 4
(position 3 '(1 2 3 4 5))
;; => 2
(some #'numberp '(a b 5 d))
;; => T
(some #\s "has")
(defun vowelp (c)
  (some (lambda (v) (equal c v)) "aeiou"))
(every #'vowelp "oui")
;; => T
(every #'vowelp '(#\a #\e 7))
;; => NIL

(reduce #'+ '(3 4 6 5 2))
;; => 20
(reduce (lambda (best item)
          (if (and (evenp item) (> item best))
              item
              best))
        '(7 6 6 5 2)
        :initial-value 0)
;; => 6
(defun sum (seq)
  (reduce #'+ seq))
(sum '(1 2 3))
;; => 6
(sum (make-array 5 :initial-contents '(1 2 3 4 5)))
;; => 15

;; MAP... is GENERIC
(map 'list
     (lambda (x)
       (if (eq x #\s)
           #\S
           x))
     "this is a string")
;; =>
;; (#\t #\h #\i #\S #\Space #\i #\S #\Space #\a #\Space #\S #\t #\r #\i #\n #\g)
(map 'string
     #'char-upcase
     "hello")
;; => "HELLO"
(string-upcase "hello")
;; => "HELLO"

;; SUBSEQ
(subseq "america" 2 6)
;; => "eric"
(subseq (loop for x from 1 to 10 collect (* x x)) 2 4)
;; => (9 16)

;; SORT
(sort '(5 8 2 4 9 3 6 1) #'<)
;; => (1 2 3 4 5 6 8 9)

;; TYPE PREDICATES
(numberp 5)
;; => T
(defun add (a b)
  (cond ((and (numberp a) (numberp b)) (+ a b))
        ((and (listp a) (listp b)) (append a b))))
(add 3 4)
;; => 7
(add '(a b) '(c d))
;; => (A B C D)

;; Awkward and long - and so instead we can have...
;; DEFMETHOD
(defmethod add ((a number) (b number)) (+ a b))
;; => #<STANDARD-METHOD (#<BUILT-IN-CLASS NUMBER> #<BUILT-IN-CLASS NUMBER>)>
(defmethod add ((a list) (b list)) (append a b))
;; => #<STANDARD-METHOD (#<BUILT-IN-CLASS LIST> #<BUILT-IN-CLASS LIST>)>
(add 5 6)
;; => 11
(add '(e f) '(g h))
;; => (E F G H)

;; defmethod can be used with defstruct ... for oop! w00t!
;; Now on to the CLOS!
