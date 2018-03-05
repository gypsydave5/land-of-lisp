;; Booleans and nil punning
(if '() 'i-am-true 'i-am-false)
;; => I-AM-FALSE
(if '(1) 'i-am-true 'i-am-false)
;; => I-AM-TRUE

(defun my-length (list)
  (if list
      (1+ (my-length (cdr list)))
      0))
(my-length '(list with four symbols))
;; => 4

(defun my-length-r (list)
  (labels ((recur (list len)
             (if list
                 (recur (cdr list) (1+ len))
                 len)))
    (recur list 0)))
(my-length-r '(another list with five symbols))
;; => 5

(eq '() nil)
;; => T
(eq '() ())
;; => T
(eq '() 'nil)
;; => T


(defvar *number-to-mess-with* nil)

(if (oddp 5)
    (progn (setf *number-to-mess-with* 77)
           (princ *number-to-mess-with*)
           "TRUE")
    "FALSE")
;; => 77
;;
;;    "TRUE"

(defvar *arch-enemy* nil)
(defun pudding-eater (person)
  (cond ((eq person 'henry)
         (setf *arch-enemy* 'stupid-lisp-alien)
         '(curse you lisp alien - you ate my pudding))
        ((eq person 'johnny)
         (setf *arch-enemy* 'useless-old-johnny)
         '(i hope you choked on my pudding johnnny))
        (t
         '(why you eat my pudding stranger ?))))

(defun pudding-eater-q (person)
  (cond ((eq person 'henry)
         (setf *arch-enemy* 'stupid-lisp-alien)
         '(curse you lisp alien - you ate my pudding))
        ((eq person 'johnny)
         (setf *arch-enemy* 'useless-old-johnny)
         '(i hope you choked on my pudding johnnny))
        (t
         `(why you eat my pudding ,person ?))))

(pudding-eater 'johnny)
;; => (I HOPE YOU CHOKED ON MY PUDDING JOHNNNY)
*arch-enemy*
;; => USELESS-OLD-JOHNNY
(pudding-eater 'george-cloony)
;; => (WHY YOU EAT MY PUDDING STRANGER ?)
(pudding-eater-q 'george-cloony)
;; => (WHY YOU EAT MY PUDDING GEORGE-CLOONY ?)

;; CASE
(defun pudding-eater (person)
  (case person ((henry)
         (setf *arch-enemy* 'stupid-lisp-alien)
         '(curse you lisp alien - you ate my pudding))
        ((johnny)
         (setf *arch-enemy* 'useless-old-johnny)
         '(i hope you choked on my pudding johnnny))
        (otherwise
         '(why you eat my pudding stranger ?))))

(and 1 2 3)
;; => 3
(and 1 2 nil 3)
;; => NIL
(or nil nil nil 7 nil)
;; => 7

;; CONDITIONALLY SET A GLOBAL
(defparameter *is-it-even* nil)
(or (oddp 4) (setf *is-it-even* t))

;; NOT-NIL PUNNING...
(member 1 '(3 4 1 5))
;; => (1 5)

(member nil '(3 4 nil 5 6))
;; => (NIL 5 6)

(find-if #'oddp '(2 4 5 6))
;; => 5
(if (find-if #'oddp '(2 4 5 6))
    'there-is-an-odd-number
    'there-is-an-even-number)
;; => THERE-IS-AN-ODD-NUMBER

;; EQUALITY

;; eq - fast, but only for symbols
(eq '1 '1)
;; => T
(eq 1.0 1)
;; => NIL
(eq (list 1 2 3) (list 1 2 3))
;; => NIL
(eq "1" "1")
;; => NIL
(eq #\a #\a)
;; => T
(eq 1 1)
;; => T

;; equal - isomorphic comparison
(equal (list 1 2 3) (list 1 2 3))
;; => T
(equal (list 1 2 3) '(1 2 3))
;; => T
(equal "ello" "ello")
;; => T
(equal 1.0 1)
;; => NIL
(equal #\a #\a)
;; => T

;; eql - symbols, numbers, characters

;; equalp - posh equal, doesn't care about capitalization or float/int
(equalp "hello" "HELLO")
;; => T
(equalp 1.0 1)
;; => T
(= 1 1.0)
;; => T
