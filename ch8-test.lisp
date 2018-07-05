(ql:quickload "lisp-unit")

(defpackage #:grand-theft-wumpus-test
  (:use #:common-lisp #:lisp-unit))

(in-package #:grand-theft-wumpus-test)

(define-test one-add-one-is-two-test
  (assert-equal (+ 1 1) 2))

#-xlisp-test
(let ((*print-errors* t)
      (*print-failures* t))
  (run-tests :all))

