(ql:quickload "lisp-unit")
(load "grand-theft-wumpus")

(defpackage #:grand-theft-wumpus-test
  (:use #:common-lisp #:lisp-unit #:grand-theft-wumpus))

(in-package #:grand-theft-wumpus-test)

(defparameter *test-nodes* '(1 2 3 4 ))
(defparameter *test-edges* '((5 . 1) (1 . 5) (3 . 4) (4 . 3) (2 . 3) (3 . 2)))

(define-test direct-edges-test
  (assert-equal (car (direct-edges 1 *test-edges*)) (cons 1 5))
  (let ((edges (direct-edges 3 *test-edges*)))
    (assert-true (contains-edge '(3 . 2) edges))
    (assert-true (contains-edge '(3 . 4) edges))))

(define-test get-connected-test
  "get connected returns a list including all the nodes a node is connected to"
  (assert-true (set-equal '(5 1) (get-connected 5 *test-edges*)))
  (assert-true (set-equal '(5 1) (get-connected 1 *test-edges*)))
  (assert-true (set-equal '(3 4 2) (get-connected 2 *test-edges*)))
  (assert-true (set-equal '(3 4 2) (get-connected 4 *test-edges*)))
  (assert-true (set-equal '(3 4 2) (get-connected 3 *test-edges*)))
  (assert-true (set-equal '(0) (get-connected 0 *test-edges*))))

(define-test find-islands-test
  (let ((islands (find-islands *test-nodes* *test-edges*)))
    (assert-eql 2 (length islands))
    (assert-true (member '(5 1) islands :test #'set-equal))
    (assert-true (member '(2 3 4) islands :test #'set-equal))))

(define-test connect-with-bridges-test
  (let ((islands (find-islands *test-nodes* *test-edges*)))
    (assert-equal '((2 . 5) (5 . 2)) (connect-with-bridges islands))))

(define-test connect-all-islands-test
  (let ((connected-edges (connect-all-islands *test-nodes* *test-edges*)))
    (assert-true (set-equal *test-nodes* (get-connected 1 connected-edges)))
    (assert-true (set-equal *test-nodes* (get-connected 2 connected-edges)))
    (assert-true (set-equal *test-nodes* (get-connected 3 connected-edges)))
    (assert-true (set-equal *test-nodes* (get-connected 4 connected-edges)))
    (assert-true (set-equal *test-nodes* (get-connected 5 connected-edges)))))

(run-tests :all)