(load "graph-util")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)

(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cops-odds 15)

(defun random-node ()
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  (unless (eq a b)
          (list (cons a b) (cons b a))))

(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
                        collect (edge-pair (random-node)
                                           (random-node)))))

;; which gives us some AWESOEM PWRS
(make-edge-list)
;; ((20 . 11) (11 . 20) (16 . 13) (13 . 16) (20 . 24) (24 . 20) (5 . 22) (22 . 5)
;;  (19 . 18) (18 . 19) (23 . 17) (17 . 23) (29 . 24) (24 . 29) (16 . 29)
;;  (29 . 16) (10 . 12) (12 . 10) (15 . 11) (11 . 15) (3 . 11) (11 . 3) (1 . 20)
;;  (20 . 1) (28 . 25) (25 . 28) (7 . 17) (17 . 7) (10 . 22) (22 . 10) (3 . 8)
;;  (8 . 3) (28 . 30) (30 . 28) (8 . 11) (11 . 8) (16 . 21) (21 . 16) (27 . 24)
;;  (24 . 27) (7 . 11) (11 . 7) (21 . 17) (17 . 21) (11 . 14) (14 . 11) (25 . 27)
;;  (27 . 25) (13 . 30) (30 . 13) (4 . 17) (17 . 4) (9 . 26) (26 . 9) (9 . 1)
;;  (1 . 9) (25 . 7) (7 . 25) (6 . 20) (20 . 6) (1 . 30) (30 . 1) (18 . 10)
;;  (10 . 18) (27 . 18) (18 . 27) (12 . 18) (18 . 12) (20 . 22) (22 . 20)
;;  (12 . 15) (15 . 12) (15 . 26) (26 . 15) (6 . 2) (2 . 6) (18 . 25) (25 . 18)
;;  (23 . 11) (11 . 23) (20 . 23) (23 . 20) (28 . 27) (27 . 28) (26 . 29)
;;  (29 . 26) (25 . 22) (22 . 25))

;; loop is interesting...

(loop repeat 10
      collect 1) ;; (1 1 1 1 1 1 1 1 1 1)

(loop for n from 1 to 10
      collect n) ;; (1 2 3 4 5 6 7 8 9 10)

(loop for n from 1 to 10
      collect (* n n)) ;; (1 4 9 16 25 36 49 64 81 100)

;; islands in the stream

(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
                   (eq (car x) node))
                 edge-list))

(defun get-connected (node edge-list)
  (let ((visited nil))
    (labels ((traverse (node)
               (unless (member node visited)
                 (push node visited)
                 (mapc (lambda (edge)
                         (traverse (cdr edge)))
                       (direct-edges node edge-list)))))
      (traverse node))
    visited))

(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
               (let* ((connected (get-connected (car nodes) edge-list))
                      (unconnected (set-difference nodes connected)))
                 (push connected islands)
                 (when unconnected
                   (find-island unconeected)))))
      (find-island nodes))
    islands))

(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
            (connect-with-bridges (cdr islands)))))

(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list))
          edge-list))

(connect-all-islands '(1 2 3) (make-edge-list))
;; ((16 . 18) (18 . 16) (26 . 21) (21 . 26) (20 . 15) (15 . 20) (13 . 21)
;;  (21 . 13) (3 . 21) (21 . 3) (28 . 26) (26 . 28) (5 . 26) (26 . 5) (23 . 1)
;;  (1 . 23) (16 . 6) (6 . 16) (20 . 7) (7 . 20) (28 . 14) (14 . 28) (12 . 18)
;;  (18 . 12) (29 . 3) (3 . 29) (26 . 30) (30 . 26) (12 . 9) (9 . 12) (26 . 15)
;;  (15 . 26) (4 . 11) (11 . 4) (25 . 7) (7 . 25) (29 . 22) (22 . 29) (18 . 28)
;;  (28 . 18) (16 . 21) (21 . 16) (29 . 1) (1 . 29) (13 . 16) (16 . 13) (19 . 6)
;;  (6 . 19) (10 . 15) (15 . 10) (23 . 24) (24 . 23) (8 . 9) (9 . 8) (24 . 14)
;;  (14 . 24) (24 . 8) (8 . 24) (18 . 22) (22 . 18) (18 . 29) (29 . 18) (29 . 14)
;;  (14 . 29) (17 . 22) (22 . 17) (11 . 2) (2 . 11) (19 . 28) (28 . 19) (26 . 10)
;;  (10 . 26) (11 . 8) (8 . 11) (29 . 24) (24 . 29) (5 . 28) (28 . 5) (27 . 6)
;;  (6 . 27) (26 . 14) (14 . 26) (17 . 16) (16 . 17))

                                        ; livin' on the edges

(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num*
                   collect i))
         (edge-list (connect-all-islands nodes (make-edge-list)))
         (cops (remove-if-not (lambda (x)
                                (zerop (random *cops-odds*)))
                              edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)
            (cons node1
                  (mapcar (lambda (edge)
                            (list (cdr edge)))
                          (remove-duplicates (direct-edges node1 edge-list)
                                             :test #'equal))))
          (remove-duplicates (mapcar #'car edge-list))))
