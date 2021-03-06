(load "grand-theft-wumpus")

(in-package #:grand-theft-wumpus)

(setf *edge-num* 1000)
(setf *node-num* 1000)
(time (dotimes (i 100) (get-connected 1 (make-edge-list))))
;; => NIL

(defun hash-edges (edge-list)
  (let ((tab (make-hash-table)))
    (mapc (lambda (x)
            (let ((node (car x)))
              (push (cdr x) (gethash node tab))))
          edge-list)
    tab))
;; as NIL is the empty value, and NIL is also '(), we can push into it.

(defun get-connected-hash (node edge-tab)
  (let ((visited (make-hash-table)))
    (labels ((traverse (node)
               (unless (gethash node visited)
                 (setf (gethash node visited) t)
                 (mapc (lambda (edge)
                         (traverse edge))
                       (gethash node edge-tab)))))
      (traverse node))
    visited))

(time (dotimes (i 100)
        (get-connected-hash 1 (hash-edges (make-edge-list)))))
;; faster
