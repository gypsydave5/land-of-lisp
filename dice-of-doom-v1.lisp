(defpackage #:dice-of-doom)
(in-package #:dice-of-doom)
(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 2)
(defparameter *board-hexnum* (* *board-size* *board-size*))

(make-array 5)
;; => #(NIL NIL NIL NIL NIL)
(make-array 5 :initial-element 'bob)
;; => #(BOB BOB BOB BOB BOB)
(make-array 5 :initial-contents '(1 2 3 4 5))
;; => #(1 2 3 4 5)

(defun board-array (lst)
  (make-array *board-hexnum* :initial-contents lst))

(board-array '(1 2 3 4))
;; => #(1 2 3 4)

(random 5)
;; => 4
(loop for n below 4
   collect 1)
;; => (1 1 1 1)

(defun gen-board  ()
  (board-array (loop for n below *board-hexnum*
                  collect (list (random *num-players*)
                                (1+ (random *max-dice*))))))
(gen-board)
;; => #((1 3) (0 1) (0 2) (0 2))

(code-char 97)
;; => #\a
(defun player-letter (n)
  (code-char (+ 97 n)))
(player-letter 1)
;; => #\b

(defparameter *test-board* #((0 0) (1 1) (2 2) (3 3)))
;; => *TEST-BOARD*

(defun draw-row (board row-num)
  (loop for x below *board-size*
     for hex = (aref board (+ x (* *board-size* row-num)))
     do (format t "~a-~a " (player-letter (first hex)) (second hex))))
(draw-row *test-board* 1)
;; => c-2 d-3
;;
;;    NIL
(draw-row *test-board* 0)
;; => a-0 b-1
;;
;;    NIL

(defun indent (indent-size)
  (loop repeat indent-size
     do (princ "  ")))
;; => INDENT

(defun draw-board (board)
  (loop for row-num below *board-size*
     do (progn (fresh-line)
               (indent (- *board-size* row-num))
               (draw-row board row-num))))
;; => DRAW-BOARD

(draw-board *test-board*)
;; =>     a-0 b-1
;;      c-2 d-3
;;
;;    NIL
