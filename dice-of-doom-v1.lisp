(defpackage #:dice-of-doom)
(in-package #:dice-of-doom)
(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 3)
(defparameter *board-hexnum* (* *board-size* *board-size*))

(make-array 5)
;; => #(NIL NIL NIL NIL NIL)
(make-array 5 :initial-element 'bob)
;; => #(BOB BOB BOB BOB BOB)
(make-array 5 :initial-contents '(1 2 3 4 5))
;; => #(1 2 3 4 5)

(defun board-array (lst)
  (make-array *board-hexnum* :initial-contents lst))

(random 5)
;; => 4
(loop for n below 4
   collect 1)
;; => (1 1 1 1)

(defun gen-board ()
  (board-array (loop for n below *board-hexnum*
                  collect (list (random *num-players*)
                                (1+ (random *max-dice*))))))
(gen-board)
;; => #((1 3) (0 1) (1 3) (1 2) (1 2) (1 1) (1 1) (1 3) (0 3))

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

;; The Game Tree-equal
(defun game-tree
    (board player spare-dice first-move)
  (list player
        board
        (add-passing-move board
                          player
                          spare-dice
                          first-move
                          (attacking-moves board
                                           player
                                           spare-dice))))
;; => GAME-TREE

(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
      moves
      (cons (list nil
                  (game-tree (add-new-dice board player (1- spare-dice))
                             (mod (1+ player) *num-players*)
                             0
                             t))
            moves)))
;; => ADD-PASSING-MOVE

(mapcan (lambda (x) (list 1 x)) '(5 6 7 8))
;; => (1 5 1 6 1 7 1 8)

(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
             (car (aref board pos)))
           (dice (pos)
             (cadr (aref board pos))))

    (mapcan (lambda (src)
              (when (eq (player src) cur-player)
                (mapcan (lambda (dst)
                          (when (and (not (eq (player dst) cur-player))
                                     (> (dice src) (dice dst)))
                            (list
                             (list (list src dst)
                                   (game-tree (board-attack board cur-player src dst (dice src))
                                              cur-player
                                              (+ spare-dice (dice dst))
                                              nil)))))
                        (neighbors src))))
            (loop for n below *board-hexnum*
               collect n))))
;; => ATTACKING-MOVES

(append '(1 2) '(3 4 5))
;; => (1 2 3 4 5)

(defun neighbors (pos)
  (let ((up (- pos *board-size*))
        (down (+ pos *board-size*)))
    (loop for p in (append (list up down)
                           (unless (zerop (mod pos *board-size*))
                             (list (1- up) (1- pos)))
                           (unless (zerop (mod (1+ pos) *board-size*))
                             (list (1+ pos) (1+ down))))
       when (and (>= p 0) (< p *board-hexnum*))
       collect p)))
;; => NEIGHBORS

(defun board-attack (board player src dst dice)
  (board-array (loop for pos
                  for hex across board
                  collect (cond ((eq pos src) (list player 1))
                                ((eq pos dst) (list player (1- dice)))
                                (t hex)))))
;; => BOARD-ATTACK

(defun add-new-dice (board player spare-dice)
  (labels ((recur (lst n)
             (cond ((null lst) nil)
                   ((zerop n) lst)
                   (t (let ((cur-player (caar lst))
                            (cur-dice (cadar lst)))
                        (if (and (eq cur-player player)
                                 (< cur-dice *max-dice*))
                            (cons (list cur-player (1+ cur-dice))
                                  (recur (cdr lst) (1- n)))
                            (cons (car lst) (recur (cdr lst) n))))))))
    (board-array (recur (coerce board 'list) spare-dice))))
;; => ADD-NEW-DICE

;; Good against the living

(defun play-vs-human (tree)
  (print-info tree)
  (if (caddr tree)
      (play-vs-human (handle-human tree))
      (announce-winner (cadr tree))))
;; => PLAY-VS-HUMAN

(defun print-info (tree)
  (fresh-line)
  (format t "current player = ~a" (player-letter (car tree)))
  (draw-board (cadr tree)))
;; => PRINT-INFO

(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (caddr tree)))
    (loop for move in moves
       for n from 1
       do (let ((action (car move)))
            (fresh-line)
            (format t "~a. ." n)
            (if action
                (format t "~a -> ~a" (car action) (cadr action))
                (princ "end turn"))))
    (fresh-line)
    (cadr (nth (1- (read)) moves))))
;; => HANDLE-HUMAN

(defun winners (board)
  (let* ((tally (loop for hex across board
                   collect (car hex)))
         (totals (mapcar (lambda (player)
                           (cons player (count player tally)))
                         (remove-duplicates tally)))
         (best (apply #'max (mapcar #'cdr totals))))
    (mapcar #'car
            (remove-if (lambda (player-score)
                         (not (eq (cdr player-score) best)))
                       totals))))
;; => WINNERS

(defun announce-winner (board)
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
        (format t "The game is a tie between ~a" (mapcar #'player-letter w))
        (format t "The winner is ~a" (player-letter (car w))))))
;; => ANNOUNCE-WINNER

;; minimax
(defun rate-position (tree player)
  (let ((moves (caddr tree)))
    (if moves
        (apply (if (eq (car tree) player)
                   #'max
                   #'min)
               (get-ratings tree player))
        (let ((w (winners (cadr tree))))
          (if (member player w)
              (/ 1 (length w))
              0)))))
;; => RATE-POSITION

(defun get-ratings (tree player)
  (mapcar (lambda (move)
            (rate-position (cadr move) player))
          (caddr tree)))
;; => GET-RATINGS

(defun handle-computer (tree)
  (let ((ratings (get-ratings tree (car tree))))
    (cadr (nth (position (apply #'max ratings) ratings)
               (caddr tree)))))
;; => HANDLE-COMPUTER

(defun play-vs-computer (tree)
  (print-info tree)
  (cond ((null (caddr tree)) (announce-winner (cadr tree)))
        ((zerop (car tree)) (play-vs-computer (handle-human tree)))
        (t (play-vs-computer (handle-computer tree)))))
;; => PLAY-VS-COMPUTER

;; Memoization

(let ((old-neighbors (symbol-function 'neighbors))
      (previous (make-hash-table)))
  (defun neighbors (pos)
    (or (gethash pos previous)
        (setf (gethash pos previous) (funcall old-neighbors pos)))))
;; => NEIGHBORS

(labels ((square (x) (* x x)))
  (let ((previous (make-hash-table)))
    (defun square-mem (x)
      (or (gethash x previous)
          (setf (gethash x previous) (funcall #'square x))))))
;; => SQUARE-MEM

(let ((old-game-tree (symbol-function 'game-tree))
      (previous (make-hash-table :test #'equalp)))
  (defun game-tree (&rest rest)
    (or (gethash rest previous)
        (setf (gethash rest previous)
              (apply old-game-tree rest)))))
;; => GAME-TREE

(let ((old-rate-position (symbol-function 'rate-position))
      (previous (make-hash-table)))
  (defun rate-position (tree player)
    (let ((tab (gethash player previous)))
      (unless tab
        (setf tab (setf (gethash player previous) (make-hash-table))))
      (or (gethash tree tab)
          (setf (gethash tree tab)
                (funcall old-rate-position tree player))))))
;; => RATE-POSITION

;; TCO
(defun my-length (lst)
  (labels ((f (lst acc)
             (if lst
                 (f (cdr lst) (1+ acc))
                 acc)))
    (f lst 0)))
;; => MY-LENGTH
(compile 'my-length)
;; => MY-LENGTH
;;    NIL
;;    NIL
(my-length (loop for i below 100000 collect 'x))
;; => 100000

(defun add-new-dice (board player spare-dice)
  (labels ((recur (lst n acc)
             (cond ((zerop n) (append (reverse acc) lst))
                   ((null lst) (reverse acc))
                   (t (let ((cur-player (caar lst))
                            (cur-dice (cadar lst)))
                        (if (and (eq cur-player player)
                                 (< cur-dice *max-dice*))
                            (recur (cdr lst)
                                   (1- n)
                                   (cons (list cur-player (1+ cur-dice)) acc))
                            (recur (cdr lst)
                                   n
                                   (cons (car lst) acc))))))))
    (board-array (recur (coerce board 'list) spare-dice ()))))
;; => ADD-NEW-DICE
(compile 'add-new-dice)
;; => ADD-NEW-DICE
;;    NIL
;;    NIL
