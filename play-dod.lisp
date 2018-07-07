(load "dice-of-doom-v1")
(in-package #:dice-of-doom)
(setf *random-state* (make-random-state t))
(play-vs-computer (game-tree (gen-board) 0 0 t))
