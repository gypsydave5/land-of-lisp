;; PAIRS
(cons 2 3)
;; => (2 . 3)

;; CIRCLES
;; set this first
(setf *print-circle* t)
(defparameter foo (list 1 2 3))
(setf (cdddr foo) foo)
;; => #1=(1 2 3 . #1#)

;; ALIST
(defparameter *drink-order* '((bill . double-expresso)
                              (lisa . small-drip-coffee)
                              (john . medium-latte)))
(assoc 'lisa *drink-order*)
;; => (LISA . SMALL-DRIP-COFFEE)

(push '(lisa . large-mocha-with-whipped-cream) *drink-order*)
(assoc 'lisa *drink-order*)
;; => (LISA . LARGE-MOCHA-WITH-WHIPPED-CREAM)
*drink-order*
;; =>
;; ((LISA . LARGE-MOCHA-WITH-WHIPPED-CREAM) (BILL . DOUBLE-EXPRESSO)
;;  (LISA . SMALL-DRIP-COFFEE) (JOHN . MEDIUM-LATTE))


;; =>
;; ((LIVING-ROOM
;;   (YOU ARE IN THE LIVING-ROOM. A WIZARD IS SNORING LOUDLY ON THE COUCH.))
;;  (GARDEN (YOU ARE IN A BEAUTIFUL GARDEN. THERE IS A WELL IN FRONT OF YOU.))
;;  (ATTIC (YOU ARE IN THE ATTIC. THERE IS A GIANT WELDING TORCH IN THE CORNER.)))

(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp)
                 (prin1-to-string exp)))
(dot-name 'w?h)
;; => "W_H"

(defparameter *max-label-length* 30)
(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
        (if (> (length s) *max-label-length*)
            (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
            s))
      ""))
(dot-label 'arsteesnsenerniaorstnoainerstinioenrast)
;; => "ARSTEESNSENERNIAORSTNOAINER..."

(defun nodes->dot (nodes)
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes))
(nodes->dot *nodes*)
;; =>
;; LIVING_ROOM[label="(LIVING-ROOM (YOU ARE IN TH..."];
;; GARDEN[label="(GARDEN (YOU ARE IN A BEAUT..."];
;; ATTIC[label="(ATTIC (YOU ARE IN THE ATTI..."];
;;
;; ((LIVING-ROOM
;;   (YOU ARE IN THE LIVING-ROOM. A WIZARD IS SNORING LOUDLY ON THE COUCH.))
;;  (GARDEN (YOU ARE IN A BEAUTIFUL GARDEN. THERE IS A WELL IN FRONT OF YOU.))
;;  (ATTIC (YOU ARE IN THE ATTIC. THERE IS A GIANT WELDING TORCH IN THE CORNER.)))

(defun edges->dot (edges)
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (fresh-line)
                  (princ (dot-name (car node)))
                  (princ "->")
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (dot-label (cdr edge)))
                  (princ "\"];"))
                (cdr node)))
        edges))
(edges->dot *edges*)
;; =>
;; LIVING_ROOM->GARDEN[label="(WEST DOOR)"];
;; LIVING_ROOM->ATTIC[label="(UPSTAIRS LADDER)"];
;; GARDEN->LIVING_ROOM[label="(EAST DOOR)"];
;; ATTIC->LIVING_ROOM[label="(DOWNSTAIRS LADDER)"];
;;
;; ((LIVING-ROOM (GARDEN WEST DOOR) (ATTIC UPSTAIRS LADDER))
;;  (GARDEN (LIVING-ROOM EAST DOOR)) (ATTIC (LIVING-ROOM DOWNSTAIRS LADDER)))

(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))
(graph->dot *nodes* *edges*)
;; => digraph{
;;    LIVING_ROOM[label="(LIVING-ROOM (YOU ARE IN TH..."];
;;    GARDEN[label="(GARDEN (YOU ARE IN A BEAUT..."];
;;    ATTIC[label="(ATTIC (YOU ARE IN THE ATTI..."];
;;    LIVING_ROOM->GARDEN[label="(WEST DOOR)"];
;;    LIVING_ROOM->ATTIC[label="(UPSTAIRS LADDER)"];
;;    GARDEN->LIVING_ROOM[label="(EAST DOOR)"];
;;    ATTIC->LIVING_ROOM[label="(DOWNSTAIRS LADDER)"];}
;;
;;    "}"

(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
                   fname
                   :direction :output
                   :if-exists :supersede)
    (funcall thunk))
  (sb-ext:run-program "/bin/sh" (list "-c" (concatenate 'string "dot -Tpng -O " fname))))

