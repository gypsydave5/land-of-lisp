                                        ; going beyond basic lists
;; the difference between
(cons 1 (cons 2 (cons 3 ())))
;; and
'(1 2 3)
;; are _entirely_ superficial...

;; but we can get a little mental...
(cons 1 (cons 2 3)) ;; (1 2 . 3)
;; this is a dotted list

;; it's just another way of expressing a situation
'(1 . (2 . (3 . nil))) ;; (1 2 3)

                                        ;pairs
;; nothing beats a pair...
;; ... for expressing two things together
(cons 1 4) ;; (1 . 4)
(car (cons 1 4)) ;; 1
(cdr (cons 1 4)) ;; 4

                                        ; circularity in lists
(setf *print-circle* t) ;; T
;; this lets us get LOCO

(defparameter foo '(1 2 3)) ;; FOO
(setf (cdddr foo) foo)

(mapcar (lambda (n) (* n n)) foo) ;; never, ever run this

(cdr foo) ;; #1=(2 3 1 . #1#)
(cdddr foo) ;; #1=(1 2 3 . #1#)

                                        ; a(ssociation )lists

(defparameter *drinks-order* '((dave . weissbier)
                               (chris . gin)
                               (dan . horrible)
                               (roy . citra))) ;; *DRINKS-ORDER*

(assoc 'dave *drinks-order*) ;; (DAVE . WEISSBIER)
(assoc 'dan *drinks-order*) ;; (DAN . HORRIBLE)
(push '(dan . something-better) *drinks-order*)
;; ((DAN . SOMETHING-BETTER)
;; (DAVE . WEISSBIER)
;; (CHRIS . GIN)
;; (DAN . HORRIBLE)
;; (ROY . CITRA))
(assoc 'dan *drinks-order*) ;; (DAN . SOMETHING-BETTER)
(pop *drinks-order*) ;; (DAN . SOMETHING-BETTER)
(pop *drinks-order*) ;; (DAVE . WEISSBIER)
;; bit of a guess but it works!

                                        ; moar datas

;; lisp is good at trees

(defparameter *tree* '((roots (tap-root)
                        (creepers))
                       (trunk (bark (lichen))
                        (sap-wood (celulose)
                         (xylem)))
                       (branches (leaves (chlorophyll (magnesium)))
                        (twigs))))

;; but graphs are harder - remember that ruddy wizards house... ?

;; a dotviz library

(defun dot-name (exp)
  (substitute-if #\_
                 (complement #'alphanumericp)
                 (prin1-to-string exp))) ;; DOT-NAME
(dot-name "YO!") ;; "_YO__"
(dot-name '(yo mama)) ;; "_YO_MAMA_"
(dot-name 'living-room) ;; "LIVING_ROOM"

(substitute-if #\e #'digit-char-p "I'm a l33t hack3r") ;; "I'm a leet hacker"
;; remember, dude, the p is for predicate - t or f

(substitute-if 'w00t #'oddp '(1 2 3 4 5 6 7 8 9)) ;; (W00T 2 W00T 4 W00T 6 W00T 8 W00T)
;; could make fizzbuzz go quickly...

(alphanumericp #\a) ;; T
(alphanumericp #\!) ;; NIL
(complement #'oddp) ;; #<COMPILED-FUNCTION COMMON-LISP::COMPLEMENT-1>
(complement #'alphanumericp) ;; #<COMPILED-FUNCTION COMMON-LISP::COMPLEMENT-1>

(defparameter *max-label-length* 30) ;; *MAX-LABEL-LENGTH*
(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
        (if (> (length s) *max-label-length*)
            (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
            s))
      "")) ;; DOT-LABEL

(subseq "This is so awesome. Not!" 0 18) ;; "This is so awesome"
(dot-label 'this-is-a-label) ;; "THIS-IS-A-LABEL"
(dot-label 'this-label-is-almost-certainly-too-long) ;; "THIS-LABEL-IS-ALMOST-CERTAI..."
(dot-label ()) ;; ""

(defun nodes->dot (nodes)
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes)) ;; NODES->DOT

;; borrowing nodes from chapter 5ish
(nodes->dot *nodes*)
;; LIVING_ROOM[label="(LIVING-ROOM (YOU ARE IN TH..."];
;; GARDEN[label="(GARDEN (YOU ARE IN A BEAUT..."];
;; ATTIC[label="(ATTIC (YOU ARE IN THE ATTI..."];

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
        edges)) ;; EDGES->DOT

(edges->dot *edges*)
;; LIVING_ROOM->GARDEN[label="(WEST DOOR)"];
;; LIVING_ROOM->ATTIC[label="(UPSTAIRS LADDER)"];
;; GARDEN->LIVING_ROOM[label="(EAST DOOR)"];
;; ATTIC->LIVING_ROOM[label="(DOWNSTAIRS LADDER)"];

(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (fresh-line)
  (princ "}")) ;; GRAPH->DOT

(graph->dot *nodes* *edges*)
;; digraph{
;; LIVING_ROOM[label="(LIVING-ROOM (YOU ARE IN TH..."];
;; GARDEN[label="(GARDEN (YOU ARE IN A BEAUT..."];
;; ATTIC[label="(ATTIC (YOU ARE IN THE ATTI..."];
;; LIVING_ROOM->GARDEN[label="(WEST DOOR)"];
;; LIVING_ROOM->ATTIC[label="(UPSTAIRS LADDER)"];
;; GARDEN->LIVING_ROOM[label="(EAST DOOR)"];
;; ATTIC->LIVING_ROOM[label="(DOWNSTAIRS LADDER)"];
;; }

;; cool - a compiler...

;; from file to picture...

(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
                   fname
                   :direction :output
                   :if-exists :supersede)
    (funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -O " fname))) ;; DOT->PNG

;; so... a thunk... a delayed function to be called later... with no arguments...
;; a nullary function... a suspension

(with-open-file (my-stream
                 "testy-mc-filerson.txt"
                 :direction :output
                 :if-exists :supersede)
  (princ "Hello, file!" my-stream)) ;; "Hello, file!"
;; yup, that's defo had a side-effect

;; `my-stream` is defined and available for the scope of `with-open-file`
;; it is the stream in question

;; :direction :output - we're only outputting to the file
;; :if-exists :supersede - if it's there, we're just getting rid of it

;; colon prepended symbols always mean themselves unambiguously
:cigar ;; :CIGAR
(let ((:cigar '(a smoke)))
  princ :cigar) ;; LET: :CIGAR is a constant, may not be used as a variable

;; *standard-input* is the, well, the standard output stream - meaning that
;; the file will now capture anything we print to standard output.
;; this is interresting!
;; as the stream argument is lexically scoped to the `with-open-file` function
;; any function that writes to *standard-output* within that scope will write
;; to the file - we are redirecting standard output in the scope by changing
;; the meaning of the symbol *standard-output* locally.
;;
;; (even the calls to princ that are buried a few levels deep)
;;
;; so far, so AWESOMELY FUNCTIONAL

(defun graph->png (fname nodes edges)
  (dot->png fname (lambda ()
                    (graph->dot nodes edges)))) ;; GRAPH->PNG

(graph->png "wizard.dot" *nodes* *edges*) ;; NIL
;; more side-effects - go look in the file system...
;; pretty damn cool

;; undirected graphs
(defun uedges->dot (edges)
  (maplist (lambda (list)
             (mapc (lambda (edge)
                   (unless (assoc (car edge) (cdr list))
                     (fresh-line)
                     (princ (dot-name (caar list)))
                     (princ "--")
                     (princ (dot-name (car edge)))
                     (princ "[label=\"")
                     (princ (dot-label (cdr edge)))
                     (princ "\"];")))
             (cdar lst)))
  edges)) ;; UEDGES->DOT

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (ugraph->dot nodes edges))))

(ugraph->png "uwizard.dot" *nodes* *edges*)
