                                        ; the wizards adventure game
                                        ; nodes (or places)
;; set the scene
(defparameter *nodes*
  '((living-room
     (you are in the living room.
      a wizard is snoring loudly on the couch.))
    (garden
     (you are in a beautiful garden.
      there is a well in front of you.))
    (attic
     (you are in the attic.
      there is a giant welding torch in the corner.))))
;; *NODES*

;; we can access this associated list (alist) with (assoc)
(assoc 'garden *nodes*)
;; (GARDEN (YOU ARE IN A BEAUTIFUL GARDEN. THERE IS A WELL IN FRONT OF YOU))
;; so a get location function can look like this:
(defun describe-location (location nodes)
  (cadr (assoc location nodes))) ;; DESCRIBE-LOCATION

(describe-location 'attic *nodes*)
;; (YOU ARE IN THE ATTIC. THERE IS A GIANT WELDING TORCH IN THE CORNER.)
                                        ; edges (or paths)
;; another data structure
(defparameter *edges*
  '((living-room (garden west door)
                (attic upstairs ladder))
   (garden (living-room east door))
   (attic (living-room downstairs ladder)))) ;; *EDGES*
;; and a way of constructing a description
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.)) ;; DESCRIBE-PATH

(describe-path '(garden west door)) ;; (THERE IS A DOOR GOING WEST FROM HERE)
;; this is QUASI-QUOTING - we're used to quoting Lisp code with a ' - but
;; we can also do it with a ` - and this allows us to disquote sexps inside
;; using ,. compare
;; '(,(+ 5 5) green bottles) ;; ERROR - COMMA NOT INSIDE A BACK QUOTE
'(\,(+ 5 5) green bottles) ;; (|,| (+ 5 5) GREEN BOTTLES)
`(,(+ 5 5) green bottles) ;; (10 GREEN BOTTLES)
`(\,(+ 5 5) green bottles) ;; (|,| (+ 5 5) GREEN BOTTLES)
                                        ; multiple paths at once
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))
    ;; DESCRIBE-PATHS
;; note the cdr - we're guaranteed to get a list back :D,
;; append - weel, it's join
;; apply - apply the function to the following args
;; mapcar apply the function to each membe of the list
(describe-paths 'living-room *edges*)
    ;; (THERE IS A DOOR GOING WEST FROM HERE.
    ;; THERE IS A LADDER GOING UPSTAIRS FROM HERE.)
;; mapcar - in action yo
(mapcar #'floor (mapcar #'sqrt '(1 4 9 16 25))) ;; (1 2 3 4 5)
(mapcar (lambda (x) (* x x)) '(1 2 3 4 5)) ;; (1 4 9 16 25)
;; #' is a shorthand for the function operator
(equal #'+ (function +)) ;; T (good to know that)
;; the idea is to prevent namespacing issues between variables and functions
;; (seems a little hand-holdy to me, but whatevs
;; note that Scheme doesn't have this problem/advantage (delete as applicable)
;; append
(append '(mary mary) '(quite contrary) '(how) '(are you today?))
    ;; (MARY MARY QUITE CONTRARY HOW ARE YOU TODAY?)
(append '(mary mary) '(quite contrary) '(how) '(are you) 'today?)
    ;; (MARY MARY QUITE CONTRARY HOW ARE YOU . TODAY?)
;; (append '(mary mary) '(quite contrary) '(how) '(are) 'you 'today?)
    ;; 'YOU is not a type of list
;; see how append handles the atoms
;; and apply
(apply #'+ '(1 2 3 4)) ;; 10
                                        ; objects
(defparameter *objects* '(whiskey bucket frog chain)) ;; *OBJECTS*
;; and objects are places
(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden))) ;; *OBJECT-LOCATIONS*
(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
             (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs))) ;; OBJECTS-AT
;; 1 - labels allows the function to be referenced within scope (could use flet)
;; 2 - the function at-loc-p tests whether the objects associated location
;;     is the location passed in. Convention dictates that fuctions which
;;     return a nil/truth we append -p - think a Ruby ?
;; 3 - remove-if-not applies at-loc-p to each obj in objs, and removes it if
;;     if the function returns nil, returning the filtered list
(objects-at 'living-room *objects* *object-locations*) ;; (WHISKEY BUCKET)

                                        ; describing visible objects
(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
           `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj
                            (objects-at loc objs obj-loc))))) ;; DESCRIBE-OBJECTS

(describe-objects 'living-room *objects* *object-locations*)
;; (YOU SEE A WHISKEY ON THE FLOOR. YOU SEE A BUCKET ON THE FLOOR.)

                                        ; putting it together
;; the player needs a location
(defparameter *location* 'living-room) ;; *LOCATION*
;; and a way to look around..
(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*))) ;; LOOK

(look) ;; (YOU ARE IN THE LIVING ROOM. A WIZARD IS SNORING LOUDLY ON THE COUCH THERE IS A
       ;; DOOR GOING WEST FROM HERE. THERE IS A LADDER GOING UPSTAIRS FROM HERE. YOU SEE
       ;; A WHISKEY ON THE FLOOR. YOU SEE A BUCKET ON THE FLOOR.
;; am I reading global variables? Then I am not functional. Dysfunctional?

                                        ; walking
(defun walk (direction)
  (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))
    (if next
        (progn (setf *location* (car next))
               (look))
        '(you cannot go that way))))
;; some exciting stuff there
;; :key is, well, a keyword parameter for the find function, which
;; tells it to use the the #'cadr of the elements in the list to
;; select against direction on - use it as the key- i.e.
(find 'y (list '(5 x) '(3 y) '(1 z)) :key (function cadr)) ;; (3 Y)
(find 'y '((5 x) (3 y) (1 z)) :key #'cadr) ;; (3 Y)
;; or even
(find 'x '((x 1) (y 2) (z 3)) :key #'car) ;; (X 1)
(cadr (find 'x '((x 1) (y 2) (z 3)) :key #'car)) ;; 1
;; here's the data structure we're looking at:
(assoc *location* *edges*) ;; (LIVING-ROOM (GARDEN WEST DOOR) (ATTIC UPSTAIRS LADDER))
;; so we get hold of the cdr
(cdr (assoc *location* *edges*)) ;; ((GARDEN WEST DOOR) (ATTIC UPSTAIRS LADDER))
;; and then match the element in that list that has a cadr (2nd element)
;; that matches location.
;; if it exists, we change the location to the car of the match (the location)
;; and take a look
;; otherwise we return an error message.
(walk 'west)
;; (YOU ARE IN A BEAUTIFUL GARDEN. THERE IS A WELL IN FRONT OF YOU. THERE IS A
;;  DOOR GOING EAST FROM HERE. YOU SEE A FROG ON THE FLOOR. YOU SEE A CHAIN ON THE
;;  FLOOR.)
                                        ; picking things up
;; because what's an adventure without the infinite pockets?
(defun pickup (object)
  (cond ((member object
                 (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
        (t '(you cannot get that)))) ;; PICKUP
;; pushing the new object location into the list essentially overwrites
;; the location as assoc will only ever return the first match -
;; a useful little hack - no need to remove the original entries too,
;; so we're left with a history. Push works as expected:
(defparameter *a-list* (list 1 2 3 4))  ;; *A-LIST*
(push 5 *a-list*) ;; (5 1 2 3 4)
*a-list* ;; (5 1 2 3 4)
(walk 'east)
;; (YOU ARE IN THE LIVING ROOM. A WIZARD IS SNORING LOUDLY ON THE COUCH. THERE IS
;; A DOOR GOING WEST FROM HERE. THERE IS A LADDER GOING UPSTAIRS FROM HERE. YOU
;; SEE A WHISKEY ON THE FLOOR. YOU SEE A BUCKET ON THE FLOOR.)
(pickup 'whiskey) ;; (YOU ARE NOW CARRYING THE WHISKEY)
;; need to implement a drink function...

;; now an inventory
(defun inventory ()
  (cons 'items\: (objects-at 'body *objects* *object-locations*))) ;; INVENTORY
(inventory) ;; (|ITEMS:| WHISKEY)
