(defparameter *nodes* '((living-room
                         (you are in the living-room. a wizard is snoring loudly on the couch.))
                        (garden
                         (you are in a beautiful garden. there is a well in front of you.))
                        (attic (you are in the attic. there is a giant welding torch in the corner.))))

(assoc 'garden *nodes*)
;; =>
;; (GARDEN (YOU ARE IN A BEAUTIFUL GARDEN. THERE IS A WELL IN FRONT OF YOU.))

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))
(describe-location 'living-room *nodes*)
;; => (YOU ARE IN THE LIVING-ROOM. A WIZARD IS SNORING LOUDLY ON THE COUCH.)

(defparameter *edges* '((living-room
                         (garden west door)
                         (attic upstairs ladder))
                        (garden
                         (living-room east door))
                        (attic
                         (living-room downstairs ladder)))
  "assoc list of (location &edge), where edges are (destination direction means)")

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(describe-path '(garden west door))
;; => (THERE IS A DOOR GOING WEST FROM HERE.)

;; alternative
(defun direction (edge)
  (cadr edge))
(defun means (edge)
  (caddr edge))
(defun better-describe-path (edge)
  `(there is a ,(means edge) going ,(direction edge) from here.))
;; /alternative

(defun describe-paths (location edges)
  (apply #'append
         (mapcar #'describe-path (cdr (assoc location edges)))))

(describe-paths 'living-room *edges*)
;; =>
;; (THERE IS A DOOR GOING WEST FROM HERE. THERE IS A LADDER GOING UPSTAIRS FROM
;;  HERE.)

(defparameter *objects* '(whiskey bucket frog chain))
(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))
(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
             (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))
(objects-at 'living-room *objects* *object-locations*)
;; => (WHISKEY BUCKET)

(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
             `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))
(describe-objects 'living-room *objects* *object-locations*)
;; => (YOU SEE A WHISKEY ON THE FLOOR. YOU SEE A BUCKET ON THE FLOOR.)

(defparameter *location* 'living-room)

(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))
(look)
;; =>
;; (YOU ARE IN THE LIVING-ROOM. A WIZARD IS SNORING LOUDLY ON THE COUCH. THERE IS
;;  A DOOR GOING WEST FROM HERE. THERE IS A LADDER GOING UPSTAIRS FROM HERE. YOU
;;  SEE A WHISKEY ON THE FLOOR. YOU SEE A BUCKET ON THE FLOOR.)

(defun walk (direction)
  (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))
    (if next
        (progn (setf *location* (car next))
               (look))
        '(you cannot go that way.))))
(walk 'down)
;; => (YOU CANNOT GO THAT WAY.)

(defun pickup (object)
  (cond ((member object (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
        (t '(you cannot get that.))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))
