                                        ; lisp is symetry
;; 1 - an empty list is false
(if '()
    'true!
    'false!) ;; FALSE!
;; 1.1 - a non empty list is true
(if '(1)
    'true!
    'false!) ;; TRUE!

;; This is handy when recurrring

(defun list-length (list)
  (if list
      (1+ (list-length (cdr list)))
      0))

(list-length '(1 2 3 4 5 'monkey)) ;; 6
;; when the list is false - when it's empty - it returns 0 (base case)
;; otherwise it returns 1 plus the length of the rest (the cdr) of the list
;; nice
                                        ; !massive digression!
;; ps - like that (1+) ??
(defun plus-one (number)
  (+ 1 number))
(plus-one 1) ;; 2 
;; or even
(defun my-partial (fun &rest args)
  (lambda (&rest more-args)
    (apply fun (append args more-args))))
(setf (symbol-function 'plus-one) (my-partial #'+ 1)) ;; don't ask me...
(plus-one 1) ;; 2 
;; still no idea how that #' is affecting + - some more magic...
;; looking at how easy it is to create a partial on the fly, it's no small
;; wonder that the function isn't built in - maybe this would be better:
(defun plus-one (&rest numbers)
  (apply #'+ (cons 1 numbers)))
;; will find out about that soon enough - summoning a function as a symbol - a symbol function???
;; we generate the argument list by cons'ing one with the numbers, - an alternative
;; would be (append '(1) numbers) - as append can only take list arguments
(plus-one 5) ;; 6
;; Lisp is fun!
                                        ; !digression ends!
;; and now...
                                        ; the symetry of nil!
(eq nil ()) ;; T
(eq nil '()) ;; T
(eq 'nil ()) ;; T
;; and so on - do you really want me to prove that transitivity holds
;; c'mon - this ain't JavaScript :D
;; (this also ain't Scheme - where 'false' and 'empty' are two different things)

                                        ; conditionals
;; if
(if (= (+ 1 1) 2)
    'yup
    'nope) ;; YUP

(if '(1)
    'the-list-has-stuff-in-it
    'the-list-is-empty) ;; THE-LIST-HAS-STUFF-IN-IT

(if '()
    'the-list-has-stuff-in-it
    'the-list-is-empty) ;; THE-LIST-IS-EMPTY
;; and it can be used as an escape
(if (oddp 5)
    'yes-five-is-odd
    (/ 1 0)) ;; YES-FIVE-IS-ODD
;; 'if' isn't evaluating all of the expressions held within it, which is what
;; most Lisp functions would do - 'if' is a SPECIAL FORM - special priviledges
;;; were we to want to be super un functional, we could consider a way of having
;;; functions do more than one thing - even unto some of those DREADED side effects
(defvar *number-was-odd* nil)
*number-was-odd* ;; 
(if (oddp 5)
    (progn (setf *number-was-odd* t)
           'odd-number)
    'even-number) ;; ODD-NUMBER
;; and now also...
*number-was-odd* ;; T
;; ther are two functions that have an implicit progn
(when (oddp 5)
  (setf *number-was-odd* 'boom)
  'odd-number) ;; ODD-NUMBER
*number-was-odd* ;; BOOM
(unless (oddp 4)
  (setf *number-was-odd* nil)
  'its-even) ;; ITS-EVEN

;; we also have the swiss-army conditional 'cond'
(defvar *arch-enemy* nil)
(defun pudding-eater (person)
  (cond ((eq person 'henry)
         (setf *arch-enemy* 'stupid-lisp-alien)
         '(curse you lisp alien you ate my pudding))
        ((eq person 'johnny)
         (setf *arch-enemy* 'useless-old-johnny)
         '(i hope you choke on my pudding johnny))
        (t
         '(why you eat my pudding stranger))))
;; each cond section consists of an expression, evaluating to true or
;; false, followed by a series of other expressions which are evalueated
;; in turn if the first one is true
;; The first successful match returns - the rest aren't run
;; the t at the end is the Lisp idiom for a default behaviour
(pudding-eater 'henry) ;; (CURSE YOU LISP ALIEN YOU ATE MY PUDDING)
*arch-enemy* ;; STUPID-LISP-ALIEN
(pudding-eater 'spike) ;; (WHY YOU EAT MY PUDDING STRANGER)

;; one more conditional ... case makes a cond a lot shorter by defaulting
;; to using eq to compare to a single argument:
(defvar *arch-enemy* nil)
(defun pudding-eater (person)
  (case person
    ((johnny) (setf *arch-enemy* 'useless-old-johnny)
     '(bah johnny stop stealing my pudding))
    ('henry (setf *arch-enemy* 'stupid-lisp-alien)
            '(you again lisp alien baaaaah))
    (otherwise '(why you keep eating my pudding stranger)))) ;; PUDDING-EATER
(pudding-eater 'henry) ;; (YOU AGAIN LISP ALIEN BAAAAAH)
(pudding-eater 'johnny) ;; (BAH JOHNNY STOP STEALING MY PUDDING)

                                        ; and & or
;; are short circuity conditionals of joy
(defvar *is-even* nil)
(or (oddp 4) (setf *is-even* t)) ;; T
*is-even* ;; T
(setf *is-even* nil) 
(or (oddp 5) (setf *is-even* t)) ;; T
*is-even* ;; NIL

;; the last expression not evaluating if the first is already true
;; and vice versa
(setf *is-even* t) ;; T
(and (oddp 4) (setf *is-even* nil)) ;; NIL
(setf *is-even* t) ;; T
(and (oddp 5) (setf *is-even* nil)) ;; NIL
*is-even* ;; NIL 

;; and we can do fun things like this:
(and *file-modified* (ask-user-about-saving) (save-file))

                                        ; more than the truth
;; because everything is truthy, unless it's empty/nil, boolean functions can
;; return much more useful things than just t
(member 1 '(5 4 1 2 3)) ;; (1 2 3)
(member 1 '(3 5 6 8 3 1 7)) ;; (1 7)
;; i.e. yes, it's in here, and it and the following are...
;; almost like eql for the car of each cdr, then return the cdr
;; and we get the tail because
(member nil '(1 2 5 nil)) ;; (NIL)
;; yes, a list with nil in it is still true :D

;; look, there are a few fun ways to do the same...
(find-if #'oddp '(2 4 6 7 8)) ;; 7
(if (find-if #'oddp '(2 4 6 7 8))
    'there-is-an-odd-number
    'there-are-no-odd-numbers) ;;THERE-IS-AN-ODD-NUMBER
;; but alas...
(find-if #'null '(1 2 null 5)) ;; NIL
(find-if #'null '(1 2 5)) ;; NIL

                                        ; comparisons
;; so, you can compare symbols with eq
(eq 'bob 'bob) ;; T
;; but not for anything else
(defparameter *one* 1) ;; *ONE*
(eq *one* 1) ;;
;; ok, well apparently it might
(eq *one* 1.0) ;; NIL
;; but we can use
(eq '(1 2 3) '(1 2 3)) ;; NIL
(equal '(1 2 3) '(1 2 3)) ;; T
(equal (list 1 2 3) '(1 2 3)) ;; T
(eq 1 (/ 2 2)) ;; 
(eq "bob" "bob") ;; NIL 
(equal "bob" "bob") ;; T
;; there's also
(= 1 1) ;; T
(equalp "bob" "BOB") ;; T
(eq #\a #\a) ;; T
(eql #\z #\z) ;; T
;; and for floats
(equalp 1 1.0) ;; T
;; there's probably a lot more going on here
