					; lists
;; all of Lisp is... lists!
;; (a b c d)
;; won't work because Lisp will try to evaluate it - and it's got no
;; idea what a b c d are
;;
;; so we can escape them with...

'(a b c d)

;; and there's our list

					; symbols
;;are words - and are case insensitive
(eq 'bob 'BOB)  
;; is true

					; numbers
;; Are either ints or floats, with floats being communicative
(+ 1 1.0) ;; 2.0
(+ 1 1) ;; 2
;; and integers will behave... rationally :D
(/ 1 2) ;; 1/2
;; but floats don't
(/ 1 2.0) ;; 0.5

					; strings
;; only "double" quotes 'singles' are escapes in Lisp, donchaknow...
(princ "one") ;; one "one"
;; here we evaluate to the string "one", but also have the side effect of printing "one"
;; and we can have escape characters too (I hope)...
(princ "the sentence \"this sentence is false\" is false.")

					; data mode and code mode
;; everything is data! everything is code! Anything can be quoted or disquoted! Behold:
(expt 2 2) ;; 4
;; but also
'(expt 2 2) ;; (EXPT 2 2)
;; hmmm
(let ((code '(expt 2 2)))
  (eval code)) ;; 4
;; the miracles of eval - ahahaaaa!

					; cons cells
;; if the bricks are symbols, number and strings, the mortar is cons cells
;; eh?
(cons 1 2) ;; (1 . 2)
;; (1 . 2) is a cons cell, the dot joins them together
;; there are two parts of a cons cell, the car:
(car (cons 'first 'last)) ;; FIRST
;; and the cdr:
(cdr (cons 'first 'last)) ;; LAST
;;; ( C-U C-X C-E is bloody marvelous)

;; you can cons onto Anything
(cons 'me "and my girl") ;; (ME . "and my girl")
;; even other conses
(cons 'me (cons 'myself 'and-I)) ;; (ME MYSELF . AND-I)
;; including nil
(cons "all by myself" nil) ;; ("all by myself")
;; hey - he looks like list - and he is
;; all lists in Lisp are a series of linked cons cells where the final cell's cdr points to nil
(cons 'me (cons 'myself (cons 'and-I nil))) ;; (ME MYSELF AND-I)
;; which is the same as...
(list 'me 'myself 'and-I) ;; (ME MYSELF AND-I)
;; and as
'(me 'myself 'and-I) ;; (ME MYSELF AND-I)
;; simples :D

					; car cdr
;; so we access the first in a list with car
(car '(first last my-everything)) ;; FIRST
;; and cdr gives us the rest
(cdr '(first last my-everything)) ;; (LAST MY-EVERYTHING)
;; but we can get nesty...
(car (car '((bill ben) weed))) ;; BILL
;; or even
(cdr (car '((bill ben) weed))) ;; (BEN)
;; which can be written
(cdar '((bill ben) weed)) ;; (BEN)
;; and so on
(cadar '((bill ben) weed)) ;; BEN
(cadr '((bill ben) weed)) ;; WEED
(cadaar '(((bill ben) weed))) ;; BEN
(cadddr '(one two three four)) ;;  FOUR
;; maaadness! 
