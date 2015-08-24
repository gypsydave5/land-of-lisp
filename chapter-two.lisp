(let
    ;; let is the scope around declarations
    ;; here's the declarations
    ((a 5)
     (b 10))
  ;; and now we do stuff
  (+ a b))

(flet
    ;; this is the same but for functions
    ((add-seven (n)
		(+ n 7)))
  (add-seven 7)) ;; seven and seven is...

(flet
    ((times-two (n)
		(* n 2))
     (plus-one (n)
	       (+ 1 n)))
  (plus-one (times-two 6)))

;; but the functions can't be referenced in each other
(flet
    ((times-two (n)
		(* n 2))
     (double-plus-one (n)
		      ((+ 1
			  (times-two n)))))
  (double-plus-one 6))
;; don't touch this - it doesn't work

;; to do this we need `labels`
(labels
    ((times-two (n)
		(* n 2))
     (double-plus-one (n)
		      ((+ (times-two n)
			  1))))
  (double-plus-one 6))
