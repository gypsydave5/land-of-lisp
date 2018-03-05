;; Numbers

(defun square (n) (* n n))
;; => SQUARE

(eq 'fooo 'FoOo)
;; => T

(+ 1 1.0)
;; => 2.0

(expt 53 53)
;; => 24356848165022712132477606520104725518533453128685640844505130879576720609150223301256150373

(/ 4 6)
;; => 2/3

(/ 4.0 6)
;; => 0.6666667

;; Strings
(princ "Tutti Frutti")
;; => Tutti Frutti
;;
;;    "Tutti Frutti"

(princ "He yelled \"Stop that thief!\" from the busy street")
;; => He yelled "Stop that thief!" from the busy street
;;
;;    "He yelled \"Stop that thief!\" from the busy street"

;; CODE
(expt 2 3)
;; => 8

;; DATA
'(expt 2 3)
;; => (EXPT 2 3)

;; Lists are cons cells
(cons 1 (cons 2 (cons 3 nil)))
;; => (1 2 3)
(list 1 2 3)
;; => (1 2 3)
'(1 2 3)
;; => (1 2 3)
(equal (cons 1 (cons 2 (cons 3 nil))) (list 1 2 3))
;; => T
(equal (cons 1 (cons 2 (cons 3 nil))) '(1 2 3))
;; => T

(let ((l '((peas carrots tomatoes) (pork beef chicken) duck)))
  (list (caddr l) (car (cddadr l))))
;; => (DUCK CHICKEN)

