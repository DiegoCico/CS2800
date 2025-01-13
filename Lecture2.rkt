#lang lsl

;; my-abs: Intger - Natural

;; p1 : Boolean Boolean Boolean -> Boolean
;; : is a contract making sure it intakes the correct contracts
(: p1 (-> Boolean Boolean Boolean Boolean))
   (define (p1 P Q R)
   (or P (and Q R)))


;; giving example of a random type
(contract-generate Integer)
;; returns a random integer
(contract-generate Real)
;; returns a random real number
(contract-generate String)
;; returns a random String
(contract-generate Boolean)
;; returns a random Boolean

(: my-abs (-> Integer Natural))
(define (my-abs x)
        (if (< x 0)
            x
            (- x)))

;; to check/test your contrat you can do
(check-contract my-abs)

;; letter-grade : Integer -> String
(: letter-grade (-> Integer String))
(define (letter-grade n)
  (cond [(>= n 90) "A"]
        [(>= n 80) "B"]
        [(>= n 70) "C"]
        [(>= n 60) "D"]
        [else "F"]))

(check-contract letter-grade)

;; True
(: letter-grade-prop (-> Integer True))
(define (leter-grade-prop n)
  (member? (letter-grade n)
  (list "A" "B" "C" "D" "F")))

;; does 1000 test
(check-contract leter-grade-prop 1000)