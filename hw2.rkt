#lang lsl

;; Problem 1: 
;;
;; Translate the following expressions in logic into corresponding function definitions. 
;; Note that the number (and names) of arguments may be different for the different 
;; expressions, as they do not all use the same variables.

;; Template (part p1) to fill in:

;; (P /\ Q) \/  ~(R /\ S)
(define (p2 P Q R S)
  (or (and P Q) (not (and R S))))

;; P -> ~Q
(define (p3 P Q)
  (or (not P) (not Q)))

;; ~(P /\ Q) = ~ P \/ ~Q
(define (p4 P Q)
  (equal? (not (and P Q)) (or (not P) (not Q))))

;; part p1
;; Tests for p2 (P /\ Q) \/ ~(R /\ S)
(check-expect (p2 #t #t #t #t) #t) 
(check-expect (p2 #t #t #f #t) #t) 
(check-expect (p2 #f #t #t #t) #f)
(check-expect (p2 #f #f #f #f) #t)

;; Tests for p3 P -> ~Q
(check-expect (p3 #t #t) #f) 
(check-expect (p3 #t #f) #t) 
(check-expect (p3 #f #t) #t)
(check-expect (p3 #f #f) #t) 

;; Tests for p4 ~(P /\ Q) = ~ P \/ ~Q (De Morgan's Law)
(check-expect (p4 #t #t) #t) 
(check-expect (p4 #t #f) #t) 
(check-expect (p4 #f #t) #t) 
(check-expect (p4 #f #f) #t) 

;; Problem 2:
;;
;; Prove that the two following equalities (these are De Morgan's Laws) hold for all 
;; possible assignments (i.e., are _valid_) by first defining them (p5 and p6) 
;; and then defining their truth tables using check-expect. 
;; Remember to include all possible combinations of inputs!

;; Template (part p2) to fill in:

;; ~(P /\ Q) = ~P \/ ~Q
(define (p5 P Q)
  (equal? (not(and P Q)) (or (not P) (not Q))))

(check-expect (p5 #t #t) #t) 
(check-expect (p5 #t #f) #t) 
(check-expect (p5 #f #t) #t) 
(check-expect (p5 #f #f) #t) 
; ...

;; ~(P \/ Q) = ~P /\ ~Q
(define (p6 P Q)
  (equal? (not (or P Q)) (and (not P) (not Q))))
(check-expect (p6 #t #t) #t) 
(check-expect (p6 #t #f) #t)
(check-expect (p6 #f #t) #t)
(check-expect (p6 #f #f) #t) 
; ...

;; part p2

;; Problem 3:
;;
;; For each operator, define a version of it in terms of just `if`.
;; You are welcome to validate your encodings using truth-table tests, but 
;; you are not required. 

;; Template (part p3) to fill in:

;; /\
(define (op_and X Y)
  (if X
      (if Y #t #f)
      #f))

;; \/
(define (op_or X Y)
  (if X #t (if Y #t #f)))

;; ->
(define (op_implies X Y)
  (if X (if Y #t #f) #t))


;; =
(define (op_equal X Y)
  (if X
      (if Y #t #f)
      (if Y #f #t)))

;; ⊕ (exclusive or)
(define (op_xor X Y)
  (if X
      (if Y #f #t)  
      (if Y #t #f)))



;; part p3

(check-expect (op_and #t #t) #t)  
(check-expect (op_and #t #f) #f)  
(check-expect (op_and #f #t) #f)  
(check-expect (op_and #f #f) #f)

(check-expect (op_or #t #t) #t)
(check-expect (op_or #t #f) #t)
(check-expect (op_or #f #t) #t)
(check-expect (op_or #f #f) #f)

(check-expect (op_implies #t #t) #t)
(check-expect (op_implies #t #f) #f)
(check-expect (op_implies #f #t) #t)
(check-expect (op_implies #f #f) #t)

(check-expect (op_equal #t #t) #t)
(check-expect (op_equal #f #f) #t) 
(check-expect (op_equal #t #f) #f)  
(check-expect (op_equal #f #t) #f)

(check-expect (op_xor #t #t) #f)  
(check-expect (op_xor #t #f) #t)  
(check-expect (op_xor #f #t) #t) 
(check-expect (op_xor #f #f) #f) 



;; Problem 4:
;;
;; Perform simplifications to remove redundant variables for the three problems below, and include 
;; truth tables that confirm that your simplifications were correct. We are giving you the
;; expressions written both in logical syntax and in the LSL code that we expect
;; you to simplify & test with.

;; Template (part p4) to fill in:

;; (P /\ Q) /\ (R /\ ~Q)
(define (p9 P Q R)
  (and (and P Q) 
       (and R (not Q))))
(define (p9s P Q R) 
  #f)

;; (P /\ Q /\ P) \/ (Q /\ R)
(define (p10 P Q R)
    (or (and P Q P) 
        (and Q R)))
(define (p10s P Q R)
    (and Q (or P R)))

;; (P /\ Q /\ R) \/ (~Q /\ S /\ Q)
(define (p11 P Q R S)
    (or (and P Q R)
        (and (not Q) S Q)))
(define (p11s P Q R S)
  (and (and P Q) R))

;; part p4

(check-expect (p9 #t #t #t) (p9s #t #t #t))
(check-expect (p9 #t #f #t) (p9s #t #f #t))
(check-expect (p9 #f #t #t) (p9s #f #t #t))
(check-expect (p9 #f #f #f) (p9s #f #f #f))

(check-expect (p10 #t #t #t) (p10s #t #t #t))
(check-expect (p10 #t #f #t) (p10s #t #f #t))
(check-expect (p10 #f #t #t) (p10s #f #t #t))
(check-expect (p10 #f #f #f) (p10s #f #f #f))

(check-expect (p11 #t #t #t #t) (p11s #t #t #t #t))
(check-expect (p11 #t #f #t #t) (p11s #t #f #t #t))
(check-expect (p11 #f #t #t #t) (p11s #f #t #t #t))
(check-expect (p11 #f #f #f #f) (p11s #f #f #f #f))

;; Problem 5

;; Signature: Real Real -> Real
(: dist (-> Real Real Real))
(define (dist x y)
  (sqrt (+ (* x x) (* y y))))


(check-contract dist)
(check-expect (dist 3 4) 5) 
(check-expect (dist 0 0) 0) 
(check-expect (dist 1 1) (sqrt 2)) 
(check-expect (dist -3 -4) 5)

;; Problem 6

;; Signature: Integer -> Integer
(: cube-vol (-> Integer Integer))
(define (cube-vol x)
  (* x x x))

(check-contract cube-vol)
(check-expect (cube-vol 1) 1)  
(check-expect (cube-vol 2) 8)   
(check-expect (cube-vol 3) 27) 


;; Problem 7

;; Signature: Boolean Boolean -> Boolean
(: nor (-> Boolean Boolean Boolean))
(define (nor X Y)
  (not (or X Y)))

(check-contract nor)
(check-expect (nor #f #f) #t)
(check-expect (nor #f #t) #f) 
(check-expect (nor #t #f) #f)
(check-expect (nor #t #t) #f)

;; Problem 8

;; Signature: String Natural -> String
(: string-hide (-> String Natural String))
(define (string-hide str pos)
  (if (and (>= pos 1) (<= pos (string-length str)))
      (string-append
       (substring str 0 (- pos 1)) 
       "_"                        
       (substring str pos))       
      str))                        

(check-contract string-hide)
(check-expect (string-hide "hello" 3) "he_lo")    
(check-expect (string-hide "world" 1) "_orld")    
(check-expect (string-hide "racket" 6) "racke_")  
(check-expect (string-hide "test" 5) "test")    
(check-expect (string-hide "" 1) "")             
(check-expect (string-hide "abc" 0) "abc")        

