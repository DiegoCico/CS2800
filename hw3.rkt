#lang lsl

;; Problem 1

(define (gcd-prop ...) ...)

(define (gcd ...) ...)

;; Problem 2

(define (find-majority-prop ...) ...)

(define (find-majority ...) ...)

;; Problem 3

;; Signature: Natural Natural Natural
(: exclusive-range? (-> Natural Natural Natural Boolean))
(define (exclusive-range? lo hi n)
  (and (> n lo) (< n hi)))


(define (exclusive-range?-prop lo hi)
  (if (>= (add1 lo) hi)
      #t
      (let ([n (+ lo (random (- hi lo)))]) 
        (exclusive-range? lo hi n))))

(check-contract exclusive-range?)
(check-expect (exclusive-range? 1 10 5) #t)  
(check-expect (exclusive-range? 1 10 10) #f) 
(check-expect (exclusive-range? 1 10 1) #f)  
(check-expect (exclusive-range? 5 15 3) #f)  
(check-expect (exclusive-range? 5 15 20) #f) 
(check-expect (exclusive-range?-prop 5 15) #t) 
(check-expect (exclusive-range?-prop 10 10) #t) 
(check-expect (exclusive-range?-prop 10 9) #t) 

;; Problem 4

(define-contract Odd
  (Immediate (check (lambda (x) (and (integer? x) (odd? x))))
             (generate (lambda (fuel) (+ (* 2 (random (add1 fuel))) 1)))
             (shrink (lambda (fuel x)
                       (let ([y (/ x 2)])
                         (if (even? y) y (sub1 y)))))))

(: double-plus1 (-> Integer))
(define (double-plus1)
  (+ (* 2 (contract-generate Odd)) 1))

(check-contract double-plus1)

;; Problem 5

(: divisible-by-3-or-5? (-> Integer Boolean))
(define (divisible-by-3-or-5? n)
  (or (= (remainder n 3) 0)
      (= (remainder n 5) 0)))

(define-contract Divis3Or5
  (lambda (n)
    (and (integer? n)
         (divisible-by-3-or-5? n))))

(define (divide-3-or-5 n)
  (if (divisible-by-3-or-5? n)
      n
      0))

(check-contract divisible-by-3-or-5? 100)
(check-expect (divisible-by-3-or-5? 3) #t)    
(check-expect (divisible-by-3-or-5? 5) #t)    
(check-expect (divisible-by-3-or-5? 15) #t)   
(check-expect (divisible-by-3-or-5? 7) #f)    
(check-expect (divisible-by-3-or-5? -15) #t) 
(check-expect (divide-3-or-5 3) 3)     
(check-expect (divide-3-or-5 5) 5)    
(check-expect (divide-3-or-5 15) 15)   
(check-expect (divide-3-or-5 7) 0)   
(check-expect (divide-3-or-5 -15) -15) 
(check-expect (divide-3-or-5 -7) 0)     

