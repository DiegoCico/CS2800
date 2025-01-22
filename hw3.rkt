#lang lsl

;; Problem 1

(define (divisors n)
  (filter (lambda (d) (zero? (remainder n d)))
          (range 1 (+ n 1))))

(define (common-divisors n m)
  (filter (lambda (d) (and (zero? (remainder n d))
                           (zero? (remainder m d))))
          (range 1 (+ (min n m) 1))))

(define (gcd-ref n m)
  (if (and (zero? n) (zero? m))
      0
      (apply max (if (or (zero? n) (zero? m))
                     (if (zero? n) (divisors m) (divisors n))
                     (common-divisors n m)))))
(define (gcd n m)
  (cond
    [(and (zero? n) (zero? m)) 0]
    [(zero? m) n]
    [else (gcd m (remainder n m))]))

(define (all-divisors-less? n m gcd-val)
  (cond
    [(> 1 gcd-val) #t]
    [(and (zero? (remainder n 1)) (zero? (remainder m 1))) #f]
    [else (all-divisors-less? n m gcd-val)]))

(define (all-divisors-less?-helper n m gcd-val current-test)
  (cond
    [(>= current-test gcd-val) #t]
    [(and (zero? (remainder n current-test)) (zero? (remainder m current-test))) #f]
    [else (all-divisors-less?-helper n m gcd-val (+ current-test 1))]))

(define (gcd-prop n m)
  (if (and (zero? n) (zero? m))
      (equal? (gcd n m) (gcd-ref n m))
      (and (equal? (gcd n m) (gcd-ref n m))
           (>= (gcd n m) 1) 
           (zero? (remainder n (gcd n m))) 
           (zero? (remainder m (gcd n m))) 
           (all-divisors-less?-helper n m (gcd n m) 1))))

(check-expect (common-divisors 12 18) '(1 2 3 6))
(check-expect (gcd-ref 12 18) 6)
(check-expect (gcd 12 18) 6)
(check-expect (gcd 0 0) 0)
(check-expect (gcd 0 5) 5)

;; Problem 2

; Signature: List Natural -> Natural -1
; Purpose: Finds the majority element in a list of natural numbers.
;          Returns the majority element if it exists, otherwise returns -1.
(define (find-majority nums)
  (cond
    [(empty? nums) -1]
    [else
     ((lambda (remaining candidate count)
        (cond
          [(empty? remaining)
           (if (> (count-occurrences candidate nums) (/ (length nums) 2))
               candidate
               -1)]
          [(equal? (first remaining) candidate)
           ((lambda (r c n) (r c n))
            (rest remaining) candidate (+ count 1))]
          [else
           ((lambda (r c n)
              (if (zero? count)
                  (r (rest remaining) (first remaining) 1)
                  (r (rest remaining) candidate (- count 1))))
            (lambda (r c n) (r c n)))]))
      nums (first nums) 1)]))

(define (count-occurrences elem lst)
  (length (filter (lambda (x) (= x elem)) lst)))

; Signature: Listof Natural -> Boolean
; Purpose: Checks if the output of find-majority is correct when it's not -1.
(define (find-majority-prop nums)
  (let ([majority (find-majority nums)])
    (cond
      [(equal? majority -1) #t] 
      [else
       (and (>= majority 0)
            (> (count-occurrences majority nums) (/ (length nums) 2)))]))) 

;; Problem 3

;; Signature: Natural Natural Natural -> Boolean
(: exclusive-range? (-> Natural Natural Natural Boolean))
(define (exclusive-range? lo hi n)
  (and (> n lo) (< n hi)))

;; Signature: Natural Natural Boolean
(: exclusive-range?-prop (-> Natural Natural Boolean))
(define (exclusive-range?-prop lo hi)
  (cond
    [(>= lo hi) #t] 
    [else
     (let ([range-size (- hi lo 1)]) 
       (if (zero? range-size)
           #t 
           (let ([n (+ lo 1 (random range-size))])
             (exclusive-range? lo hi n))))]))

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

(: double-plus1 (-> Odd Odd))
(define (double-plus1 x)
  (+ (* 2 x) 1))

(check-contract double-plus1 100)

;; Problem 5

(: divisible-by-3-or-5? (-> Integer Boolean))
(define (divisible-by-3-or-5? n)
  (or (= (remainder n 3) 0)
      (= (remainder n 5) 0)))

(define-contract Divis3Or5
  (lambda (n)
    (and (integer? n)
         (divisible-by-3-or-5? n))))

(: divide-3-or-5 (-> Integer Integer))
(define (divide-3-or-5 n)
  (if (divisible-by-3-or-5? n)
      n
      0))

(check-contract divisible-by-3-or-5? 100)
(check-contract divide-3-or-5 100)
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

