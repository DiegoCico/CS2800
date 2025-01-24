#lang lsl

;; Problem 1

(define (common-divisors n m)
  (filter (lambda (d) (and (zero? (remainder n d))
                           (zero? (remainder m d))))
          (range 1 (+ (min n m) 1))))

(: gcd-ref (-> Natural Natural Natural))
(define (gcd-ref n m)
  (cond
    [(and (= n 0) (= m 0)) 0]              
    [(or (= n 0) (= m 0)) (max n m)]       
    [else (apply max (common-divisors n m))]))

(: gcd-prop (-> Natural Natural Boolean))
(define (gcd-prop n m)
  (= (gcd n m) (gcd-ref n m)))

(: gcd (-> Natural Natural Natural))
(define (gcd n m)
  (cond
    [(and (zero? n) (zero? m)) 0]
    [(zero? m) n]
    [else (gcd m (remainder n m))]))

;; Problem 2

(define (find-candidate nums candidate count)
  (cond
    [(null? nums) candidate]
    [(zero? count) (find-candidate (rest nums) (first nums) 1)]
    [(= (first nums) candidate)
     (find-candidate (rest nums) candidate (+ count 1))]
    [else
     (find-candidate (rest nums) candidate (- count 1))]))

(define (verify-majority nums candidate)
  (let ([count (length (filter (lambda (x) (= x candidate)) nums))])
    (if (> count (/ (length nums) 2))
        candidate
        -1)))

(define (find-majority nums)
  (verify-majority nums (find-candidate nums #f 0)))

(define (find-majority-prop nums)
  (let ([majority (find-majority nums)])
    (cond
      [(equal? majority -1) #t] 
      [else
       (and (>= majority 0)
            (> (count-occurrences majority nums) (/ (length nums) 2)))])))

(define (count-occurrences elem lst)
  (length (filter (lambda (x) (= x elem)) lst)))

;; Problem 3

(: exclusive-range? (-> Integer Integer Integer Boolean))
(define (exclusive-range? lo hi n)
  (and (> n lo) (< n hi)))

(: exclusive-range?-prop (-> Integer Integer True))
(define (exclusive-range?-prop lo hi)
  (cond
    [(>= lo hi) #t] 
    [else
     (let ([range-size (- hi lo 1)]) 
       (if (< range-size 1)
           #t 
           (let ([n (+ lo 1 (random range-size))])
             (and (exclusive-range? lo hi n)
                  (> n lo)
                  (< n hi)))))]))



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

