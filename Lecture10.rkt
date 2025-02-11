#lang lsl

(: id (All (x) (-> X X)))
(define (id x)
  (let ([y (+ 1 2)]) x))

(check-contract id)

(define (length xs)
  (cond [(empty? xs) 0]
        [(cons? xs) (add1 (length (rest xs)))]))

(: my-filter (-> (-> Any Boolean) (List Any) (List Any)))
(define (my-filter pred? xs)
  (cond [(empty? xs) empty]
        [(cons? xs) (if (pred? (first xs)
                               (cons (first xs)
                                     (my-filter pred? (rest  xs)))))]))