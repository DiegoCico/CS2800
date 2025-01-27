 #lang lsl

;; Problem 1

;;(: duplicate-element-prop (-> Integer Integer))
(define (duplicate-element-prop lst result)
  (cond
    [(= result -1) #t]
    [else (>= (length (filter (lambda (x) (= x result)) lst)) 2)]))

(define (sorted-lst lst)
  (sort lst <)) 

;; (: duplicate-element (->  Integer Integer))
(define (duplicate-element lst)
  (let ([sorted (sorted-lst lst)])
    (cond
      [(empty? sorted) -1] 
      [(empty? (rest sorted)) -1]
      [(= (first sorted) (second sorted)) (first sorted)] 
      [else (duplicate-element (rest sorted))]))) 

;; Problem 2

(define (common-element-prop ...) ...)

(define (common-element ...) ...)

;; Problem 3

;; (: pair-with-sum-prop (-> Integer Integer Boolean))
(define (pair-with-sum-prop result target)
  (cond
    [(empty? result) #t]
    [(and (= (length result) 2)
          (= (+ (first result) (second result)) target)) #t]
    [else #f]))

(define (pair-with-sum lst target)
  (cond
    ((null? lst) '())
    (else 
      (append (map (lambda (y) 
                     (if (= (+ (first lst) y) target)
                         (list (list (first lst)) (list y))
                         '()))
                   (rest lst))
              (pair-with-sum (rest lst) target)))))