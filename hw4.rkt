 #lang lsl

;; Problem 1

(: duplicate-element-prop (-> (List Natural) True))
(define (duplicate-element-prop lst)
  (let ([result (duplicate-element lst)])
  (cond
    [(= result -1) #t]
    [else (>= (length (filter (lambda (x) (= x result)) lst)) 2)])))

(define (sorted-lst lst)
  (sort lst <)) 

(: duplicate-element (->  (List Natural) Integer))
(define (duplicate-element lst)
  (let ([sorted (sorted-lst lst)])
    (cond
      [(empty? sorted) -1] 
      [(empty? (rest sorted)) -1]
      [(= (first sorted) (second sorted)) (first sorted)] 
      [else (duplicate-element (rest sorted))])))

(check-expect (duplicate-element '()) -1)                ; Empty list
(check-expect (duplicate-element '(1)) -1)              ; Single element
(check-expect (duplicate-element '(1 2 3 4 5)) -1)      ; No duplicates
(check-expect (duplicate-element '(1 2 2 3)) 2)         ; One duplicate (2)
(check-expect (duplicate-element '(3 3 1 2)) 3)         ; One duplicate (3)
(check-expect (duplicate-element '(3 1 2 3 4)) 3)       ; One duplicate (3)
(check-expect (duplicate-element '(4 5 6 7 8 4)) 4)     ; Duplicate at ends
(check-expect (duplicate-element '(5 6 6 7 8 9)) 6)     ; Duplicate in middle
(check-expect (duplicate-element '(1 1 2 2 3 3)) 1)     ; Multiple duplicates, returns first

(check-expect (duplicate-element-prop '(1 2 3 4 5)) #t) ; No duplicates, valid
(check-expect (duplicate-element-prop '(1 2 2 3)) #t)    ; 2 appears twice
(check-expect (duplicate-element-prop '(3 3 1 2)) #t)    ; 3 appears twice
(check-expect (duplicate-element-prop '(1 2 3 4)) #t)    ; Invalid result (3 not duplicated)
(check-expect (duplicate-element-prop '(4 4 5 6)) #t)    ; 4 appears twice

;; Problem 2

(define (common-element-prop ...) ...)

(define (common-element ...) ...)

;; Problem 3

(: pair-witg-sum-prop (-> (List Integer) Integer True))
(define (pair-with-sum-prop ls num)
  (let ([result (pair-with-sum ls num)])
    (cond
      [(empty? result) #t]  
      [else (= (+ (first result) (second result)) num)]))) 

(: pair-with-sum (-> (List Integer) Integer (List Integer)))
(define (pair-with-sum ls num)
  (if (or (empty? ls) (equal? (length ls) 1))
      '() 
      (let ((first-element (first ls)))
        (let ((other-element (- num first-element)))
          (if (member other-element (rest ls))
              (list first-element other-element) 
              (pair-with-sum (rest ls) num))))))
