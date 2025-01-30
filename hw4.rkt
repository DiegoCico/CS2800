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

(: common-element-prop (-> (List (List Integer)) True))
(define (common-element-prop lss)
  (if (equal? (common-element lss) -1) #t
      (andmap (lambda (x) (member? (common-element lss) x)) lss)))

(: common-element-two-lists (-> (List Integer) (List Integer) (List Integer)))
(define (common-elements-two-lists ls1 ls2)
  (filter (lambda (x) (member? x ls2)) ls1))


(: common-element (-> (List (List Integer)) Integer))
(define (common-element lss)
  (if (or (empty? lss) (empty? (first lss))) -1
      (if (= (length lss) 1)
          (first (first lss))
          (common-element(cons (common-elements-two-lists (first lss) (second lss)) (rest (rest lss)))))))


;; Problem 3

(: pair-with-sum-prop (-> (List Integer) Integer True))
(define (pair-with-sum-prop ls num)
  (let ([result (pair-with-sum ls num)])
    (cond
      [(empty? result) #t]
      [(= (length result) 2) (and (equal? (+ (first result) (second result)) num) (and (member? (first result) ls) (member? (second result) ls)))]  
      [else #f])))

(: pair-with-sum (-> (List Integer) Integer (List Integer)))
(define (pair-with-sum ls num)
  (if (or (empty? ls) (equal? (length ls) 1))
      '() 
      (let ((first-element (first ls)))
        (let ((other-element (- num first-element)))
          (if (member? other-element (rest ls))
              (list first-element other-element) 
              (pair-with-sum (rest ls) num))))))



(check-contract pair-with-sum 100)
(check-contract pair-with-sum-prop 100)
(check-expect (pair-with-sum '(1 0) 1) '(1 0))
(check-expect (pair-with-sum '(2 7 11 15) 9) '(2 7))
(check-expect (pair-with-sum '(1 2 3 4 5) 10) '())
(check-expect (pair-with-sum '(3 3) 6) '(3 3))
(check-expect (pair-with-sum '(1) 1) '())
(check-expect (pair-with-sum '() 5) '())
(check-expect (pair-with-sum '(4 6 8 2) 14) '(6 8))
(check-expect (pair-with-sum '(5 10 15 20) 25) '(5 20))
(check-expect (pair-with-sum '(7 1 5 9) 10) '(1 9))
(check-expect (pair-with-sum '(1 2 3 4 5 6 7 8 9 10) 17) '(7 10))
(check-expect (pair-with-sum '(100 200 300) 500) '(200 300))
(check-expect (pair-with-sum '(3 2 1 0 -1 -2 -3) -5) '(-2 -3))
(check-expect (pair-with-sum '(1 1 1 1 1 2 2 2) 3) '(1 2))

(check-expect (pair-with-sum-prop '(1 0) 1) #t)
(check-expect (pair-with-sum-prop '(2 7 11 15) 9) #t)
(check-expect (pair-with-sum-prop '(1 2 3 4 5) 10) #t)
(check-expect (pair-with-sum-prop '(3 3) 6) #t)
(check-expect (pair-with-sum-prop '(1) 1) #t)
(check-expect (pair-with-sum-prop '() 5) #t)
