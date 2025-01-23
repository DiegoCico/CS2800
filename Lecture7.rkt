#lang lsl
;; 2025-01-23

(define-struct person [name age])
(define (sorted-by-age lop) empty)


#|
(check-expect (sort-by-age (list (make-person "A" 19) (make-person "B" 19))) (list (make-person "A" 19) (make-person "B" 19)))
(check-expect (sort-by-age (list (make-person "A" 19) (make-person "B" 19))) (list (make-person "B" 19) (make-person "A" 19)))
|#
;; How do we know which is correct?

;; Struct contracts
(define-contract Person (Struct person [String Natural]))


;; sorted? : [List-of person] -> Boolean
;; Is the given list of person sorted?
(: sorted? : (-> (List Person) Boolean))
(define (sorted? l)
  (cond [(empty? l) ...]
        [(cons? l)
  ;; (first l) should be younger (or the same age as) every person in (rest l)
  (and (andmap (lambda (p) (<= (person-age(first l)) (person-age p))) (rest l))
       (sorted? (rest l)))]))

(: sort-by-age-prop (-> (List Person) Boolean))
(define (sorted-by-age-prop l)
  (let [(sorted? (sorted-by-age l))]
    (and (sorted? sorted-l)
         (same-elements? l sorted-l))))

(define (count-in x l)
  (length (filter (lambda (y) (equal? x y)) l)))
  


(define (same-elements? l1 l2)
  (and (andmap (lambda (p) (count-in p l1) (count-in p l2)) l2)
       (andmap (lambda (p) (count-in p l2) (count-in p l1)) l1)))


(check-contract sort-by-age-prop)
#|(define-contract Person (Immediate (check (lambda (x) (and (person? c)
                                                           (string? (person-name x))
                                                           (natural? (person-age x)))))
                                   (generate (lambda (fuel)) (person (contract-generate String fuel)
                                                                     (contract-generate Natural fuel)))))

|#


(define-contract (Divides n) (Immediate (check (lambda (x) (= (remainder n x) 0)))))

(: gen-6-divider (-> Boolean (Divides 6)))

