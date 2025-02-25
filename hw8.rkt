#lang lsl

;; Problem 1

;; part p1
(define-struct tv [T F AND OR NOT])

(define-contract (TruthVal A)
  (Struct tv [A
              A
              (-> A A A)
              (-> A A A)
              (-> A A)]))
;; part p1


;; part p1a
(: BOOL-TV (TruthVal Boolean))
(define BOOL-TV (make-tv #t
                         #f
                         (lambda (x y) (and x y))
                         (lambda (x y) (or x y))
                         (lambda (x) (not x))))

(test-suite "BOOL-TV"
(check-expect (tv-T BOOL-TV) #t)
(check-expect (tv-F BOOL-TV) #f)
(check-expect ((tv-AND BOOL-TV) #t #t) #t)
(check-expect ((tv-AND BOOL-TV) #t #f) #f)
(check-expect ((tv-AND BOOL-TV) #f #f) #f)
(check-expect ((tv-OR BOOL-TV) #t #t) #t)
(check-expect ((tv-OR BOOL-TV) #t #f) #t)
(check-expect ((tv-OR BOOL-TV) #f #f) #f)
(check-expect ((tv-NOT BOOL-TV) #t) #f)
(check-expect ((tv-NOT BOOL-TV) #f) #t))


;; Problem 2:


;; part p2
(define-contract Variable Natural)

(define-struct n (var))
(define-contract (N X) (Struct n [X]))
;; a negated variable

(define-contract VariableClause (OneOf Variable (N Variable)))
;; a Variable is either a natural number or a negated one

(define-contract (Formula V) (List V)) 
(define-contract (CNF V) (List (Formula V)))

;; part p2b

(define (all-n cnf)
  (cond [(empty? cnf) '()]
        [else (append (first cnf) (all-n (rest cnf)))]))

(define (n->var lit)
  (if (number? lit)
      lit
      (n-var lit)))

(: variable-upper-bound (-> (CNF VariableClause) Variable))
(define (variable-upper-bound cnf)
  (if (empty? cnf)
      0
      (let ([result (map n->var (all-n cnf))])
        ;; check again if it is a list of list and it is just
        ;; empty
        (if (empty? result)
            0
            (apply max result)))))

(test-suite "variable-upper-bound"
(check-expect (variable-upper-bound (list (list (make-n 4)))) 4)
(check-expect (variable-upper-bound (list (list))) 0)
(check-expect (variable-upper-bound '()) 0)
(check-expect (variable-upper-bound (list (list 3))) 3)
(check-expect (variable-upper-bound (list (list (make-n 4)))) 4)
(check-expect (variable-upper-bound (list (list 2 (make-n 3))
                                          (list (make-n 5) 4)
                                          (list 1))) 5)
(check-expect (variable-upper-bound (list (list 2 3)
                                          (list (make-n 3) 2))) 3)
)

;; Problem 3:

;; part p3


(define (eval-c api c)
  (if (empty? c)
      (tv-F api)                      
      ((tv-OR api) (first c)
                 (eval-c api (rest c)))))

(: eval (All (A) (-> (TruthVal A) (CNF A) A)))
(define (eval api cnf)
  (if (empty? cnf)
      (tv-T api)                      
      ((tv-AND api) (eval-c api (first cnf))
                  (eval api (rest cnf)))))

(test-suite "eval"
  (check-expect (eval BOOL-TV '()) #t)
  (check-expect (eval BOOL-TV (list (list #t #f))) #t)
  (check-expect (eval BOOL-TV (list (list #t #f) (list #t))) #t)
  (check-expect (eval BOOL-TV (list (list #t #f) (list #f))) #f)
  (check-expect (eval BOOL-TV (list (list #t) '())) #f))


;; part p3
