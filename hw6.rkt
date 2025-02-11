#lang lsl

;; Problem 1

;; Part p1a
(define-struct push [num])
(define-struct add [])
(define-struct mul [])
(define-struct sub [])
(define-contract (Push X) (Struct push [X]))
(define-contract Add (Struct add []))
(define-contract Mul (Struct mul []))
(define-contract Sub (Struct sub []))
(define-contract SimpleInstr (OneOf (Push Integer) Add Mul Sub))

(: simple-eval (-> (List Integer) (List SimpleInstr) (List Integer)))
(define (simple-eval stk instrs)
  (local [
          (: stack-binop (-> [-> Integer Integer Integer] [List Integer] [List SimpleInstr] [List Integer]))
          (define (stack-binop op stk instrs)
            (if (>= (length stk) 2)
                (simple-eval (cons (op (first stk) (second stk))
                                   (rest (rest stk)))
                             instrs)
                (list)))  
          
          (: eval-instr (-> SimpleInstr [List Integer] [List SimpleInstr] [List Integer]))
          (define (eval-instr i stk instrs)
            (cond [(add? i) (stack-binop + stk instrs)]
                  [(mul? i) (stack-binop * stk instrs)]
                  [(sub? i) (stack-binop - stk instrs)]
                  [(push? i) (simple-eval (cons (push-num i) stk) instrs)]))]
    (cond [(empty? instrs) stk]         
          [(cons? instrs) (eval-instr (first instrs) stk (rest instrs))])))

;; Part p1

(: simple-stack-verify (-> (List SimpleInstr) (List SimpleInstr) Boolean))
(define (simple-stack-verify p1 p2)
  (equal? (simple-eval (list) p1)
          (simple-eval (list) p2)))

;; Testing

(define prog1
  (list (make-push 10)
        (make-push 20)
        (make-push 2)
        (make-mul)
        (make-add)))

(define prog2
  (list (make-push 10)
        (make-push 40)
        (make-add)))

(check-expect (simple-eval (list) prog1) (list 50))
(check-expect (simple-eval (list) prog2) (list 50))
(check-expect (simple-stack-verify prog1 prog2) #true)
(check-expect (simple-eval (list) 
                           (list (make-push 1)
                                 (make-push 2)
                                 (make-add)))
              (list 3))

(check-expect (simple-stack-verify (list (make-push 1)
                                          (make-push 2)
                                          (make-add))
                                  (list (make-push 1)
                                        (make-push 2)))
              #false)

;; Problem 2
(define (drop lst n)
  (if (zero? n)
      lst
      (drop (rest lst) (sub1 n))))

(: simple-const-fold (-> (List SimpleInstr) (List SimpleInstr)))
(define (simple-const-fold p)
  (cond
    [(or (empty? p) (empty? (rest p)) (empty? (rest (rest p))))
     p]
    [else
     (let ([first (first p)]
           [second (second p)]
           [third (third p)])
       (if (and (push? first)
                (push? second)
                (add? third))
           (cons (make-push (+ (push-num second) (push-num first)))
                 (simple-const-fold (drop p 3)))
           (cons first (simple-const-fold (rest p)))))]))


(check-expect (simple-const-fold (list (make-push 3)
                                        (make-push 4)
                                        (make-add)))
              (list (make-push 7)))
(check-expect (simple-const-fold (list (make-push 5)
                                        (make-add)
                                        (make-push 3)))
              (list (make-push 5)
                    (make-add)
                    (make-push 3)))
(check-expect (simple-const-fold (list (make-push 3)
                                        (make-push 4)
                                        (make-mul)))
              (list (make-push 3)
                    (make-push 4)
                    (make-mul)))
(check-expect (simple-const-fold (list (make-push 3)
                                        (make-push 4)
                                        (make-add)
                                        (make-push 5)
                                        (make-push 6)
                                        (make-add)))
              (list (make-push 7)
                    (make-push 11)))
(check-expect (simple-const-fold (list (make-push 1)
                                        (make-push 2)
                                        (make-add)
                                        (make-push 3)
                                        (make-add)))
              (list (make-push 3)
                    (make-push 3)
                    (make-add)))
(check-expect (simple-const-fold (list (make-push 3)
                                        (make-push 4)))
              (list (make-push 3)
                    (make-push 4)))

;; Problem 3
(: simple-const-fold-prop (-> (List SimpleInstr) True))
(define (simple-const-fold-prop p)
  (simple-stack-verify p (simple-const-fold p)))

(check-contract simple-const-fold-prop)