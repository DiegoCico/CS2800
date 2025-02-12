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
    ;; checks if there are atleast three elements
    [(or (empty? p) (empty? (rest p)) (empty? (rest (rest p))))
     ;; if not it returns p
     p]
    [else
     ;; if there are atleast three elements their values are set to first,second,third respectively
     (let ([first (first p)]
           [second (second p)]
           [third (third p)])
       ;; if the first second and third are push push add
       (if (and (push? first)
                (push? second)
                (add? third))
           ;; it pushes the first two numbers and then performs an operation
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


;; Problem 4

;; part p4a
(define-struct var [name])
(define-contract (Var X) (Struct var [X]))
(define-contract Instr (OneOf (Push Integer) Add Mul Sub (Var String)))

(define-struct bind [name value])
(define-contract (Bind X Y) (Struct bind [X Y]))
(define-contract Binding (Bind String Integer))


(: eval (-> (List Binding) (List Integer) (List Instr) (List Integer)))
; will return an empty list if it reaches an unbound variable, or a malformed
; program (trying to do an operation without enough values on stack).
(define (eval env stk instrs)
  (local [(: stack-binop (-> [-> Integer Integer Integer] [List Integer]
                             [List Instr]
                             [List Integer]))
          ; evaluates a binary operator on top two numbers of stack, if present
          (define (stack-binop op stk instrs)
            (if (>= (length stk) 2)
                (eval env
                      (cons (op (first stk) (second stk))
                            (rest (rest stk)))
                      instrs)
                (list)))

          (: lookup-var (-> String [List Binding] [List Integer]
                            [List Instr] [List Integer]))
          (define (lookup-var name env stk instrs)
            (cond [(empty? env) (list)]
                  [(cons? env) (if (equal? name (bind-name (first env)))
                                   (eval env
                                         (cons (bind-value (first env))
                                               stk)
                                         instrs)
                                   (lookup-var name (rest env) stk instrs))]))

          (: eval-instr (-> Instr [List Integer] [List Instr] [List Integer]))
          ; evaluates a single instruction, given a stack and rest of instructions
          (define (eval-instr i stk instrs)
            (cond [(add? i) (stack-binop + stk instrs)]
                  [(mul? i) (stack-binop * stk instrs)]
                  [(sub? i) (stack-binop - stk instrs)]
                  [(push? i) (eval env (cons (push-num i) stk) instrs)]
                  [(var? i) (lookup-var (var-name i) env stk instrs)]))]
    (cond [(empty? instrs) stk]
          [(cons? instrs) (eval-instr (first instrs) stk (rest instrs))])))
;; part p4a

;; Your first task is to first define an updated version of `simple-stack-verify`.
;; This time it will take a substitution (set of variable bindings) that it
;; can pass to `eval`.

;; part p4b
 (: stack-verify (-> (List Binding) (List Instr) (List Instr) Boolean)) 
 (define (stack-verify env p1 p2)
   (equal? (eval env (list) p1) (eval env (list) p2))) 

;; part p4b

;; part p4c
(: const-fold (-> (List Instr) (List Instr)))
(define (const-fold p)
  (cond
    ;; checks if there are atleast three elements (otherwise cannot check)
    [(or (empty? p) (empty? (rest p)) (empty? (rest (rest p))))
     ;; if not it returns p
     p]
    [else
     ;; if there are atleast three elements their values are set to first,second,third respectively
     (let ([first (first p)]
           [second (second p)]
           [third (third p)])
       ;; if the first second and third are push push add
       (if (and (push? first)
                (push? second)
                ;; need to change this to be any operation
                (or (add? third) (sub? third) (mul? third)))
           ;; it pushes the first two numbers and then performs an operation
           ;; need to remove + sign and generalize to each equation
           (cond [(add? third) (cons (make-push (+ (push-num second) (push-num first)))
                 (const-fold (drop p 3)))]
                 [(mul? third) (cons (make-push (* (push-num second) (push-num first)))
                 (const-fold (drop p 3)))]
                 [(sub? third) (cons (make-push (- (push-num second) (push-num first)))
                 (const-fold (drop p 3)))])
           (cons first (const-fold (rest p)))))]))
;; part p4c

;; part p4d

;; this should check the
(: const-fold-prop (-> (List Instr) Boolean))
(define (const-fold-prop p)
  (equal? (eval '() '() p)
          (eval '() '() (const-fold p))))

(check-contract const-fold-prop)


;; Problem 5

;; part p5
(define-struct leaf [value])
(define-struct node [left right])
(define-contract (Leaf X) (Struct leaf [X]))
(define-contract (Node X Y) (Struct node [X Y]))
(define-contract (Tree X) (OneOf (Leaf X) (Node (Tree X) (Tree X))))


(: tree-map (All (X Y) (-> (-> X Y) (Tree X) (Tree Y))))
;; (: tree-map (-> (Tree Integer) (Tree Integer) (Tree Integer)))

(define (tree-map f t)
  ;; if the leaf is t then another leaf is made of type f and t
  ;; if the node is type t then a node is made of 
  (cond [(leaf? t) (make-leaf (f (leaf-value t)))]
        [(node? t) (make-node (tree-map f (node-left t))
                              (tree-map f (node-right t)))]))

(check-contract tree-map)
;; part p5


;; Problem 6

;; part p6

(: p6a (All (X) (-> Boolean X X X)))
(define (p6a b x1 x2)
  (if b x1 x2))

(: p6b (All (X) (-> Boolean X X X)))
(define (p6b b x1 x2)
  (if b x2 x1))

(: p6c (All (X) (-> Boolean X X X)))
(define (p6c b x1 x2)
  x2)

(: p6d (All (X) (-> Boolean X X X)))
(define (p6d b x1 x2)
  x1)

;; part p6