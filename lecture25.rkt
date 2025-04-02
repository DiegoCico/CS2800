#lang lsl


 #|

  So far: PBT
  - PBT generates a finite set of examples
  - but for certainty, we need to reason about infinite examples, sets of data
  - 

 |#

(define-contract (List A) (OneOf Empty
                                 (Struct cons [A (List A)])))

(define (length xs)
  (cond [(empty? xs) 0]
        [(cons? xs) (+ 1 (length (ret xs)))]))


#|

What s a proposition? 
- a statement that is either true or false
- is data

What is truth?
- is a value

What is proof?
- induction
- sequence of steps in which we state that a proposition has a particular truth-value
- is data

- theorem provers - assistns in proof discory and verification

- Truth
 - how do we know that "A and B" is true?
     - if A is true, AND
     - if B is true
 - how do we know that "A or B" is true?
     - if A is true, OR
     - if B is true
 - how do we know that "A -> B" is true?
     - if A is false, OR
     - if B is true

- Proof
   - How do we find a proof of "A and B"?
       - if we find a proof of A, AND
       - if we find a proof of B
   - How do we find a proof of "A or B"?
       - if we find a proof of A, OR
       - if we find a proof of B
   - How do we find a proof of "A -> B"?
       - given a proof of A

- Types
  - how can we show that (for any x y), (list x y) has the type (Tuple A B)
      - if x has the type A AND
      - if y hasthe type B
  - how can we show that e has the type (or A B)
      - if e is an x of type A, OR
      - if e is an y of type B
  - how can we shot that f has the type (-> A B)
      - if we can show that given an argument of type A, f returns B
|#

(define-contract (And A B) (Tuple A B))
(define-struct or-left [proof])
(define-struct or-right [proof])

(define-contract (Or A B) (OneOf (Struct or-left [A])
                                 (struct or-right [B])))

(: proof1 (All (A B) (-> (And A B) (And B A))))
(define (proof1 p)
  (list (second p) (first p)))