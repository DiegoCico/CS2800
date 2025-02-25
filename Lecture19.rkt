#lang lsl


;; Given a set S with n elemtns, show that the number of subsets is 2^n

;; Proof by induction on n

;; Base Case n = 0

;; Inductive case: assume above holds for n, show that it holds for n + 1
;; S = { }
;; P(S) = { {} }
;; n^0 = 1

;; Inductive case: assume above holds for n, show that it holds for n + 1
;; Assume that |S| = n, 2^n, |P(S)| = 2^n
;; Since we are adding a new element to S to get S', take the P(S) and add
;; the new element to each set from P(S)

;; How many elements does P(S') have?
;; |P(S')| = 2 * |P(S)|
;;         = 2 * 2^n = 2^(n+1)

;; In each case, we are constructing a new powerset
;; Base case: easy just create powerset of size 1
;; Inducive case:
;;    input: oiwerset of a set of size n
;;    outpu: powerset of a set of size n + n

;; sets = lists
;; powerset = list of list
;; give a function subsets-proof, such that length of (subsets-proof n) is
;; 2^n
;; (: subsets-proof (-> Natural (List (List Any))))
;; For all n, given a set S of size N, the size of the powerset P(S) is 2^n 
(: subsets-proof
   (Function (arguments (n Natural))
             (result (AllOd (List (List Natural))
              (lambda (l) (= (length l) (expt 2 n))))))
(define (subsets-proof n)
  (cond [(zero? n) (list (list))] ;; base 
        [(else
          (let [(n-minus-1 (subsets-proof (-n 1)))])
           (append n-minus-1 (map (lambda (x) (cons ... )) ;; add an element to every subset for n-1
                n-minus-1)))]))  ;; recursive

;; recrusive case - induction step

;; fun : Natural -> (List String)

;; Curry-howard correspondence
;; Logic <-> programs
;; Types / Signatures / contracts <-> Propositions
;; Proof <-> programs


;; say you only have $2 and $5 bills/ show that you can make nay dollar amount >= $4

;; P(N) = there existis an t and f, such that n = 2t + 5f
;; FOr all n?=, there exists an t and f, such that n = 2t + 5f

;; Proof by induction on n.

;; Base case: n = 4
;; t = 2, f = 0

;; Inductive step: Assume that P (n-1) holds. Show  that P(N) holds too

;; Fom assumption we have there is a t and f, such that
;; n - 1 = 2t + 5f
;; We want to show that there is a t' and f', such thqat, n = 2t' + 5f'

;; two casses: f = 0 or f != 0
;; f = 0
;; Then t >= 2 .....




