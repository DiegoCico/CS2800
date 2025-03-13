#lang lsl

(define-mutable-struct cell [value])
(define-contract (Cell X) (Struct cell [X]))
(define-contract (NotEq v)
  (lambda (other) (not eq? v other)))

(: f (Function (arguments [a (Cell Natural)]
                          [b (AllOf (Cell Natural) (NotEq a))])
               (result False)))
(define (f x y)
  (if (<= (cell-value x) (cell-value y))
      (begin (set-cell-value! x (add1 (cell-value x)))
             (f x y))
      #f))

(define a (make-cell 10))
(define b (make-cell 11))
;;a b
;;(f a b)
;;a b

;; Temporal Specifications
;; Specifying behavior over several invocations of a function

(: ids (List Natural))
(define ids empty)
(: generate-unique2 (-> (AllOf Natural (Record ids))))
(define (generate-unique2)
  (random 1000))