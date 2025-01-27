#lang lsl


(define-contract (SameLength l1) (lambda (l2)  (= (length l1) (length l2))))
;; my-rev : (X) [List X] -> [List X]
(: my-rev (Function (arguments [xs (List Any)])
                               (result (AllOf (List Any) (SameLength xs)))))
(define (my-rev xs)
  (reverse xs))

(my-rev (append (list 1 2 3) (list 4 5 6)))
=
(append (list 6 5 4) list( 3 2 1))

(: my-rev-append-prop (-> (List Any) (List Any) True))
(define (my-rev-append-prop xs ys)
  (equal? (my-rev (append xs ys))
          (append (my-rev yx) (my-rev xs))))

;; Data Error Recovery - Itemizations and Constant contracts

;; A bit is 0 1
;; a Packet is a bit
;; A Message a list of packet

(define-contract Bit (Imediate (check (lambda (b) (or (= b 0) (= b 1))
                                       (generate (lambda (fuel) ))))))


;; idea : introduce redunancy - triplicate each bit - take majority to decode
(define-contract 3Packet (Tuple Bit Bit Bit))