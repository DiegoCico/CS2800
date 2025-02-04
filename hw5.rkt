#lang lsl

;; Problem 0

;; part p0a
(: left-pad (-> Natural String String))
(define (left-pad n s)
  (string-append (implode (build-list n (lambda (_) " "))) s))

;; part p0b
(: buggy-left-pad (-> Natural String String))
(define (buggy-left-pad n s)
  (string-append s (implode (build-list n (lambda (_) " ")))))

;; part p0c
(: left-pad-spec (-> Natural String Boolean))
(define (left-pad-spec n s)
  (= (string-length (buggy-left-pad n s))
     (+ n (string-length s))))

;; part p0d
(define BUGGY-INPUT-N 3)
(define BUGGY-INPUT-S "hello")

(check-expect (left-pad BUGGY-INPUT-N BUGGY-INPUT-S) "   hello")
(check-expect (buggy-left-pad BUGGY-INPUT-N BUGGY-INPUT-S) "hello   ")
(check-expect (left-pad-spec BUGGY-INPUT-N BUGGY-INPUT-S) #t)
(check-expect (string=? (left-pad BUGGY-INPUT-N BUGGY-INPUT-S)
                        (buggy-left-pad BUGGY-INPUT-N BUGGY-INPUT-S))
             #f)


;; Part p1a

;; A Bit is either 0 or 1.
(define-contract Bit (OneOf (Constant 0) (Constant 1)))

;; A Key is a tuple of 6 Bits.
(define-contract Key (Tuple Bit Bit Bit Bit Bit Bit))

;; A Message is also a tuple of 6 Bits.
(define-contract Message (Tuple Bit Bit Bit Bit Bit Bit))


;; Part p1b

;; The bit-wise XOR function.
(: xor (-> Bit Bit Bit))
(define (xor b1 b2)
  (modulo (+ b1 b2) 2))

(check-expect (xor 0 0) 0)
(check-expect (xor 0 1) 1)
(check-expect (xor 1 0) 1)
(check-expect (xor 1 1) 0)

;; XOR two lists of bits, element by element.
(: xor-list (-> (List Bit) (List Bit) (List Bit)))
(define (xor-list l1 l2)
  (map xor l1 l2))

(check-expect (xor-list (list 1 0 0) (list 1 1 1)) (list 0 1 1))
(check-expect (xor-list (list 0 0 0) (list 0 0 0)) (list 0 0 0))

;; Encryption: XOR the message with the key.
(: encrypt (-> Message Key Message))
(define encrypt xor-list)

;; Decryption: XOR the encrypted message with the key (works because XOR is its own inverse).
(: decrypt (-> Message Key Message))
(define decrypt xor-list)


;; Part p1c
(: xor-perfect-prop (-> Message Message True))
(define (xor-perfect-prop encr-msg arbitrary-msg)
  (let ([computed-key (xor-list encr-msg arbitrary-msg)])
    (equal? (decrypt encr-msg computed-key) arbitrary-msg)))

(check-contract xor-perfect-prop)