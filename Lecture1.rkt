#lang lsl

;; Propositional Logic -> LSL

;; T, F, ^, V , => (implication), ~, three equals mark

;; T = #t
;; F = #f
;; ^ = and
;; V = or
;; ~ = not
;; => = ??
;; three equals mark = = boolean=?

;; P ^ Q = (and P Q)
;; P V Q = (or P Q)
;; ~p = (not P)
;; p three equls = (boolean=? P Q)

;; P V (Q ^ R)
(define (p1 P Q R)
  (or P (and Q R)))

;; (P V Q) ^ (P V ~Q)
(define (p2 P Q)
  (and (or P Q) (or P (not Q))))

;; (P 3equals Q) 3equls (P V ~Q) ^ (~P V Q)
(define(p3 P Q)
  ((boolean=? (boolean=? P Q) (and (or P (not Q)) (or (not P) Q)))))