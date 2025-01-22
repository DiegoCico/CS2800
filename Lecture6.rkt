#lang lsl

;; flt : Natural Natural Natural Boolean
(: flt (-> Natural Natural Natural False))
(define (flt a b c)
  (and (> a 0) (> b 0) (> c 0)
       (= (expt c 3) (+ (expt a 3) (expt b 3)))))

(check-contract flt 100)

(define (collatz-trace n)
  (cond [(<= n 1) (list 1)]
        [(even? n) (cons n (collatz-trace (/ n 2)))]
        [else (cons n (collatz-trace (+ (* 3 n) 1)))]))


;; Soundeness and Completeness

;; Soundness = alll provable statements are _true_
;; THat means we cannot prove statements that are false

;; Completeness = all true  statements are provable
;; However, false statements might be provable

;; Soundness and Completeness of Specification

;; A specification is _sound_ if all the implementations
;; that it allows (admits) are correct.
;; There might be implementations that are correct but are not
;; admitted by the specification

;; A specification is _complete_ if all correct implementations
;; are admitted
;; However, it might admit some incorrect ones.

(define (longest-string los) ...)

(check-expect (longest-string (list "a" "bb" "ccc" "dd")) "ccc")
(check-expect (longest-string empty) "")
(check-expect (longest-string (list "a")) "a")


(: longest string-porp (-> (List Strig) True))
(define (longest-string-prop los)
  (let ([longest (logest-string los)]
;; For all strings s in los, (string-length longest) >=
(andmap (lambda (s) (>= string-length longest)
          (string-length s)))
los)))

(define (longest-string los)
  (cond [(empty? los) ""]
        [(cons? los) (if (>= ( string-length (first loss))
                         (string-length (longest-string (rest lost))))
                         (first los)
                         (longest-string (res los)))]))

(define (longest-string-folder los)
  foldr (lambda (s longest) (if (>= (string-length s) (string-length longest))
                                s
                                longest))
  ""
  los)