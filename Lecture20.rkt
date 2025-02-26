#lang lsl
;; Mutable State

(define x 10)

;; int x = 10;
;; x = x + 1;

;; Already associated with an x
;; (define x (+ x 1))

(define-mutable-struct posn [x y])
(define my-posn (make-posn 1 2))

(define (f p)
  (set-posn-x! p 42))
my-posn

(define (g q)
  (f q))

(g my-posn)
my-posn

(define my-posn2 (make-posn 1 2))
(define (f2 p)
  ;; Change where it is being referring to 
  (set! p (make-posn 888 999)))

(f2 my-posn2)
my-posn2

(define (f3 x)
  (set! x (+ x 1)))

(define y 10)
(define yy 10)
(f3 y)
(f3 yy)
y
yy

(define-mutable-struct one [val])
(define (f4 x)
  (make-one (add1 (one-val x))))

(define z (make-one 10))
(f4 z)
z


(define (test x)
  (set-one-val! x 1))

(define c (make-one 10))
c
(test c)
c



;; EXERCISES

"X"
(local [(define X 10)
        (define (updater)
          (set! X (+ X 10)))
        (define (reader)
          X)]
  (begin (set! X 0)
          (updater))
  )

(define (a T)
  (lambda (x) (+ x T)))

(a 2)