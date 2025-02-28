#lang lsl

;; Problem 1

;; part p1a
(define-mutable-struct counter (val))
(define-contract (Counter X) (Struct counter [X]))

(: make-counter-1 (-> (-> Natural Natural)))
(define make-counter-1
  (let ([c (make-counter 0)])
    (lambda ()
      (lambda (inc)
        (begin
          (set-counter-val! c (+ inc (counter-val c)))
          (counter-val c))))))

(: make-counter-2 (-> (-> Natural Natural)))
(define make-counter-2
  (lambda ()
    (let ([c (make-counter 0)])
      (lambda (inc)
        (begin
          (set-counter-val! c (+ inc (counter-val c)))
          (counter-val c))))))
;; part p1a

;; part p1b
(: counter-distinguish (-> (-> (-> Natural Natural)) Natural))
(define (counter-distinguish make-counter)
  (let ([c1 (make-counter)]
        [c2 (make-counter)])
    (abs (- (c1 1) (c2 1)))))

(check-expect (not (equal? (counter-distinguish make-counter-1)
                           (counter-distinguish make-counter-2)))
              #t)
(check-expect (counter-distinguish make-counter-1) 1)
(check-expect (counter-distinguish make-counter-2) 0)
(define cd1a (counter-distinguish make-counter-1))
(define cd1b (counter-distinguish make-counter-1))
(check-expect cd1a 1)
(check-expect cd1b 1)
(define cd2a (counter-distinguish make-counter-2))
(define cd2b (counter-distinguish make-counter-2))
(check-expect cd2a 0)
(check-expect cd2b 0)
(check-expect (not (equal? (counter-distinguish make-counter-1)
                           (counter-distinguish make-counter-2)))
              #t)


;; Problem 2

(: fast-incr (-> (Counter Natural) (Counter Natural) Natural))
(define (fast-incr c1 c2)
  (begin (set-counter-val! c1 (+ (counter-val c1) 1))
         (set-counter-val! c2 (+ (counter-val c2) 1))
         (+ (counter-val c1) (counter-val c2))))

(: fast-incr-prop (-> (Counter Natural) (Counter Natural) True))
(define (fast-incr-prop c1 c2)
  (equal? (+ (counter-val c1) (counter-val c2) 2)
          (fast-incr c1 c2)))

(: fast-incr-exercise (-> Natural))
(define (fast-incr-exercise)
  (let ([c (make-counter 0)])
    (fast-incr c c)))


(check-expect (fast-incr-exercise) 4)


