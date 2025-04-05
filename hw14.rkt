#lang racket

(require lsl/performance)

;; Problem 1

;; part p1a
(define (list-reverse l)
  (define (list-reverse-acc l acc)
    (if (empty? l)
        acc
        (list-reverse-acc (rest l) (cons (first l) acc))))
  (list-reverse-acc l empty))

;; part p1a

;; switched the complexity to O(n^2) -> O(N)
(visualize (build-list 10 (lambda (n) (* n 1000))) list-reverse)


;; Problem 2

;; part p2a
(define (fib n)
  (define (fib-iter count a b)
    (if (= count 0)
        a
        (fib-iter (- count 1) b (+ a b))))
  (fib-iter n 1 1))

;; part p2a

;; was around O(n^2) -> O(N)
(visualize (build-list 10 (lambda (n) (* n 1000))) fib)