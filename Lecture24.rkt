#lang lsl

(define (start-handler others)
  (action 0 (list (send-packet "self" 2))))

(define (receive-handler st msg)
  (let [(value (receive-packet-msg msg))]
    (if (> st 10)
        (action st empty)
        (action (+ value st)
                (list (send-packet "self" 2))))))

(define (next-process name)
  (if (string=? name "a") "b" "a"))

(define (my-process name)
  (local [(define (start-handler others))
          (action 0 (list (send-packet name 2)))
          (define (receive-handler st msg)
  (let [(value (receive-packet-msg msg))]
    (if (> st 10)
        (action st empty)
        (action (+ value st)
                (list (send-packet name 2))))))]))

(define p1 (process (name "self")
                   (on-start start-handler)
                   (on-receive receive-handler)))



(start first (list p1))