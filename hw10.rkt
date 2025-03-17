#lang lsl

;; Problem 1
;; Define the state for the "a" process

;; part p1

;; 0 -> a
;; 1 -> b
;; 2 -> c
(define-contract AState (OneOf Natural))
;; part p1

;; Problem 2
;; Define handlers for the "a" process

(: a-start (-> (List String) (Action AState)))
(define (a-start others)
  (action 1 (list (send-packet "b" "hello"))))

(test-suite
 "a-start"
 (check-expect (a-start '("b" "c"))
               (action 1 (list (send-packet "b" "hello")))))

(: a-receive (-> AState (ReceivePacket String) (Action AState)))
(define (a-receive st pkt)
  (if (equal? pkt (receive-packet "c" "got it"))
      (action 1 (list (send-packet "b" "hello")))
      (action st '())))

(test-suite
 "a-receive"
 (check-expect (a-receive 2 (receive-packet "c" "got it"))
               (action 1 (list (send-packet "b" "hello"))))
 (check-expect (a-receive 2 (receive-packet "b" "got it"))
               (action 2 '()))) 

;; part p2
;; Problem 3
;; Define state for the "b" process

;; part p3
(define-contract BState (OneOf Natural))
;; part p3

;; Problem 4
;; Define handlers for the "b" process

;; part p4
(: b-start (-> (List String) (Action BState)))
(define (b-start others)
  (action 1 '()))

(test-suite
 "b-start"
 (check-expect (b-start '("a" "c")) (action 1 '())))

(: b-receive (-> BState (ReceivePacket String) (Action BState)))
(define (b-receive st pkt)
  (if (equal? pkt (receive-packet "a" "hello"))
      (action 1 (list (send-packet "c" "hello")))
      (action st '())))

(test-suite
 "b-receive" 
 (check-expect (b-receive 1 (receive-packet "a" "hello"))
               (action 1 (list (send-packet "c" "hello"))))
 (check-expect (b-receive 1 (receive-packet "c" "hello")) 
               (action 1 '()))) 

;; part p5
(define-contract CState (OneOf Natural))
;; part p5

;; Problem 6
;; Define handlers for the "c" process

;; part p6
(: c-start (-> (List String) (Action CState)))
(define (c-start others)
  (action 0 '()))

(test-suite
 "c-start"
 (check-expect (c-start '("a" "b")) (action 0 '())))

(: c-receive (-> CState (ReceivePacket String) (Action CState)))
(define (c-receive st pkt)
  (if (and (equal? pkt (receive-packet "b" "hello")) (< st 4))
      (action (+ st 1) (list (send-packet "a" "got it")))
      (action st '())))

(test-suite
 "c-receive"
 (check-expect (c-receive 0 (receive-packet "b" "hello"))
               (action 1 (list (send-packet "a" "got it"))))
 (check-expect (c-receive 3 (receive-packet "b" "hello"))
               (action 4 (list (send-packet "a" "got it"))))
 (check-expect (c-receive 4 (receive-packet "b" "hello")) 
               (action 4 '()))) 
;; part p6
