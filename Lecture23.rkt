#lang lsl

(define-contract GhLast
  ;; symbols
  (OneOf (Constant 'none)
         (Constant 'g-called)
         (Constant 'h-called)))

(: last-fun GhLast)
(define last-fun 'none)

(define (update-last-g lf x)
  (cond [(equal? lf 'g-called) "error: g called again"] ;; faliieur
        [else 'g-called])) ;; ok


(: g (-> (AllOf Natural (Record update-last-g last-fun)) Natural))
(define (g x)
  (add1 x))

(define (update-last-h lf x)
  (cond [(equal? lf 'h-called) "error: h called again"] ;; faliieur
        [else  'h-called])) ;; ok

(: h (-> (AllOf Natural (Record update-last-h last-fun)) Natural))
;;(: h (-> Natural Natural))
(define (h x)
  (* 10 x))

;; PROCESS

(define (start-hadler others)
  (action 0 (list (send-packet "self" 2))))

(define (on-receive st msg)
  (let [(value (receive-packet-msg msg))]
  (action (+ value st)
          (list (send-packet "self" 2)))))

(define p1 (process (name "self")
                    (on-start start-hadler)
                    (on-receive on-receive)))