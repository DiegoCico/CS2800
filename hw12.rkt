#lang lsl

(define STARTBAL 1000)
(define MAX-TRANSFERS 20)

(define-struct bank-state-v1 (balance num-transfers other-banks))
(define-contract BS1 (Struct bank-state-v1 [Natural Natural (List String)]))

(define-struct transfer (bal))
(define-contract Transfer (Struct transfer [Natural]))

(define (choose-random lst)
  (list-ref lst (random (length lst))))

(define (random-transfer-amount balance)
  (if (zero? balance)
      0
      (add1 (random balance))))

(: bank-start-v1 (-> (List String) (Action BS1)))
(define (bank-start-v1 others)
  (let* ([init-bal STARTBAL]
         [amount (random-transfer-amount init-bal)]
         [target (choose-random others)]
         [new-bal (- init-bal amount)]
         [init-state (make-bank-state-v1 new-bal 0 others)])
    (begin
      (action init-state (list (send-packet target (make-transfer amount)))))))

;; bank receive not working?

(: bank-receive-v1 (-> BS1 (ReceivePacket Transfer) (Action BS1)))
(define (bank-receive-v1 st pkt)
  (let* ([received-amt (transfer-bal (receive-packet-msg pkt))]
         [new-bal (+ (bank-state-v1-balance st) received-amt)]
         [new-num (add1 (bank-state-v1-num-transfers st))]
         [other-banks (bank-state-v1-other-banks st)]
         [state-after-receipt (make-bank-state-v1 new-bal new-num other-banks)])
    (if (< new-num MAX-TRANSFERS)
        (let* ([transfer-amt (random-transfer-amount new-bal)]
               [target (choose-random other-banks)]
               [final-bal (- new-bal transfer-amt)]
               [final-state (make-bank-state-v1 final-bal new-num other-banks)])
          (action final-state (list (send-packet target (make-transfer transfer-amt)))))
        (action state-after-receipt '()))))


(define (bank-process-v1 nm)
  (process (name nm)
           (on-start bank-start-v1)
           (on-receive bank-receive-v1)))

(define (bank-v1)
  (start first (list (bank-process-v1 "bank1")
          (bank-process-v1 "bank2")
          (bank-process-v1 "bank3")
        (bank-process-v1 "bank4")
        (bank-process-v1 "bank5"))))

;; testing if it works
;;(start-debug first (list (bank-process-v1 "a") (bank-process-v1 "b")))

;; testng bank 
;(start-debug first (bank-v1))
(bank-v1)



