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
;; (bank-v1)





;; Problem 2

 (define UNTIL-SNAPSHOT 10) 

 (define-struct bank-state (balance num-transfers other-banks snapshot ignored))
                                    
 (define-contract BS (Struct bank-state [Natural Natural (List String) (Maybe Natural) (List String)])) 
  
 (define-struct marker ()) 
 (define-contract Marker (Struct marker [])) 
  
 (define-contract Message (OneOf Transfer Marker))


 (: bank-start (-> (List String) (Action BS)))
 (define (bank-start others)
   ;; start by initiating the balance
  (let* ([init-bal STARTBAL]
         ;; ammount to transfer
         [amount (random-transfer-amount init-bal)]
         ;; who tranferring to
         [target (choose-random2 others)]
         ;; the new balance
         [new-bal (- init-bal amount)]
         ;; randomly start the snapshot w 50 50 chance
         ;; should it be false if it has not been started?
         [snapshot #f]
         ;; fn for initiating the banks state
         ;; now using make-bank-state

         ;; what should the initial value of snapshot be? curr -> snapshot
         [init-state (make-bank-state new-bal 0 others snapshot '())])
    (begin
      ;; now init the state using all the prior vars
      (action init-state (list (send-packet target (make-transfer amount)))))))

(check-contract bank-start)

 #| things to consider:
 - is there a snapshot going on?
 - 

 |#

;; new choose random that wont call random on an empty list
;; makes check-contract work
(define (choose-random2 lst)
  (if (empty? lst) "no banks to send to"
  (list-ref lst (random (length lst)))))

 (: bank-receive (-> BS (ReceivePacket Message) (Action BS))) 
 (define (bank-receive st pkt)

   ;; store all of the orignal vfalues for easy access
   (let* ([msg (receive-packet-msg pkt)]
         [sender (receive-packet-from pkt)]
         [bal (bank-state-balance st)]
         [num-transfers (bank-state-num-transfers st)]
         [other-banks (bank-state-other-banks st)]
         [snapshot (bank-state-snapshot st)]
         [ignored (bank-state-ignored st)])
     (cond [(transfer? msg)
            ;; perform if there is a transfer happening
            (let* ([received-amt (transfer-bal msg)]
                   ;; add bal to prev bal
                   [new-bal (+ (bank-state-balance st) received-amt)]
                   ;; inc the number of transfers
                   [new-num-transfers (add1 num-transfers)]
                   ;; if it should be added to the snapshot
                   ;; only if snapshot is greater than 0, since otherwise snapshot not happenign
                   [add-to-snap (and (not (false? snapshot)) (not (member? sender ignored)))]
                   ;; if the snapshot needs to be updated
                   ;; this is what hte value shold be equal to after the snaphost
                   [new-snap (if add-to-snap
                                 ;; increase by recieved amt
                                (+ snapshot received-amt)
                                snapshot)]

                   [start-snap? (and (false? snapshot)
                                     ;; start the snap w random 50% chance
                                          (>= new-num-transfers UNTIL-SNAPSHOT)
                                          (= 0 (random 2)))]
              
                   ;; if a snapshoot was started, then return new bal otheriwse return new-snap -> in the case that a snapshot was already requested
                   [snap-val (if start-snap? new-bal new-snap)]
                   ;; generates the marker messages that are sent to the other banks if a snapshot is started
                   [marker-msg (if start-snap? (map (lambda (b) (send-packet b (make-marker))) other-banks) '())])
              (if (< new-num-transfers MAX-TRANSFERS)
                  ;; same as before but now mark transfer and update for bank-state
        (let* ([transfer-amt (random-transfer-amount new-bal)]
               [target (choose-random2 other-banks)]
               [final-bal (- new-bal transfer-amt)]
               [final-state (make-bank-state final-bal new-num-transfers other-banks snap-val ignored)]
               [transfer (send-packet target (make-transfer transfer-amt))])
          ;; make sure to append the final marker actions onto
          (action final-state (append marker-msg (list transfer))))
        (action (make-bank-state new-bal new-num-transfers other-banks snap-val ignored)
                     marker-msg)))]
           ;; other condition 
           [(marker? msg)
            ;; if greater than 0 that means a snapshot happened
            (let* ([snapshotted? (not (false? snapshot))]
                   ;; add the sender to the ignore list
                   [new-ignored (cons sender ignored)])
              (if (not snapshotted?)
                  ;; this means that this isthe first marker being afdded
                  ;; snpahost + send markers to the other banks
                  (let* ([snapshot-val bal]
                         [marker-msg (map (lambda (b) (send-packet b (make-marker))) other-banks)]
                         [new-state (make-bank-state bal num-transfers other-banks snapshot-val new-ignored)])
                    (action new-state marker-msg))
                  ;; otherwise can just add the sender to the ignored list 
               (action (make-bank-state bal num-transfers other-banks snapshot new-ignored) '())))])))
                    
(check-contract bank-receive)





              
              
             
 (define (bank-process nm)
   (process
    (name nm)
    
    (on-start bank-start)
    (on-receive bank-receive)))
  
 (define (bank)
   (start first
          (list (bank-process "b1")
                 (bank-process "b2")
                  (bank-process "b3"))))
;; Problem 3

;; should take int hte output of bank
;; bank should retunr the list of all the final process states 
;; returns the #t if the total recorded in all the snapshot fields sums up to startbal
(define (snapshot-correct? final-states)
  (let* ([snapshots (map (lambda (pair)
                           ;; the snapshot will be in idx 1 of the state for each state
                           ;; snapshots to be this from map
                           (bank-state-snapshot (second pair)))
                         final-states)]
         ;; only keep teh snapshots that are numebrs
         [valid-snaps (filter number? snapshots)] 
         [total (apply + valid-snaps)]
         ;; must multiply over all states bc each bank starts w that amt
         [expected (* STARTBAL (length final-states))])
    (= total expected)))