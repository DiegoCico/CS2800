#lang lsl


;; Problem 1 -- look over
(: knapsack-spec (-> (List (Struct item (Natural Natural)))
                     Natural
                     True))
(define (knapsack-spec items max-weight)
  (let* ([combinations (powerset items)]
         [valid-combinations (filter (lambda
                                         (comb)
                                       (<= (sum (map item-weight comb)) max-weight)) combinations)]
         [potential-max (apply-max (sum (map item-weight valid-combinations)))]
         [actual (knspsack items max-weight)])
    (and (member? actual valid-combinations)
         (apply-max (sum (map item-weight valid-combiations))))
    ))



;; problem 2 -- easy


;; problem 3 -- look over Immediate generate
;; you could switch out the String to a contract of MenuItem to pick something from the menu item
;; PRINT OUT CHECK IMMEDIATEE
;; ACTUALLY PRINT OUT THE WHOLE DRRACKET DOCS (


;; Problem 4

;; Implode combine a list of characters togerther
;; explode beaks a string or character apart
(define (check-palindrome x)
  (let* ([x-list (explode (number->string x))])
    (equal? (implode (reverse x-list)) (number->string x))))


(check-palindrome 121)  ;; #t
(check-palindrome 123)  ;; #f
(check-palindrome 1221) ;; #t



;; EXAM UP TO LECTURE 15