#lang lsl

;; Logical Student Language - LSL

;; f1 : Number Number  -> Number
;; Add two numbers

(define (f1 x y)
(+ x y))

(check-expect (f1 10 6) 16)

;; my-abs : Number -> NonNegativeNumber
;; Absolute valye of the given number
(define (my-abs x)
  (if (>= x 0)
      x
      (* x -1)))

(check-expect (my-abs 10) 10)
(check-expect (my-abs -2) 2)

;; nonnegative? : Number -> Boolean
;; Is the given number positive or zero?
(define (nonnegative? x)
  (or (zero? x)
      (positive? x)))

(check-satisfied (my-abs 10) nonnegative?)
(check-satisfied (my-abs -2) nonnegative?)
(check-satisfied (my-abs 0) nonnegative?)

;; prime-factor-of : Integer -> Integer 
;; Returns one of the prime factors of the given number


;; How do we know that a given number is a prime facotr of another given number?
;; given x and y, when is x a  prime factor of y?

;; Property:
;; when x divides y
;; and
;; only has factoes one and itself - is a prime

;; prime-factor? : Number Number -> Boolean
;; Is the first given unmber a prime factor of the second given number?
(define (prime-factor? x y)
  (let* ([divides? (lambda (x y) (zero? (remainder y x)))]
        [prime? (lambda (x) (andmap (lambda (y) (not (divides? y x))) 
                                    (range 2 (sqrt x))))])
    (and (divides? x y)
         (prime? x))))

(check-expect (prime-factor? 7 7) #true)
(check-expect (prime-factor? 2 10) #true)
(check-expect (prime-factor? 5 10) #true)
(check-expect (prime-factor? 7 10) #false)


