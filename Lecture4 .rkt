#lang lsl


;; Immediate COntractss
;; imediate check
;;           generate

(define-contract Even
  (Immediate
   (check (lambda (x)
            (and (integer? x) (even? x))))
   (generate (lambda (fuel)
               (* 2 (random (quotient fuel 2)))))))

(define-contract NonGenEven even?)
(define-contract Positive positive?)


;; LIST
;; (list 1 "ferd" #t)
;; List Integer
(define-contract HomeworkPoints (List Points))

(: only-non-negatives (-> (List Integer) (List Natural)))
;; Return only the non-negative integer
(define (only-non-negative xs)
  (filter (lambda (x) (or (zero? x) (positive?))) xs))

(check-contract only-non-negative)

;; Tuples: fixed-sized lists with valus of given tupes
(define-contract TwoIntsAndNat (Tuple Integer Integer Natural))

;; kv-swap : [Tuple Natural String] -> [Tuple String Natural]
(: kv-swap (-> (Tuple Natural String) (Tuple String Natural)))
(define (kv-swap kv)
  (list (second kv) (first kv)))

;; Function Design REcipe
;; 1 Signature
;; 2 Purpose
;; 3 Exaples/Tests
;; 4 Code

;; Design + Specification Recipe
;; 1 Signature (as a contract)
;; 2 Purpose
;; 3 Specification/Tests
;; 4 Code / Implementation
;; 5 Verification

;; Case Study: Conversion of Colors to B&W
;; We want to be able to convert colors into
;; B&W values, luminance, so that a given set
;; of colors is still distinguishable

;; A Color is one of:
;; - "red"
;; - "blue"
;; - "green"
;; - "yellow"
;; - "purple"
(define COLOR-RED "red")
(define COLOR-BLUE "blue")
(define COLOR-GREEN "green")
(define COLOR-YELLOW "yellow")
(define COLOR-PURPLE "purple")
(define ALL-COLORS
  (list COLOR-RED
        COLOR-BLUE
        COLOR-GREEN
        COLOR-YELLOW
        COLOR-PURPLE))

(define-contract Color
  (Immediate (check (lambda (c) (member? c ALL-COLOR)))
             (generate (lambda (_)
                         (list-ref ALL-COLORS (random 0
                                     (length ALL-COLORS)))))))

;; color -> b&w : Color -> Number[0,255]
;; Conver a color to its luminance (b&w value), such that
;; it is distinguishable from other colors


;; Spec: For any color c, its luminance should be at least a
;; given threshold (20) away from all other colors' luminances.
(: color->b&w-prop (-> Color True))
(define MIN-DELTA 20)
(define (color->b&w-prop c)
  ;; for anny other c1, 
  ;; (abs (color->b&w) - (color->b&w c2)) > MIN-DELTA
  (andmap (lambda) (c1) (> (abs (- (color->b&w c) (color->b&w c1))) MIN-DELTA) ALL-COLOR))



