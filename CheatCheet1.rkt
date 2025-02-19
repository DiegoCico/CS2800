#lang lsl

;; -------------
;; key notes
;; --------------

;; 📌 `list-ref` Explanation
;; `list-ref` retrieves an element from a list at a specific index.
;; Syntax: (list-ref list index)

;; ✅ Example Usage:
(define colors (list "red" "blue" "green" "yellow" "purple"))

(list-ref colors 0)  ;; Output: "red"   (first element)
(list-ref colors 2)  ;; Output: "green" (third element)
(list-ref colors 4)  ;; Output: "purple" (fifth element)



;; ------------------------------
;; CREATING TRUTH TABLE
;; ------------------------------
;; (P ^ Q) V (P ^ ~Q)
(define (p5 P Q)
  (and (or P Q) (or P (not Q))))
(check-expect (p5 #t #t) #t)
(check-expect (p5 #t #f) #t)
(check-expect (p5 #f #t) #f)
(check-expect (p5 #f #f) #f)

;; (P V Q) ^ (~R) ^ (Q V ~R)
(define (p1 P Q R)
  (or (and P Q)
      (not R)
      (and Q (not R))))

(check-expect (p1 #t #t #t) #t)
(check-expect (p1 #t #t #f) #t)
(check-expect (p1 #t #f #t) #f)
(check-expect (p1 #t #f #f) #t)
(check-expect (p1 #f #t #t) #f)
(check-expect (p1 #f #t #f) #t)
(check-expect (p1 #f #f #t) #f)
(check-expect (p1 #f #f #f) #t)

;; ~(P V Q V R) ^ (Q V ~P)
(define (p2 P Q R)
  (or (not (and P Q R))
      (and Q (not P))))

(check-expect (p2 #t #t #t) #f)
(check-expect (p2 #t #t #f) #t)
(check-expect (p2 #t #f #t) #t)
(check-expect (p2 #t #f #f) #t)
(check-expect (p2 #f #t #t) #t)
(check-expect (p2 #f #t #f) #t)
(check-expect (p2 #f #f #t) #t)
(check-expect (p2 #f #f #f) #t)

;; --------------------------------------------------
;; CONVERTING VARIABLE TYPES AND CONTRACT FUNCTIOSN
;; --------------------------------------------------

(: mymap (All (X) (-> (-> X X) (List X) (List X))))
;; - `mymap` is a polymorphic function that works for any type `X`.
;; - It takes:
;;   1. A function `f` of type `(-> X X)`, meaning `f` maps an `X` to another `X`.
;;   2. A list `l` of type `(List X)`, a list containing elements of type `X`.
;; - It returns a new list of type `(List X)`, where each element is transformed by `f`.
(define (mymap f l)
  (cond 
    [(empty? l) empty]  ;; Base case: return empty list if `l` is empty.
    [(cons? l)          ;; Recursive case: apply `f` to first element,
     (cons (f (first l)) ;; and recursively process the rest.
           (mymap f (rest l)))]))

(mymap number->string '(1 2 3))
;; '("1" "2" "3")
(mymap string->number '("1" "2" "3"))
;; '(1 2 3)

;; ------------------------------
;; IMPLODE/EXPLODE 
;; ------------------------------

(explode "hello")  
;; Output: (#\h #\e #\l #\l #\o)

(implode '("H" "i" "!"))  
;; Output: "Hi!"

(explode "")  
;; Output: ()

(implode '())  
;; Output: ""

(explode "racecar")  
;; Output: (#\r #\a #\c #\e #\c #\a #\r)

(equal? (explode "madam") (reverse (explode "madam")))  
;; Output: #t (Checks if "madam" is a palindrome)

(define (check-palindromic-number x)
  (let ([x-list (explode (number->string x))]) ;; Convert the number `x` to a string, then explode it into a list of characters.
    (equal? x-list (reverse x-list)))) ;; Check if the list of characters is the same when reversed (palindrome check).

(check-expect (check-palindromic-number 191) #t)
(check-expect (check-palindromic-number 10) #f)
(check-expect (check-palindromic-number 010) #f)

;; ------------------------------
;; Immediate
;; ------------------------------

(define-contract ContractName
  (Immediate (check <predicate>)  ;; Predicate function to validate values
             (generate <generator>)  ;; Function to generate test values
             (feature "<feature-name>" <feature-function>)  ;; Feature extraction (optional)
             (shrink <shrinking-function>)  ;; Shrinking strategy (optional)
  ))

;;COLOR EXAMPLE
(define RED "red")
(define BLUE "blue")
(define GREEN "green")
(define YELLOW "yellow")
(define PURPLE "purple")
(define ALL-COLORS (list RED BLUE GREEN YELLOW PURPLE))

(define-contract Color
  (Immediate 
    (check (lambda (x) (member? x ALL-COLORS)))  
    ;; Check if x is a valid color in ALL-COLORS.

    (generate (lambda (_)  
                (list-ref ALL-COLORS (random (length ALL-COLORS)))))  
    ;; Generate a random color from ALL-COLORS.
  ))

(check-contract Color "red")  ;; ✅ Passes
(check-contract Color "blue") ;; ✅ Passes
(check-contract Color "black") ;; ❌ Fails (not in ALL-COLORS)

(generate-contract Color)  ;; Randomly returns "red", "blue", etc.

;; PALINDRONE EXAMPLE

(define (generate-palindromic-number fuel)
  ;; Generates a random palindromic number using the given `fuel`.

  (let* ([x (contract-generate Natural fuel)]  
         ;; Generate a natural number `x` from the contract generator.
         
         [x-list (explode (number->string x))])  
         ;; Convert `x` into a list of characters (digits).
    
    (if (< (random) 1/2)  
        ;; Randomly decide which palindrome form to use.

        (string->number (implode (append x-list (reverse x-list))))  
        ;; Even-length palindrome: Append `x-list` with its reversed version.
        ;; Example: if x = 123 → "123321" → Output: 123321

        (string->number (implode (append x-list (rest (reverse x-list))))))))
        ;; Odd-length palindrome: Append `x-list` with the reversed version, excluding the first element.
        ;; Example: if x = 123 → "12321" → Output: 12321

(define-contract PalindromicNumber 
  (Immediate (check check-palindromic-number)  
  ;; Check function: Ensures the number is a palindrome.

             (generate generate-palindromic-number)))  
  ;; Generate function: Creates palindromic numbers.

(generate-palindromic-number 10)  
;; Possible Outputs: 123321, 4554, 878, 24642

(check-contract PalindromicNumber 121)  ;; ✅ Passes (121 is a palindrome)
(check-contract PalindromicNumber 123)  ;; ❌ Fails (123 is not a palindrome)

;; ------------------------------
;; DEFINE-STRUCT
;; ------------------------------

(define-struct circle (radius))
;; A `circle` has one field: `radius`.

(define-struct rectangle (width height))
;; A `rectangle` has two fields: `width` and `height`.

(define-struct square (side))
;; A `square` has one field: `side`.

;; Define a function to calculate the area of a shape

(: area (-> (OneOf (Struct circle [Natural])
                    (Struct rectangle [Natural Natural])
                    (Struct square [Natural]))
             Real)) ;; Number with decimals 

(define (area shape)
  (cond
    [(circle? shape) (* pi (sqr (circle-radius shape)))]  ;; Area of a circle: π * r2
    [(rectangle? shape) (* (rectangle-width shape) (rectangle-height shape))]  ;; w * h
    [(square? shape) (sqr (square-side shape))]))  ;; s2

(define c (make-circle 5))       ;; A circle with radius 5
(define r (make-rectangle 4 6))  ;; A rectangle with width 4 and height 6
(define s (make-square 3))       ;; A square with side length 3

(area c)  ;; Output: 78.54 (π * 52)
(area r)  ;; Output: 24 (4 * 6)
(area s)  ;; Output: 9 (32)

;; ------------------------------
;; DISTINCT PURE FUNCTIONS
;; ------------------------------
;; A distinct pure function is a pure function that is meaningfully different 
;; from others by producing a different output for at least one input.

(: foo (All (X) (-> (-> X X) (-> X X) X (Tuple X X X))))
(define (foo f1 f2 x) ...)
;; Infinite, can repeat applications of f1 or f2
;; There are infinitely many functions because for each element of the resulting tuple,
;; you can apply f1 and f2 in arbitrarily long (but finite) sequences to x.
;; In short: the freedom to nest f1 and f2 arbitrarily yields infinitely many outcomes.


(: bar (All (X Y) (-> (-> X Y) (-> X Y) X (Tuple Y Y Y))))
(define (bar f1 f2 x) ...)
;; 2^3 = 8
;; There are exactly 8 functions because for each of the three tuple components,
;; you have exactly 2 choices—either f1(x) or f2(x)—resulting in 2^3 = 8 total combinations.
;; In short: each component offers a binary choice, so there are exactly 8 distinct functions.


(: baz (All (X Y Z) (-> (-> X Y) (-> X Y) X (Tuple Z Z Z))))
(define (baz f1 f2 x) ...)
;; 0, we have no way of getting Z
;; There are 0 functions because the contract requires a value of type Z,
;; but we only have x : X and functions f1, f2 : X -> Y, with no way to obtain a Z.
;; In short: without a conversion from X or Y to Z, no function can satisfy the contract.

;; ----------------
;; TREES
;; ----------------

;; Define a leaf structure that holds a single value.
(define-struct leaf [value])

;; Define a node structure with two children: left and right.
(define-struct node [left right])

;; A contract that ensures an IntTree is either:
;;  1) A leaf containing an Integer
;;  2) A node containing two valid IntTrees
(define-contract IntTree
  (OneOf (Struct leaf [Integer])
         (Struct node [IntTree IntTree])))

;; The contract (Tree X) defines a tree that holds values of type X.
;; A valid tree is either:
;;   - A Leaf containing a value of type X, or
;;   - A Node with two subtrees, each of which is a (Tree X).
(define-contract (Tree X)
  (OneOf (Leaf X)
         (Node (Tree X) (Tree X))))

;; -- Example 1: Creating a Leaf --
;; Create a leaf that holds the number 42.
(define leaf1 (make-leaf 42))  ;; A leaf with the value 42.

;; -- Example 2: Creating a Simple Node --
;; Create a node with two leaves:
;;   - Left subtree: a leaf with value 1.
;;   - Right subtree: a leaf with value 2.
(define tree1 
  (make-node (make-leaf 1)   ;; Left subtree: leaf with value 1.
             (make-leaf 2)))  ;; Right subtree: leaf with value 2.

;; -- Example 3: Creating a More Complex Tree --
;; Create a tree with nested nodes:
;;   - Left subtree: a leaf with value 3.
;;   - Right subtree: a node with:
;;       * Left subtree: a leaf with value 4.
;;       * Right subtree: a leaf with value 5.
(define tree2 
  (make-node (make-leaf 3)              ;; Left subtree: leaf with value 3.
             (make-node (make-leaf 4)    ;; Right subtree: node.
                        (make-leaf 5)))) ;; Node's right subtree: leaf with value 5.

(check-contract (Tree Number) leaf1)   ;; Passes: 42 is a Number.
(check-contract (Tree Number) tree1)     ;; Passes: both leaves contain numbers.
(check-contract (Tree Number) tree2)     ;; Passes: all leaves contain numbers.

;; Creating a tree with an invalid value (a string) for a tree expecting a Number.
(define invalid-tree (make-leaf "oops"))  ;; A leaf with a string value.
(check-contract (Tree Number) invalid-tree)  ;; Fails: "oops" is not a Number.


;; QUIZ

(define (all-even? tree)
  (cond
    [(leaf? tree)
     (even? (leaf-val tree))]
    [(node? tree)
     (and (all-even? (node-left tree))
          (all-even? (node-right tree)))]
    [else
     ;; If it's neither leaf nor node (which shouldn't happen
     ;; if the contract IntTree is enforced), we can return #f
     ;; or signal an error. We'll return #f for safety.
     #f]))

(define my-leaf (make-leaf 4))               ;; A leaf with an even number
(define my-bad-leaf (make-leaf 3))           ;; A leaf with an odd number
(define my-tree (make-node (make-leaf 2)     ;; A node with:
                           (make-leaf 6)))   ;;   - left leaf: 2
                                             ;;   - right leaf: 6 (both even)

(all-even? my-leaf)       ;; => #t  (4 is even)
(all-even? my-bad-leaf)   ;; => #f  (3 is odd)
(all-even? my-tree)       ;; => #t  (2 and 6 are even)