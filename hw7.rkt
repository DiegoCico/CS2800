#lang lsl


;; part p1a
(define EMPTY 'empty)
(define WALL 'wall)
(define PLAYER 'player)
(define EXIT 'exit)

(define-contract Cell (OneOf (Constant EMPTY)
                             (Constant WALL)
                             (Constant PLAYER)
                             (Constant EXIT)))
;; part p1a


;; part p1b
(define-struct posn (x y))
(define-struct at (c pos))

(define-contract (At X Y) (Struct at [X Y]))
(define-contract Posn (Struct posn [Natural Natural]))

(define-contract Maze (List (At Cell Posn)))
;; part p1b

;; part p1c
(define-contract SExp (OneOf Symbol
                             Integer
                             Boolean
                             String
                             (List SExp)))
;; part p1c

;; part p3
(define-struct invalid-sexp (sexp))
;; part p3

;; Problem 1


(define (all-positions dimension)
  (letrec ((helper (lambda (x y acc)
                     (cond
                       [(>= x dimension) (reverse acc)]
                       [(>= y dimension) (helper (+ x 1) 0 acc)]
                       [else (helper x (+ y 1) (cons (make-posn x y) acc))]))))
    (helper 0 0 '())))

(define (random-cell)
  (let ((options (list EMPTY WALL)))
    (list-ref options (random (length options)))))

(define (random-maze dimension)
  (let* ((positions (all-positions dimension))
         (player-pos (list-ref positions (random (length positions))))
         (non-player-positions (filter (lambda (p) (not (equal? p player-pos)))
                                       positions))
         (exit-pos (list-ref non-player-positions (random (length non-player-positions)))))
    (map (lambda (p)
           (cond
             [(equal? p player-pos) (make-at PLAYER p)]
             [(equal? p exit-pos)   (make-at EXIT p)]
             [else                 (make-at (random-cell) p)]))
         positions)))

(check-expect (length (random-maze 3)) 9)


;; Problem 2
(: single-player-exit? (-> (List at?) Boolean))
(define (single-player-exit? maze)
  (and
   (= (length (filter (lambda (cell-at) (equal? (at-c cell-at) PLAYER))
                        maze))
      1)
   (= (length (filter (lambda (cell-at) (equal? (at-c cell-at) EXIT))
                        maze))
      1)))

(define (random-maze-2 dimension)
  (let ((maze (random-maze dimension)))
    (if (single-player-exit? maze)
        maze
        (random-maze-2 dimension))))

(check-contract single-player-exit?)

(check-expect (single-player-exit? (random-maze-2 3)) #t)
(check-expect (length (random-maze-2 3)) 9)

;; Problem 3

(define sexp_DB (list 'X '_ 'P 'E))

(define-contract sexp_options
  (Immediate
    (check (lambda (s)
             (cond
               [(eq? s 'X) s]
               [(eq? s '_) s]
               [(eq? s 'P) s]
               [(eq? s 'E) s]
               [else (list-ref sexp_DB (random (length sexp_DB)))])))
    (generate (lambda (fuel)
                (list-ref sexp_DB (random (length sexp_DB)))))))

;; should be SExp but causes errors? 
(: sexp->cell (-> sexp_options Cell))
(define (sexp->cell s)
  (cond
    [(eq? s 'X) WALL]
    [(eq? s '_) EMPTY]
    [(eq? s 'P) PLAYER]
    [(eq? s 'E) EXIT]
    [else (raise (make-invalid-sexp s))]))



(: cell->sexp (-> Cell SExp))
(define (cell->sexp cell)
  (cond
    [(eq? cell WALL) 'X]
    [(eq? cell EMPTY) '_]
    [(eq? cell PLAYER) 'P]
    [(eq? cell EXIT) 'E]
    [else (raise (make-invalid-sexp cell))]))

(check-contract sexp->cell)
(check-contract cell->sexp)

(check-expect (sexp->cell 'X) WALL)
(check-expect (sexp->cell '_) EMPTY)
(check-expect (sexp->cell 'P) PLAYER)
(check-expect (sexp->cell 'E) EXIT)

(check-expect (cell->sexp WALL) 'X)
(check-expect (cell->sexp EMPTY) '_)
(check-expect (cell->sexp PLAYER) 'P)
(check-expect (cell->sexp EXIT) 'E)

;; Problem 4
(: cell-roundtrip-prop (-> Cell Boolean))

(define (cell-roundtrip-prop cell)
  (equal? (sexp->cell (cell->sexp cell)) cell))

(check-contract cell-roundtrip-prop)


;; Problem 5


;; Problem 6


;; Problem 7


;; Problem 8

