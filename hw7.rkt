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

(define maze1 (list (make-at 'wall (make-posn 1 1))))

(define maze2 (list (make-at 'wall (make-posn 0 1))
                    (make-at 'wall (make-posn 1 1))))

;;(check-expect (maze->sexp maze1)


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


(: sexp->cell
   (Function (arguments (_ SExp))
             (result Cell)
             (raises invalid-sexp)))

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

(check-contract sexp->cell 4)
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

;; gets max x in maze
(define (max-x maze)
  ;; iterates through eaxhx cell and finds 
  (apply max (map (lambda (a) (posn-x (at-pos a))) maze)))

;; gets max y in maze
(define (max-y maze)
  (apply max (map (lambda (a) (posn-y (at-pos a))) maze)))

;; finds cell at a given index
(define (find-cell-at maze x y)
  ;; finds a cell that has matching x and y coords and will retunr it
  (memf (lambda (cell) (and (= (posn-x (at-pos cell)) x)
                            (= (posn-y (at-pos cell)) y))) maze))

(: maze->sexp (-> Maze SExp))
(define (maze->sexp maze)
  (if (empty? maze)
      '()
      (build-list (add1 (max-y maze))
                  (lambda (y)
                    (build-list (add1 (max-x maze))
                                (lambda (x)
                                  (if (list? (find-cell-at maze x y))
                                      (cell->sexp (at-c (first (find-cell-at maze x y))))
                                      '_)))))))
                              

(check-contract maze->sexp)

(check-expect (maze->sexp '())
              '())

(define example-maze
  (list
    (make-at PLAYER (make-posn 0 0))  
    (make-at WALL   (make-posn 1 0))
    (make-at EMPTY  (make-posn 2 0))  
    (make-at EMPTY  (make-posn 0 1)) 
    (make-at WALL   (make-posn 1 1))  
    (make-at EMPTY  (make-posn 2 1))  
    (make-at WALL   (make-posn 0 2))  
    (make-at EMPTY  (make-posn 1 2))  
    (make-at EXIT   (make-posn 2 2)))) 

(check-expect (maze->sexp example-maze)
              '((P X _)
                (_ X _)
                (X _ E)))

(check-expect (maze->sexp example-maze)
              '((P X _)
                (_ X _)
                (X _ E)))

(check-expect (maze->sexp (list (make-at PLAYER (make-posn 1 1))))
              '(( _ _)
                (_ P)))

(check-expect (maze->sexp (list (make-at PLAYER (make-posn 0 0))
                                 (make-at EMPTY (make-posn 1 0))
                                 (make-at EXIT (make-posn 2 0))))
              '((P _ E)))

(check-expect (maze->sexp (list (make-at PLAYER (make-posn 0 0))
                                 (make-at WALL (make-posn 0 1))
                                 (make-at EXIT (make-posn 0 2))))
              '((P)
                (X)
                (E)))

(check-expect (maze->sexp (list (make-at PLAYER (make-posn 0 0))
                                 (make-at EXIT (make-posn 2 2))))
              '((P _ _)
                (_ _ _)
                (_ _ E)))

(check-expect (maze->sexp (list (make-at WALL (make-posn 1 1))
                                 (make-at PLAYER (make-posn 0 0))
                                 (make-at EXIT (make-posn 2 2))))
              '((P _ _)
                (_ X _)
                (_ _ E)))

(check-expect (maze->sexp (list (make-at PLAYER (make-posn 0 0))
                                 (make-at WALL (make-posn 2 0))
                                 (make-at WALL (make-posn 0 1))
                                 (make-at EXIT (make-posn 0 2))))
              '((P _ X)
                (X _ _)
                (E _ _)))

(define maze-2x3
  (list (make-at PLAYER (make-posn 0 0))
        (make-at EMPTY  (make-posn 1 0))
        (make-at WALL   (make-posn 2 0))
        (make-at WALL   (make-posn 0 1))
        (make-at EXIT   (make-posn 1 1))
        (make-at EMPTY  (make-posn 2 1))))

(check-expect (maze->sexp maze-2x3)
              '((P _ X)
                (X E _)))




;; NOW sexp->maze STUFF


;; converts a single  row
(define (row-converter row x y)
  (if (empty? row) '()
      (cons (make-at (sexp->cell (first row)) (make-posn x y))
            (row-converter (rest row) (add1 x) y))))

;; all row converter
;; takes in rows and y (only need to track y since only concerned w that value since each row contains x calues)
(define (every-row-converter rows y)
  (if (empty? rows) '()
      (append (row-converter (first rows) 0 y)
              (every-row-converter (rest rows) (add1 y)))))

(define (valid-sexp? sexp)
  (and (list? sexp)
       (andmap list? sexp) 
       (andmap (lambda (row)
                 (andmap (lambda (x) (member? x (list 'X '_ 'P 'E))) row)) sexp)))



(: sexp->maze 
   (Function (arguments (_ SExp))
             (result Maze)
             (raises invalid-sexp)))

(define (sexp->maze sexp)
  (if (not (list? sexp))
      (raise (make-invalid-sexp sexp))
      (if(empty? sexp)
         '()
         (if (valid-sexp? sexp)
         (every-row-converter sexp 0)
         (raise (make-invalid-sexp sexp))))))


(check-raises (sexp->maze 4))
(check-raises (sexp->maze "X"))
(check-contract sexp->maze 2)



(check-expect (sexp->maze '((X X P)
  (E X _)
  (_ _ _))
) (list
 (make-at 'wall (make-posn 0 0))
 (make-at 'wall (make-posn 1 0))
 (make-at 'player (make-posn 2 0))
 (make-at 'exit (make-posn 0 1))
 (make-at 'wall (make-posn 1 1))
 (make-at 'empty (make-posn 2 1))
 (make-at 'empty (make-posn 0 2))
 (make-at 'empty (make-posn 1 2))
 (make-at 'empty (make-posn 2 2))))

(check-expect (maze->sexp (list
 (make-at 'wall (make-posn 0 0))
 (make-at 'wall (make-posn 1 0))
 (make-at 'player (make-posn 2 0))
 (make-at 'exit (make-posn 0 1))
 (make-at 'wall (make-posn 1 1))
 (make-at 'empty (make-posn 2 1))
 (make-at 'empty (make-posn 0 2))
 (make-at 'empty (make-posn 1 2))
 (make-at 'empty (make-posn 2 2)))) '((X X P)
  (E X _)
  (_ _ _)))



(check-expect (maze->sexp maze1) '((_ _) (_ X)))
(check-expect (maze->sexp maze2) '((_ _) (X X)))

;;(check-expect (sexp->maze '((_ _) (_ X))) (list (make-at 'wall (make-posn 1 1))))





;; Problem 6

(: maze-roundtrip-prop (-> Maze True))
(define (maze-roundtrip-prop maze)
  ;; now filters out hte empty spaces (in the case taht they were filled up (ie maze1))
  ;;(equal? (filter (lambda (cell) (not (eq? (at-c cell) 'empty))) (sexp->maze (maze->sexp maze))) maze))
  (equal? (sexp->maze (maze->sexp maze)) maze))

(check-contract cell-roundtrip-prop)


;; Problem 7

(define (find-player maze)
  (at-pos (first (memf (lambda (cell) (eq? (at-c cell) PLAYER)) maze))))


(define (find-exit maze)
  (at-pos (first (memf (lambda (cell) (eq? (at-c cell) EXIT)) maze))))


(define (find-neighbors maze posn)
  (let ([neighbor-coords (list
                          (make-posn (- (posn-x posn) 1) (posn-y posn))
                          (make-posn  (+ (posn-x posn) 1) (posn-y posn))
                          (make-posn  (posn-x posn) (+ 1 (posn-y posn)))
                          (make-posn (posn-x posn) (- 1 (posn-y posn))))])
    (filter (lambda (cell) (find-cell-at maze (posn-x cell) (posn-y cell))) neighbor-coords)))
                                
                             


(: find-path (-> Maze (List Posn) Posn Boolean))

(define (find-path maze visited curr)
  (if (member? curr visited) #f
      (if (equal? curr (find-exit maze))
          #t
          (ormap (lambda (neighbor) 
         (find-path maze (cons curr visited) neighbor))
       (find-neighbors maze curr)))))

(: path-exists? (-> Maze Boolean)) 
(define (path-exists? maze)
  (find-path maze '() (find-player maze)))

(define (random-maze-3 dimension)
  (let ([curr-maze (random-maze-2 dimension)])
    (if (path-exists? curr-maze)
        curr-maze
        (random-maze-3 dimension))))



(check-expect (path-exists? (list
 (make-at 'wall (make-posn 0 0))
 (make-at 'wall (make-posn 1 0))
 (make-at 'player (make-posn 2 0))
 (make-at 'exit (make-posn 0 1))
 (make-at 'wall (make-posn 1 1))
 (make-at 'empty (make-posn 2 1))
 (make-at 'empty (make-posn 0 2))
 (make-at 'empty (make-posn 1 2))
 (make-at 'empty (make-posn 2 2)))) #t)


;; Problem 8
(define (find-length-path maze visited curr)
  (if (member? curr visited) #f
      (if (equal? curr (find-exit maze))
          0
          (let* ([neighbor-paths
                  ;; finds the length of hte paths for all nieghbros
                (map (lambda (neighbor)
                       (find-length-path maze (cons curr visited) neighbor)) (find-neighbors maze curr))]
                 [valid-paths (filter number? neighbor-paths)])
            ;; filters out false valies
            ;; if empty list then #f othewrise returns min length of hte vaoid paths + 1 
                 (if (empty? valid-paths) #f (+ 1 (apply min valid-paths)))))))

(: path-length? (-> Maze (Maybe Natural)))
(define (path-length? maze)
  (if (empty? maze) #f
   (find-length-path maze '() (find-player maze))))

(check-contract path-length? 2)

(check-expect (path-length? (list
 (make-at 'wall (make-posn 0 0))
 (make-at 'wall (make-posn 1 0))
 (make-at 'player (make-posn 2 0))
 (make-at 'exit (make-posn 0 1))
 (make-at 'wall (make-posn 1 1))
 (make-at 'empty (make-posn 2 1))
 (make-at 'empty (make-posn 0 2))
 (make-at 'empty (make-posn 1 2))
 (make-at 'empty (make-posn 2 2)))) 3)


(define (random-maze-4 dimension)
  (let ([path (random-maze-3 dimension)])
    (if (>= (path-length? path) dimension) path (random-maze-4 dimension))))
    
        
  