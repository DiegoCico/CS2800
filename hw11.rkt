#lang lsl

;; part p0
(define-mutable-struct cell (free? value))
(define-contract Cell (Struct cell [Boolean Any]))

(define-mutable-struct memory (pos cells))
(define-contract Memory (Struct memory [Natural (List Cell)]))

(define MEMORY
  (make-memory 0 (build-list 100 (lambda (_) (make-cell #t 0)))))

; helper provided in case you need to know size
(define (memorysize)
  (length (memory-cells MEMORY)))
;; part p0

;; part p0a
(: with-memory (-> Memory (-> Any) Any))
(define (with-memory M thnk)
  (let ((old-memory MEMORY))
    (begin
      (set! MEMORY M)
      (let ((result (thnk)))
        (begin
          (set! MEMORY old-memory)
          result)))))



;; part p0a

;; Problem 1

;; part p1

;; scans cells starting at index i and allocates the first free cell found.
(: scan-free (-> Natural (Maybe Cell)))
(define (scan-free i)
  (if (>= i (length (memory-cells MEMORY)))
      #f
      (if (cell-free? (list-ref (memory-cells MEMORY) i))
          (begin
            (set-cell-free?! (list-ref (memory-cells MEMORY) i) #f)
            (set-memory-pos! MEMORY (+ i 1))
            (list-ref (memory-cells MEMORY) i))
          (scan-free (+ i 1)))))

(: malloc (-> (Maybe Cell)))
(define (malloc)
  (let* ((pos (memory-pos MEMORY))
         (cells (memory-cells MEMORY))
         (size (length cells)))
    (if (< pos size)
        (begin
          (set-cell-free?! (list-ref cells pos) #f)
          (set-memory-pos! MEMORY (+ pos 1))
          (list-ref cells pos))
        (scan-free 0))))


;; -----------
;; Testing
(define (test1)
  (let ((c (malloc)))
    (list (cell-free? c) (memory-pos MEMORY) (cell-value c))))

(define (test2)
  (begin
    (malloc)
    (malloc)
    (malloc))) 

(check-expect
 (with-memory (make-memory 0 (list (make-cell #t 'a)
                                    (make-cell #t 'b)
                                    (make-cell #t 'c)))
   test1)
 (list #f 1 'a))

(check-expect
 (with-memory (make-memory 0 (list (make-cell #t 'x)
                                    (make-cell #t 'y)))
   test2)
 #f)

;; Problem 2

;; part p2
(: trim-pointer (-> Natural Natural))
(define (trim-pointer pos)
  (if (and (> pos 0)
           (cell-free? (list-ref (memory-cells MEMORY) (- pos 1))))
      (trim-pointer (- pos 1))
      pos))

(: free (-> Cell False))
(define (free c)
  (begin
    (set-cell-free?! c #t)
    (set-memory-pos! MEMORY (trim-pointer (memory-pos MEMORY)))
    #f))


;; part p2

;; Problem 3

;; part p3
(: defrag (-> False))
(define (defrag)
  (begin
    (let* ((cells (memory-cells MEMORY))
           (used (filter (lambda (c) (not (cell-free? c))) cells))
           (free-cells (filter cell-free? cells))
           (new-cells (append used free-cells)))
      (begin
        (set-memory-cells! MEMORY new-cells)
        (set-memory-pos! MEMORY (length used))))
    #f))
