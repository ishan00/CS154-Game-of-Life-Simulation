#lang racket
(provide (struct-out point) square-cell)
(provide is-member? belongs?)
(provide make-2d-vector 2d-vector-ref 2d-vector-set! update-board list2->2vector 2vector->list2 default-law square-to-hex hex-to-square update-hex-board)
(struct point(i j state) #:transparent)

(define square-cell #t)

(define-syntax for
  (syntax-rules (:)
    [(for init : condition : step : statements ...)
     (begin
       init
       (define (iter)
         (cond [condition (begin statements ... step (iter))]))
       (iter))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2D Vector ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-2d-vector r c init)
  (build-vector r  	 
                (lambda (x) (make-vector c init))))

(define (2d-vector-ref vec r c)
  (vector-ref (vector-ref vec r) c))

(define (2d-vector-set! vec r c val)
  (let ((v (vector-ref vec r)))
    (begin
      (vector-set! v c val)
      (vector-set! vec r v))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  List Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (before l i)
  (if (= i 0) '()
      (cons (car l) (before (cdr l) (- i 1)))))

(define (after l i)
  (if (= i 0) (cdr l)
      (after (cdr l) (- i 1))))

(define (update-list l i x)
  (append (before l i) (list x) (after l i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-law
  (make-2d-vector 2 9 0))
(2d-vector-set! default-law 0 3 1)
(2d-vector-set! default-law 1 3 1)
(2d-vector-set! default-law 1 2 1)

(define (conway-rules neighbors alive? law)
  (if alive? (= (2d-vector-ref law 1 neighbors) 1)
      (= (2d-vector-ref law 0 neighbors) 1)))
(define (neighbors-rect location)
  (let ([x (first location)]
        [y (last location)])
    (if square-cell
        (for*/list ([dx '(-1 0 1)]
                    [dy '(-1 0 1)]
                    #:when (not (= dx dy 0)))
          (list (+ x dx) (+ y dy)))
        (cond ((even? x)
               (list (list (- x 2) y)
              (list (+ x 2) y)
              (list (- x 1) y)
              (list (- x 1) (- y 1))
              (list (+ x 1) (- y 1))
              (list (+ x 1) y)))
              (else
               (list (list (- x 2) y)
              (list (+ x 2) y)
              (list (- x 1) y)
              (list (- x 1) (+ y 1))
              (list (+ x 1) (+ y 1))
              (list (+ x 1) y)))))))
(define (neighbour-list l) (append-map neighbors-rect l))

;;;(set->list live-cells)))
(define (count-occurrences neighbors)
  (define ht (make-hash))
  (for/fold ([i 0])
            ([item neighbors])
    (hash-update! ht item add1 0))
  ht)

(define (update-board lis law)
  (let ([num-neighbors
         (count-occurrences (append* (map (lambda(x)(append* (map (lambda(y)(neighbors-rect (list x y))) (hash-ref lis x)))) (hash-keys lis))))])
    
    (list->hash (filter (λ(x) (conway-rules (hash-ref num-neighbors x)
                                            (is-member? x lis)
                                            law)) (hash-keys num-neighbors)))))

(define (list->hash lpairs)
  (define ht (make-hash))
  (map (lambda(pair)(let* ((a (hash-ref ht (car pair) (lambda()(begin (hash-set! ht (car pair) '())
                                                                      (hash-ref ht (car pair))))))
                           (new-list (cons (cadr pair) a)))
                      (hash-set! ht (car pair) new-list)))
       lpairs)
  ht)

(define (is-member? x hashed_table)
  (belongs? (cadr x) (hash-ref hashed_table (car x) (lambda()(begin (hash-set! hashed_table (car x) '())
                                                                    (hash-ref hashed_table (car x)))))))

(define (belongs? x l)
  (define f
    (λ (a y) (if (equal? a x) #t y))) 
  (foldr f #f l))

(define (update-hex-board board law)
  (let* ([neighbour1 (neighbour-hex board)]
     	[height (vector-length board)]
    	[width (vector-length (vector-ref board 0))]
    	[new-board (build-vector height (lambda (m) (build-vector width (λ(n) (point m n 0)))))])
	(for (define i 0) : (< i height) : (set! i (+ i 1)) :
  	(for (define j 0): (< j width) : (set! j (+ j 1)):
    	(2d-vector-set! new-board i j (point i j(2d-vector-ref law (point-state
 (2d-vector-ref board i j)) (point-state (2d-vector-ref neighbour1 i j))))))) new-board))

(define (neighbour-hex board)
  (define height (vector-length board))
  (define width (vector-length (vector-ref board 0)))
  (define new-matrix (build-vector height (lambda (m) (build-vector width (λ(n) (point m n 0))))))
  (define (value a b)
    (if (or (< a 0) (>= a height)) 0
        (if (or (< b 0) (>= b width)) 0
            (point-state (2d-vector-ref board a b)))))
  (define (neighbour-calc i j)
    (if (even? i)
        (+ (value (- i 2) j) (value (+ i 2) j) (value (- i 1) j) (value (- i 1) (- j 1))
           (value (+ i 1) j) (value (+ i 1) (- j 1)))
        (+ (value (- i 2) j) (value (+ i 2) j) (value (- i 1) j) (value (- i 1) (+ 1 j))
           (value (+ i 1) j) (value (+ i 1) (+ j 1)))))   

  (define (neighbour-help i j)
    (2d-vector-set! new-matrix i j (point i j (neighbour-calc i j))))
  (for (define i 0): (< i height) : (set! i (+ i 1)) : (for (define j 0) : (< j width) : (set! j (+ j 1)) : (neighbour-help i j)))
  new-matrix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (square-to-hex)
  (begin (set! square-cell #f)
         ;(set! default-law (vector (vector 0 0 0 1 0 0 0) (vector 0 0 1 1 0 0 0))
         ))
(define (hex-to-square)
  (begin
    (set! default-law (vector (vector 0 0 0 1 0 0 0 0 0) (vector 0 0 1 1 0 0 0 0 0)))
    (set! square-cell #t)))

(define (list2->2vector l)
  (let* ([h (length l)]
         [w (length (cdr l))]
         [Return (make-2d-vector h w 0)])
    (for (define i 0): (< i h):(begin (set! i (+ i 1)) (set! l (cdr l))) : (vector-set! Return i (list->vector (car l))))
    Return))

(define (2vector->list2 v)
  (let* ([h (vector-length v)]
         [w (vector-length (vector-ref v 0))]
         [Return '()])
    (for (define i (- h 1)): (>= i 0):(set! i (- i 1)) : (set! Return (cons (vector->list (vector-ref v i)) Return)))
    Return))

(define (display-rule law)
  (define (display-rule-helper init lis ans)
  (if (null? lis) (if (= (string-length ans) 0) init (string-append init (substring ans 1)))
      (display-rule-helper init (cdr lis) (string-append ans "," (number->string (car lis))))))
  (string-append (display-rule-helper "B" (filter (lambda(x)(= (vector-ref (vector-ref law 0) x) 1)) (build-list (vector-length (vector-ref law 0)) values)) "") "/"
                 (display-rule-helper "S" (filter (lambda(x)(= (vector-ref (vector-ref law 1) x) 1)) (build-list (vector-length (vector-ref law 1)) values)) "")))

;(define (parse-rule rule)
;  (define len (if square-cell 9 7))
;  (define 