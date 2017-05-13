#lang racket
(define in (open-input-file "vacuum-cleaner.rle"))
(define parsed "")
(define out (open-output-file "vacuum-cleaner.txt"))
(define (converted x)
  
(define (pars)
  (define x (read-line in))
  (when (not (eof-object? x))
    (write (converted x) out)
    (pars)))
(pars)
(close-output-port out)
