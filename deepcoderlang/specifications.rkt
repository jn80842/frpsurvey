#lang rosette

(provide (all-defined-out))

(define (same func1 func2 . inputs)
  (equal? (apply func1 inputs)
          (apply func2 inputs)))

(define (harvest . x)
  (flatten x))

(define (get-sym-int)
  (define-symbolic* i integer?)
  i)

(define (sym-int-list size)
  (for/list ([i (range size)])
    (get-sym-int)))
