#lang rosette

(provide (all-defined-out))

(define (is-empty? e)
  (eq? e 'no-evt))