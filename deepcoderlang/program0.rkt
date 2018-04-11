#lang rosette

(require "api.rkt")
(require "operators.rkt")
(require "sketch.rkt")
(require "specifications.rkt")

(define (straightline-graph int int-list)
  (define r1 int)
  (define r2 int-list)
  (define r3 (sort-dc r2))
  (define r4 (take-dc r1 r3))
  (define r5 (sum-dc r4))
  r5)

(define program0-sketch (sketch (get-holes-list 3) (get-retval-idx) 2))

(synth-from-ref-impl program0-sketch straightline-graph (get-sym-int) (sym-int-list 3))