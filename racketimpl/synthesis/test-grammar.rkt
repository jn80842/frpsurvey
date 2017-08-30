#lang rosette/safe

(require rosette/lib/synthax)
(require rackunit)
(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")
(require "grammar.rkt")

(define (synthesize-graph spec-graph input)
  (define (synth-graph input)
    (flapjax-grmr input 1))
  (synthesize #:forall (harvest input)
              #:guarantee (same spec-graph synth-graph input)))

(define s-integer-evts (new-event-stream sym-integer 3))
(define s-integer-behavior (new-behavior sym-integer 3))

(define (zeroE-graph input)
  (zeroE))

(check-true (not (unsat? (synthesize-graph zeroE-graph s-integer-evts))))

(define (constantB-graph input)
  (constantB 1))

(check-true (not (unsat? (synthesize-graph constantB-graph s-integer-behavior))))

(define (notE-graph input)
  (notE input))

(check-true (not (unsat? (synthesize-graph notE-graph s-integer-evts))))
