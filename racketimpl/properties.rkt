#lang rosette

(require "dense-fjmodels.rkt")

(provide (all-defined-out))

(define (type-event-stream-func type?)
  (λ (event-stream) (andmap (λ (e) (or (empty-event? e) (type? e))) event-stream)))

(define (integer-event-stream? event-stream)
  (type-event-stream-func integer?))
(define (boolean-event-stream? event-stream)
  (type-event-stream-func boolean?))

