#lang rosette

(require "dense-fjmodels.rkt")

(provide (all-defined-out))

(define (type-event-stream-func type?)
  (λ (event-stream) (andmap (λ (e) (or (empty-event? e) (type? e))) event-stream)))

(define (integer-event-stream? event-stream)
  (type-event-stream-func integer?))
(define (boolean-event-stream? event-stream)
  (type-event-stream-func boolean?))

(define (get-struct-predicate item)
  (let-values ([(struct-type _) (struct-info item)])
    (struct-type-make-predicate struct-type)))

(define (get-type-predicate item)
  (cond [(struct? item) (get-struct-predicate item)]
        [(integer? item) integer?]
        [(boolean? item) boolean?]
        [(symbol? item) symbol?]
        [else (λ (i) #f)])) ;; if we don't know about the type, it's false

(define (monomorphic-event-stream? event-stream)
  (let ([actual-events (filter (λ (e) (not (empty-event? e))) event-stream)])
    (if (empty? actual-events)
        #t ;; if no events occur, trivially monomorphic
        (let* ([first-event (first actual-events)]
               [type-predicate (get-type-predicate first-event)])
          (andmap type-predicate actual-events)))))
