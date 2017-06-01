#lang rosette/safe

(require "../rosettefjapi.rkt")

;;;;; mouse tail ;;;;

(define (timestamp-list concrete-list)
  (map (λ (x) (define-symbolic* timestamp integer?) timestamp) concrete-list))

(define (integer-events-for-timestamps timestamps)
  (λ () (map (λ (ts) (define-symbolic* value integer?) (list ts value)) timestamps)))

(define (integer-event-list concrete-list)
  (λ () (map (λ (c)
         (define-symbolic* timestamp integer?)
         (define-symbolic* value integer?)
         (list timestamp value)) concrete-list)))

(define (mouse-tail-graph mouse-x-event-stream mouse-y-event-stream)
  (let ([tail-x (mapE (λ (e) (list (first e) (+ (second e) 50))) (delayE mouse-x-event-stream 3))]
        [tail-y (delayE mouse-y-event-stream 3)])
    (list (tail-x) (tail-y))))

(define mouse-timestamps (timestamp-list (list 1 2 3)))
(define mouse-x (integer-events-for-timestamps mouse-timestamps))
(define mouse-y (integer-events-for-timestamps mouse-timestamps))
