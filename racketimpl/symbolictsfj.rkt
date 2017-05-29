#lang rosette/safe

(require "rosettefjapi.rkt")

;; inputs and outputs are lists of pairs of timestamps and values
;; event streams consist of timestamp/value pairs when some event occurs (if no entry, no event is occurring
;; behaviors consist of timestamp/value pairs when some value *changes* (if no entry, behavior holds its previous value)

(define (clicksE concrete-list)
  (map (λ (c)
                  (define-symbolic* timestamp integer?)
                  (list timestamp 'click)) concrete-list))



(define (inc-dec-button-graph inc dec)
  (startsWith (collectE (mergeE (constantE inc 1) (constantE dec -1)) 0 +) 0))

(define concrete-inc-clicks (list (list 1 'click) (list 4 'click)))
(define concrete-dec-clicks (list (list 2 'click) (list 3 'click) (list 5 'click)))

(displayln (inc-dec-button-graph concrete-inc-clicks concrete-dec-clicks))

(define s-inc (clicksE (list 1)))
(define s-dec (clicksE (list 1)))

;(displayln (inc-dec-button-graph s-inc s-dec))

;;;;; mouse tail ;;;;

(define (timestamp-list concrete-list)
  (map (λ (x) (define-symbolic* timestamp integer?) timestamp) concrete-list))

(define (integer-events-for-timestamps timestamps)
  (map (λ (ts) (define-symbolic* value integer?) (list ts value)) timestamps))

(define (integer-event-list concrete-list)
  (map (λ (c)
         (define-symbolic* timestamp integer?)
         (define-symbolic* value integer?)
         (list timestamp value)) concrete-list))

(define (mouse-tail-graph mouse-x-event-stream mouse-y-event-stream)
  (let ([tail-x (mapE (λ (e) (+ e 50)) (delayE mouse-x-event-stream 3))]
        [tail-y (delayE mouse-y-event-stream 3)])
    (list tail-x tail-y)))

(define mouse-timestamps (timestamp-list (list 1 2 3)))
(define mouse-x (integer-events-for-timestamps mouse-timestamps))
(define mouse-y (integer-events-for-timestamps mouse-timestamps))

(displayln (mouse-tail-graph mouse-x mouse-y))

(define (unique-timestamps timestamps)
  (for-each ([ts timestamps])
(define mouse-tail-sol (solve (assert

;;;;;; drag and drop ;;;;;




;;;;; motion detector and porch light
(define (light-graph md-events)
  (mergeE (constantE md-events 'on) (constantE (calmE (delayE md-events 5) 5) 'off)))

(define concrete-motion (list (list 1 'd) (list 3 'd) (list 10 'd) (list 20 'd)))

(define s-motion (clicksE (list 1 2)))

(define sol (solve (assert (equal? (last (second (light-graph s-motion))) 'off))))

(define inc-clicks2 (clicksE (list 1 2 3)))
(define dec-clicks2 (clicksE (list 1 2 3)))

(define (first-always-0 inc dec)
  (assert (= (first (inc-dec-button-graph inc dec)) 0)))

(define sol2 (verify (first-always-0 inc-clicks2 dec-clicks2)))