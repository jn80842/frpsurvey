#lang rosette/safe

(require "../rosettefjapi.rkt")


;;;;; motion detector and porch light
(define (light-graph md-events)
  (mergeE (constantE md-events 'on) (constantE (calmE (delayE md-events 5) 5) 'off)))

(define concrete-motion (λ () (list (list 1 'd) (list 3 'd) (list 10 'd) (list 20 'd))))

(define s-motion (clicksE (list 1 2)))

(define sol (solve (assert (equal? (last (second (light-graph s-motion))) 'off))))

(define inc-clicks2 (clicksE (list 1 2 3)))
(define dec-clicks2 (clicksE (list 1 2 3)))

(define (first-always-0 inc dec)
  (assert (= (first (inc-dec-button-graph inc dec)) 0)))

(define sol2 (verify (first-always-0 inc-clicks2 dec-clicks2)))