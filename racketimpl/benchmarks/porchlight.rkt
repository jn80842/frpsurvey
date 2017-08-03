#lang rosette/safe

(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")


;;;;; motion detector and porch light
(define (light-graph md-events)
  (mergeE (constantE md-events 'on) (constantE (calmE (delayE md-events 5) 5) 'off)))

(define concrete-motion (list (list 1 'd) (list 3 'd) (list 10 'd) (list 20 'd)))

