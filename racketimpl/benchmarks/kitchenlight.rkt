#lang rosette/safe
(require rosette/lib/synthax)

(require "../fjmodels.rkt")
(require "../rosettefjapi.rkt")

(provide (all-defined-out))

(define stream-length 6)

(define (sym-location)
  (define-symbolic* b boolean?)
  (if b 'home 'not-at-home))

(define s-motion-sensorB (new-behavior sym-boolean stream-length))
(define s-locationB (new-behavior sym-location stream-length))
(define s-clockB (new-behavior sym-time-vec stream-length))

(define (mode-assumptions clockB locationB)
  (and (valid-behavior? clockB)
       (valid-time-vec? (behavior-init clockB))
       (andmap (λ (e) (valid-time-vec? (get-value e))) (behavior-changes clockB))
       (valid-behavior? locationB)
       ))

(define (light-status-assumptions motionSensorB)
  (and (valid-behavior? motionSensorB)))

(define (light-color-assumptions clockB locationB motionSensorB)
  (and (mode-assumptions clockB locationB)
       (light-status-assumptions motionSensorB)))

