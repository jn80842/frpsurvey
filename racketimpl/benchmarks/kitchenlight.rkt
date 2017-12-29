#lang rosette/safe
(require rosette/lib/synthax)

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")

(provide (all-defined-out))

;;            liftB
;;         /    |       \
;;       λ    liftB         liftB
;;        /     |          /  |   \
;;      λ  motionSensorB  λ clockB locationB

(current-bitwidth 5)
(define stream-length 2)

(define (sym-location)
  (define-symbolic* b boolean?)
  (if b 'home 'not-at-home))

(define concrete-motion-sensorB (behavior #f '(#t #f)))
(define concrete-clockB (behavior 0 '(1 2)))
(define concrete-location (behavior 'home '(not-at-home home)))

;; use fake hour constants for smaller bitwidths
(define hour-begin 4)
(define hour-end 2)

(define s-motion-sensorB (new-behavior sym-boolean stream-length))
(define s-locationB (new-behavior sym-location stream-length))
;(define s-clockB (new-behavior sym-time-vec stream-length))
(define s-clockB (new-behavior sym-integer stream-length))

#;(define (mode-assumptions clockB locationB)
  (and; (valid-behavior? clockB)
       (valid-time-vec? (behavior-init clockB))
       (andmap (λ (e) (valid-time-vec? e)) (behavior-changes clockB))
     ;  (valid-behavior? locationB)
       ))

#;(define (light-status-assumptions motionSensorB)
  #t) ;(and (valid-behavior? motionSensorB)))

#;(define (light-color-assumptions clockB locationB motionSensorB)
  (and (mode-assumptions clockB locationB)
       (light-status-assumptions motionSensorB)))

;; between 21:30 and 8:00 inclusive, mode is night
;; otherwise, if user is at home, home, else away
(define (mode-graph clockB userLocationB motionSensorB)
  (liftB2 (λ (clock location) (if (or (>= clock hour-begin) (< clock hour-end))
                                 'night
                                 (if (equal? location 'home)
                                     'home
                                     'away)))
         clockB userLocationB))

;; when motion sensor senses motion, light turns on
(define (kitchen-light-status-graph clockB userLocationB motionSensorB)
  (liftB1 (λ (e) (if e 'on 'off)) motionSensorB))

;; when light turns on, if mode is night, light is orange, else white
(define (kitchen-light-color-graph kitchenLightB modeB)
  (liftB2 (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none)) kitchenLightB modeB))

;; full graph
(define (kitchen-light-graph clockB userLocationB motionSensorB)
  (liftB2 (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))
         (liftB1 (λ (e) (if e 'on 'off)) motionSensorB)
         (liftB2 (λ (clock location) (if (or (>= clock hour-begin) (< clock hour-end))
                                 'night
                                 (if (equal? location 'home)
                                     'home
                                     'away)))
         clockB userLocationB)))
