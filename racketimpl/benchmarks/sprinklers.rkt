#lang rosette/safe

(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")

(provide (all-defined-out))

;;                              condB
;;            /                   |                      \
;;          pair                 pair                     pair
;;     /           \            /    \                   /    \
;;  motionSensorB  constantB  liftB   constantB    constantB    constantB
;;                     |       /    \      |            |            |
;;                    'off    λ  counterB  'on          #t          'off

(current-bitwidth 5)
(define stream-length 3)

(define s-raingaugeB (new-behavior sym-boolean stream-length (* 2 stream-length)))
(define s-clockB (new-behavior sym-integer stream-length (* 2 stream-length)))
(define s-motion-sensorB (new-behavior sym-boolean stream-length (* 2 stream-length))) ;; or, should this be event stream?