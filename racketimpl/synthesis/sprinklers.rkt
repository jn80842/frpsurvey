#lang rosette/safe

(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")
(require "../benchmarks/sprinklers.rkt")
(require "grammar.rkt")

;;                              condB
;;            /                   |                      \
;;          pair                 pair                     pair
;;     /           \            /    \                   /    \
;;  motionSensorB  constantB  liftB   constantB    constantB    constantB
;;                     |       /    \      |            |            |
;;                    'off    λ  counterB  'on          #t          'off

(define (sprinklers-graph motionSensorB sprinkler-counterB)
  ;; mask times with sensor
  (condB (list (list motionSensorB (constantB 'off))
               (list (liftB (λ (t) (not (equal? t 0))) sprinkler-counterB) (constantB 'on))
               (list (constantB #t) (constantB 'off)))))

