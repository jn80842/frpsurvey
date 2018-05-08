#lang rosette

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../sketch.rkt")
(require "../operators.rkt")
(require "../specifications.rkt")
(require "../benchmarks/kitchenlight.rkt")

(current-bitwidth #f)

(define location-home 0)
(define location-work 1)
(define location-away 2)
(define location-night 3)

(define light-on-white 0)
(define light-on-orange 1)
(define light-off 2)

(define (mode-full-graph clockB userLocationB)
  (define r1 clockB)
  (define r2 userLocationB)
  (define r3 (liftB1 (λ (c) (or (>= c 18) (<= c 8))) r1))
  (define r4 (liftB1 (λ (l) (= l location-home)) r2))
  (define r5 (constantB location-home userLocationB))
  (define r6 (constantB location-away userLocationB))
  (define r7 (constantB location-night userLocationB))
  (define r8 (ifB r4 r5 r6))
  (define r9 (ifB r3 r7 r8))
  r9)

(define mode-state-mask (make-vector 7 #f))

(define mode-sketch (sketch (get-holes-list 7) mode-state-mask
                            stateless-operator-list stateful-operator-list 2))

;; 22s
(displayln "synthesize mode function")
(synth-from-ref-impl mode-sketch mode-full-graph s-clockB s-locationB)

(define (kitchen-light-straightline-graph mode motionSensor)
  (define r1 mode)
  (define r2 motionSensor)
  (define r3 (liftB1 (λ (m) (= m location-night)) r1))
  (define r4 (constantB light-on-orange r1))
  (define r5 (constantB light-on-white r1))
  (define r6 (ifB r3 r4 r5))
  (define r7 (constantB light-off r1))
  (define r8 (ifB r2 r6 r7))
  r8)

(define light-state-mask (make-vector 3 #f))
(define light-holes (for/list ([i (range 3)]) (get-insn-holes)))

#;(define v-binding (verify (assert (same kitchen-light-graph
                                        straightline-graph
                                        s-clockB s-locationB s-motion-sensorB))))
#;(if (unsat? v-binding)
    (displayln "verified example implementation and straightline program are equivalent")
    (displayln "can't verify that straightline program matches example implementation"))

(define holes (for/list ([i (range 7)]) (get-insn-holes)))

(define state-mask (list->vector (list #f #f #f #f #f #f #f)))

;; 159s
(displayln "synthesize kitchen light given mode")
(synth-from-ref-impl mode-sketch kitchen-light-straightline-graph s-modeB s-motion-sensorB)