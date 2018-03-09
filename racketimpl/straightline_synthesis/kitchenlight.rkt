#lang rosette

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../straightline.rkt")
(require "../benchmarks/kitchenlight.rkt")

(current-bitwidth #f)

(define (straightline-graph clock userLocation motionSensor)
  (define r1 clock)
  (define r2 userLocation)
  (define r3 motionSensor)
  (define r4 (liftB2 (λ (clock location) (if (or (>= clock 4) (< clock 2))
                                 'night
                                 (if (equal? location 'home)
                                     'home
                                     'away))) r1 r2))
  (define r5 (liftB1 (λ (e) (if e 'on 'off)) r3))
  (define r6 (liftB2 (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none)) r5 r4)) r6)

(displayln "kitchen light")

(define v-binding (verify (assert (same kitchen-light-graph
                                        straightline-graph
                                        s-clockB s-locationB s-motion-sensorB))))
(if (unsat? v-binding)
    (displayln "verified example implementation and straightline program are equivalent")
    (displayln "can't verify that straightline program matches example implementation"))

(define holes (for/list ([i (range 3)]) (get-insn-holes)))
(define-symbolic* retval-idx integer?)

(define state-mask (list->vector (list #f #f #f)))

(define (synth-graph state-mask)
  (time (synthesize #:forall (harvest s-clockB s-locationB s-motion-sensorB)
                    #:guarantee (assert (same straightline-graph
                                              (recursive-sketch holes retval-idx state-mask)
                                              s-clockB s-locationB s-motion-sensorB)))))

(define binding (synth-graph state-mask))

(if (unsat? binding)
    (displayln "unsat")
    (print-from-holes (evaluate holes binding)
                      state-mask
                      (evaluate retval-idx binding) 3))