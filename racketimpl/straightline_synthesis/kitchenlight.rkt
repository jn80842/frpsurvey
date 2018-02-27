#lang rosette

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../straightline.rkt")
(require "../benchmarks/kitchenlight.rkt")

(current-bitwidth #f)

(define (straightline-kitchenlight-graph clock userLocation motionSensor)
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
                                        straightline-kitchenlight-graph
                                        s-clockB s-locationB s-motion-sensorB))))
(if (unsat? v-binding)
    (displayln "verified example implementation and straightline program are equivalent")
    (displayln "can't verify that straightline program matches example implementation"))

(define holes (for/list ([i (range 3)]) (get-insn-holes)))
(define-symbolic* retval-idx integer?)

(define (sketch-graph input1 input2 input3)
  (define r1 input1)
  (define r2 input2)
  (define r3 input3)
  (define r4 (call-stateless-stream-insn (list-ref holes 0) (list r1 r2 r3)))
  (define r5 (call-stateless-stream-insn (list-ref holes 1) (list r1 r2 r3 r4)))
  (define r6 (call-stateless-stream-insn (list-ref holes 2) (list r1 r2 r3 r4 r5)))
  (list-ref (list r1 r2 r3 r4 r5 r6) retval-idx))

(define binding (time (synthesize #:forall (harvest s-clockB s-locationB s-motion-sensorB)
                                  #:guarantee (assert (same straightline-kitchenlight-graph sketch-graph
                                                            s-clockB s-locationB s-motion-sensorB)))))

(if (unsat? binding)
    (displayln "unsat")
    (displayln "sat"))
    ;(print-from-holes (evaluate holes binding)
    ;                  (evaluate retval-idx binding) 3))