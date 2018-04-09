#lang rosette

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../sketch.rkt")
(require "../operators.rkt")
(require "../specifications.rkt")
(require "../benchmarks/kitchenlight.rkt")

(current-bitwidth #f)

#;(define (straightline-graph clock userLocation motionSensor)
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

(define location-home 0)
(define location-work 1)
(define location-away 2)
(define location-night 3)

(define light-on-white 0)
(define light-on-orange 1)
(define light-off 2)

(define (mode-straightline-graph clock userLocation constant-home constant-away constant-night)
  (define r1 clock)
  (define r2 userLocation)
  (define r3 (liftB1 (λ (c) (or (>= c 18) (<= c 8))) r1))
  (define r4 (liftB1 (λ (l) (= l location-home)) r2))
  (define r5 constant-home)
  (define r6 constant-away)
  (define r7 (ifB r4 r5 r6))
  (define r8 constant-night)
  (define r9 (ifB r3 r8 r7))
  r9)

(define mode-state-mask (make-vector 4 #f))

(define mode-sketch (sketch (get-holes-list 4) mode-state-mask (get-retval-idx)
                            stateless-operator-list stateful-operator-list 5))

(synth-from-ref-impl mode-sketch mode-straightline-graph s-clockB s-locationB
                     (constantB location-home dummyB) (constantB location-away dummyB)
                     (constantB location-night dummyB))

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

#;(define (kitchen-light-sketch mode motionSensor)
  (define r1 mode)
  (define r2 motionSensor)
  (define r3 (constantB light-on-orange r1))
  (define r4 (constantB light-on-white r1))
  (define r5 (constantB light-off r1))
  (define r6 (call-stream-insn (vector-ref light-state-mask 0) (list-ref light-holes 0) (list r1 r2 r3 r4 r5)))
  (define r7 (call-stream-insn (vector-ref light-state-mask 1) (list-ref light-holes 1) (list r1 r2 r3 r4 r5 r6)))
  (define r8 (call-stream-insn (vector-ref light-state-mask 2) (list-ref light-holes 2) (list r1 r2 r3 r4 r5 r6 r7)))
  (list-ref (list r1 r2 r3 r4 r5 r6 r7 r8) retval-idx))

(displayln "kitchen light")

#;(define v-binding (verify (assert (same kitchen-light-graph
                                        straightline-graph
                                        s-clockB s-locationB s-motion-sensorB))))
#;(if (unsat? v-binding)
    (displayln "verified example implementation and straightline program are equivalent")
    (displayln "can't verify that straightline program matches example implementation"))


(define holes (for/list ([i (range 7)]) (get-insn-holes)))


(define state-mask (list->vector (list #f #f #f #f #f #f #f)))

#;(define (synth-graph state-mask)
  (time (synthesize #:forall (harvest s-clockB s-locationB)
                    #:guarantee (assert (same mode-straightline-graph
                                              (recursive-sketch holes retval-idx state-mask)
                                              s-clockB s-locationB)))))

;(define binding (synth-graph state-mask))

#;(if (unsat? binding)
    (displayln "unsat")
    (print-from-holes (evaluate holes binding)
                      state-mask
                      (evaluate retval-idx binding) 7))