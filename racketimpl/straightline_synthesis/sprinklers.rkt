#lang rosette

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../straightline.rkt")
(require "../benchmarks/sprinklers.rkt")

;(error-print-width 100000000000)

(current-bitwidth #f)

(define (straightline-graph clockB motionSensorB raingaugeB)
  (define r1 clockB)
  (define r2 motionSensorB)
  (define r3 raingaugeB)
  (define r4 (liftB2 (λ (rain clock) (if (is-midnight? clock) 'midnight rain)) r3 r1))
  (define r5 (collectB #f (λ (r prev) (if (eq? r 'midnight) #f
                                          (if r #t prev))) r4))
  (define r6 (constantB 'on r1))
  (define r7 (liftB2 (λ (rain clock) (and (not rain)
                                          (eq? (time-vec-hour clock) 18)
                                          (< (time-vec-min1 clock) 1))) r5 r1))
  (define r8 (constantB 'off r1))
  (define r9 (condB (list (list r2 r8)
                          (list r7 r6)
                          (list (constantB #t r1) r8))))
  r9)

(displayln "sprinklers benchmark")

(define v-binding (verify (assert (same sprinklers-graph
                                        straightline-graph
                                        s-clockB s-motionSensorB s-raingaugeB))))
(if (unsat? v-binding)
    (displayln "straightline program and implementation program are equal")
    (displayln "no"))

(define holes (for/list ([i (range 6)]) (get-insn-holes)))
(define-symbolic* retval-idx integer?)

(define state-mask (list->vector (list #f #t #f #f #f #f)))

(define (synth-graph state-mask)
  (time (synthesize #:forall (harvest s-clockB s-motionSensorB s-raingaugeB)
                    #:guarantee (assert (same straightline-graph
                                              (recursive-sketch holes retval-idx state-mask)
                                              s-clockB s-motionSensorB s-raingaugeB)))))

(define binding (synth-graph state-mask))

(if (unsat? binding)
    (displayln "unsat")
(print-from-holes (evaluate holes binding)
                  state-mask
                  (evaluate retval-idx binding) 3))