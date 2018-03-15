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

(define (is-six-pm? t)
  (and (eq? (vector-ref t 0) 18)
       (eq? (vector-ref t 1) 0)
       (eq? (vector-ref t 2) 0)))
(define (is-sprinkler-time? t)
  (and (eq? (vector-ref t 0) 18)
       (<= (vector-ref t 1) 2)))

(define (rained-yesterday-graph clockE raingaugeB)
  (define r1 clockE)
  (define r2 raingaugeB)
  (define r3 (changes r2))
  (define r4 (filterE is-six-pm? r1))
  (define r5 (filterE (λ (e) e) r3))
  (define r6 (constantE #f r4))
  (define r7 (mergeE r5 r6))
  (define r8 (startsWith #f r7))
  r8)



(displayln "sprinklers benchmark")

#;(define v-binding (verify (assert (same sprinklers-graph
                                        straightline-graph
                                        s-clockB s-motionSensorB s-raingaugeB))))
#;(if (unsat? v-binding)
    (displayln "straightline program and implementation program are equal")
    (displayln "no"))

(define holes (for/list ([i (range 6)]) (get-insn-holes)))
(define-symbolic* retval-idx integer?)

(define state-mask (list->vector (list #t #f #f #f #f #t)))

(define (concrete-rained-yesterday-graph clockE raingaugeB)
  (define r1 clockE)
  (define r2 raingaugeB)
  (define r3 (call-stream-insn (vector-ref state-mask 0) (stream-insn 22 1 0 0 0) (list r1 r2)))
  (define r4 (call-stream-insn (vector-ref state-mask 1) (stream-insn 12 0 0 0 7) (list r1 r2 r3)))
  (define r5 (call-stream-insn (vector-ref state-mask 2) (stream-insn 12 2 0 0 9) (list r1 r2 r3 r4)))
  (define r6 (call-stream-insn (vector-ref state-mask 3) (stream-insn 1 5 0 0 3) (list r1 r2 r3 r4 r5)))
  (define r7 (call-stream-insn (vector-ref state-mask 4) (stream-insn 16 8 0 0 3) (list r1 r2 r3 r4 r5 r6)))
  (list-ref (list r1 r2 r3 r4 r5 r6 r7) 6))

(displayln "synthesize did it rain yesterday graph")

(define (synth-graph state-mask)
  (time (synthesize #:forall (harvest s-clockE s-raingaugeB)
                    #:guarantee (assert (same rained-yesterday-graph
                                              (recursive-sketch holes retval-idx state-mask)
                                              s-clockE s-raingaugeB)))))

(define binding (synth-graph state-mask))

(if (unsat? binding)
    (displayln "unsat")
(print-from-holes (evaluate holes binding)
                  state-mask
                  (evaluate retval-idx binding) 2))

(displayln "synthesize partial sprinkler graph")

(define (partial-sprinklers-graph clockE motionSensorB yesterdayB)
  (define r1 clockE)
  (define r2 motionSensorB)
  (define r3 yesterdayB)
  (define r4 (mapE is-sprinkler-time? r1))
  (define r5 (startsWith #f r4))
  (define r6 (notB r5))
  (define r7 (andB r6 r5))
  (define r8 (notB r2))
  (define r9 (andB r7 r8))
  r9)

(define partial-holes (for/list ([i (range 6)]) (get-insn-holes)))
(define partial-state-mask (list->vector (list #f #t #f #f #f #f)))

(define partial-synth-binding (time (synthesize #:forall (harvest s-clockE s-motionSensorB s-yesterdayRainB)
                                               #:guarantee (assert (same partial-sprinklers-graph
                                                                         (recursive-sketch partial-holes retval-idx partial-state-mask)
                                                                         s-clockE s-motionSensorB s-yesterdayRainB)))))
(if (unsat? partial-synth-binding)
    (displayln "unsat")
    (print-from-holes (evaluate partial-holes partial-synth-binding) partial-state-mask
                      (evaluate retval-idx partial-synth-binding) 3))

(displayln "synthesize full sprinkler graph")

(define (full-sprinklers-graph clockE motionSensorB raingaugeB)
  (define r1 clockE)
  (define r2 motionSensorB)
  (define r3 raingaugeB)
  (define r4 (changes r2))
  (define r5 (filterE is-six-pm? r1))
  (define r6 (filterE (λ (e) e) r4))
  (define r7 (constantE #f r5))
  (define r8 (mergeE r6 r7))
  (define r9 (startsWith #f r8))
  (define r10 (mapE is-sprinkler-time? r1))
  (define r11 (startsWith #f r10))
  (define r12 (notB r9))
  (define r13 (andB r11 r12))
  (define r14 (notB r2))
  (define r15 (andB r13 r14))
  r15)

(define full-holes (for/list ([i (range 12)]) (get-insn-holes)))
(define full-state-mask (list->vector (list #t #f #f #f #f #t #f #t #f #f #f #f)))

(define (concrete-full-sprinklers-graph clockE motionSensorB raingaugeB)
  (define r1 clockE)
  (define r2 motionSensorB)
  (define r3 raingaugeB)
  (define r4 (call-stream-insn (vector-ref full-state-mask 0) (stream-insn 22 1 0 0 0) (list r1 r2 r3)))
  (define r5 (call-stream-insn (vector-ref full-state-mask 1) (stream-insn 12 0 0 0 7) (list r1 r2 r3 r4)))
  (define r6 (call-stream-insn (vector-ref full-state-mask 2) (stream-insn 12 3 0 0 9) (list r1 r2 r3 r4 r5)))
  (define r7 (call-stream-insn (vector-ref full-state-mask 3) (stream-insn 1 5 0 0 3) (list r1 r2 r3 r4 r5 r6)))
  (define r8 (call-stream-insn (vector-ref full-state-mask 4) (stream-insn 2 7 6 0 0) (list r1 r2 r3 r4 r5 r6 r7)))
  (define r9 (call-stream-insn (vector-ref full-state-mask 5) (stream-insn 16 7 0 0 3) (list r1 r2 r3 r4 r5 r6 r7 r8)))
  (define r10 (call-stream-insn (vector-ref full-state-mask 6) (stream-insn 3 0 0 0 8) (list r1 r2 r3 r4 r5 r6 r7 r8 r9)))
  ;(define r11 (call-stream-insn (vector-ref full-state-mask 7) (stream-insn
  r1
)
#;(define (full-synth-graph full-state-mask)
  (time (synthesize #:forall (harvest s-clockE s-raingaugeB s-motionSensorB)
                    #:guarantee (assert (same full-sprinklers-graph
                                              (recursive-sketch full-holes retval-idx full-state-mask)
                                              s-clockE s-raingaugeB s-motionSensorB)))))
;(define full-binding (full-synth-graph full-state-mask))

#;(if (unsat? full-binding)
    (displayln "unsat")
    (print-from-holes (evaluate full-holes full-binding) full-state-mask
                      (evaluate retval-idx full-binding) 3))