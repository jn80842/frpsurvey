#lang rosette

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../straightline.rkt")
(require "../benchmarks/sprinklers.rkt")

(current-bitwidth 6)

(define (straightline-sprinklers-graph clockB motionSensorB raingaugeB)
  (define r1 clockB)
  (define r2 motionSensorB)
  (define r3 raingaugeB)
  (define r4 (liftB2 (λ (rain clock) (if (is-midnight? clock) 'midnight rain)) r3 r1))
  (define r5 (collectB #f (λ (r prev) (if (eq? r 'midnight) #f
                               (if r #t prev))) r4))
  (define r6 (constantB 'on))
  (define r7 (liftB2 (λ (rain clock) (and (not rain)
                                                  (eq? (time-vec-hour clock) 18)
                                                  (< (time-vec-min1 clock) 1))) r5 r1))
  (define r8 (constantB 'off))
  (define r9 (constantB #t))
  (define r10 (condB (list (list r2 r8)
                           (list r7 r6)
                           (list r9 r8))))
  r10)
(define hour-begin 4)
(define hour-end 2)

(define v-binding (verify (assert (same sprinklers-graph
                                        straightline-sprinklers-graph
                                        s-clockB s-motionSensorB s-raingaugeB))))
(if (unsat? v-binding)
    (displayln "straightline program and implementation program are equal")
    (displayln "no"))

(define holes (for/list ([i (range 7)]) (get-insn-holes)))
(define-symbolic* retval-idx integer?)

(define (sketch-graph input1 input2 input3)
  (define r1 input1)
  (define r2 input2)
  (define r3 input3)
  (define r4 (single-insn (list-ref holes 0) (list r1 r2 r3)))
  (define r5 (single-insn (list-ref holes 1) (list r1 r2 r3 r4)))
  (define r6 (single-insn (list-ref holes 2) (list r1 r2 r3 r4 r5)))
  (define r7 (single-insn (list-ref holes 3) (list r1 r2 r3 r4 r5 r6)))
  (define r8 (single-insn (list-ref holes 4) (list r1 r2 r3 r4 r5 r6 r7)))
  (define r9 (single-insn (list-ref holes 5) (list r1 r2 r3 r4 r5 r6 r7 r8)))
  (define r10 (single-insn (list-ref holes 6) (list r1 r2 r3 r4 r5 r6 r7 r8 r9)))
  (list-ref (list r1 r2 r3 r4 r5 r6 r7 r8 r9 r10) retval-idx))

(assert (and (>= (stream-insn-arg-index1 (list-ref holes 0)) 0)
             (< (stream-insn-arg-index1 (list-ref holes 0)) 3)
             (>= (stream-insn-arg-index1 (list-ref holes 1)) 0)
             (< (stream-insn-arg-index1 (list-ref holes 1)) 4)
             (>= (stream-insn-arg-index1 (list-ref holes 2)) 0)
             (< (stream-insn-arg-index1 (list-ref holes 2)) 5)
             (>= (stream-insn-arg-index1 (list-ref holes 3)) 0)
             (< (stream-insn-arg-index1 (list-ref holes 3)) 6)
             (>= (stream-insn-arg-index1 (list-ref holes 4)) 0)
             (< (stream-insn-arg-index1 (list-ref holes 4)) 7)
             (>= (stream-insn-arg-index1 (list-ref holes 5)) 0)
             (< (stream-insn-arg-index1 (list-ref holes 5)) 8)
             (>= (stream-insn-arg-index1 (list-ref holes 6)) 0)
             (< (stream-insn-arg-index1 (list-ref holes 6)) 9)
             (>= retval-idx 0)
             (< retval-idx 10)))

(define binding (time (synthesize #:forall (harvest s-clockB s-motionSensorB s-raingaugeB)
                                  #:guarantee (same straightline-sprinklers-graph sketch-graph
                                                    s-clockB s-motionSensorB s-raingaugeB))))

(if (unsat? binding)
    (displayln "unsat")
    (print-from-holes holes retval-idx binding 7 3))

