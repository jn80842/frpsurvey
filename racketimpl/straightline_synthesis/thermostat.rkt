#lang rosette
(require rosette/lib/synthax)

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../straightline.rkt")
(require "../benchmarks/thermostat.rkt")

(define (straightline-thermostat-graph tempB clockB)
  (define r1 tempB)
  (define r2 clockB)
  (define r3 (liftB1 (λ (t) (<= t 2)) r1))
  (define r4 (liftB1 (λ (c) (or (>= c 4) (>= 2 c))) r2))
  (define r5 (andB r3 r4))
  (define r6 (constantB 'on '()))
  (define r7 (constantB 'off '()))
  (define r8 (ifB r5 r6 r7))
  r8)

(define v-binding (verify (assert (same straightline-thermostat-graph
                      thermostat-graph
                      s-tempB s-clockB))))
(if (unsat? v-binding)
    (displayln "verified example implementation and straightline program are equivalent")
    (displayln "can't verify that straightline program matches example implementation"))

(define holes (for/list ([i (range 6)]) (get-insn-holes)))
(define-symbolic* retval-idx integer?)

(define (sketch-graph input1 input2)
  (define r1 input1)
  (define r2 input2)
  (define r3 (call-stream-insn (list-ref holes 0) (list r1 r2)))
  (define r4 (call-stream-insn (list-ref holes 1) (list r1 r2 r3)))
  (define r5 (call-stream-insn (list-ref holes 2) (list r1 r2 r3 r4)))
  (define r6 (call-stream-insn (list-ref holes 3) (list r1 r2 r3 r4 r5)))
  (define r7 (call-stream-insn (list-ref holes 4) (list r1 r2 r3 r4 r5 r6)))
  (define r8 (call-stream-insn (list-ref holes 5) (list r1 r2 r3 r4 r5 r6 r7)))
  (list-ref (list r1 r2 r3 r4 r5 r6 r7 r8) retval-idx))

(define binding (time (synthesize #:forall (harvest s-tempB s-clockB)
                                  #:guarantee (assert (same straightline-thermostat-graph
                                                            sketch-graph
                                                            s-tempB s-clockB)))))

(if (unsat? binding)
    (displayln "unsat")
    (print-from-holes holes retval-idx binding 6 2))