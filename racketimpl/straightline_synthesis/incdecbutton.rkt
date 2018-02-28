#lang rosette

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../straightline.rkt")
(require "../benchmarks/incdecbutton.rkt")

(current-bitwidth #f)

(define (straightline-graph inc dec)
  (define r1 inc)
  (define r2 dec)
  (define r3 (constantE 1 r1))
  (define r4 (constantE -1 r2))
  (define r5 (mergeE r3 r4))
  (define r6 (collectE 0 + r5))
  (define r7 (startsWith 0 r6))
  r7)

(displayln "inc/dec button benchmark")

(define v-binding (verify (assert (same inc-dec-button-graph
                                        straightline-graph
                                        s-inc s-dec))))
(if (unsat? v-binding)
    (displayln "verified example implementation and straightline program are equivalent")
    (displayln "can't verify that straightline program matches example implementation"))

(define holes (for/list ([i (range 5)])
                          (get-insn-holes)))
(define-symbolic* retval-idx integer?)

(define state-mask (list #f #f #f #t #t))

(define (sketch-graph input1 input2)
  (define r1 input1)
  (define r2 input2)
  (define r3 (call-stream-insn (list-ref state-mask 0) (list-ref holes 0) (list r1 r2)))
  (define r4 (call-stream-insn (list-ref state-mask 1) (list-ref holes 1) (list r1 r2 r3)))
  (define r5 (call-stream-insn (list-ref state-mask 2) (list-ref holes 2) (list r1 r2 r3 r4)))
  (define r6 (call-stream-insn (list-ref state-mask 3) (list-ref holes 3) (list r1 r2 r3 r4 r5)))
  (define r7 (call-stream-insn (list-ref state-mask 4) (list-ref holes 4) (list r1 r2 r3 r4 r5 r6)))
  (list-ref (list r1 r2 r3 r4 r5 r6 r7) retval-idx))

(define binding (time (synthesize #:forall (harvest s-inc s-dec)
                                  #:guarantee (assert (same inc-dec-button-graph
                                                            sketch-graph
                                                            s-inc s-dec)))))
(if (unsat? binding)
    (displayln "synthesis model is unsat")
    (print-from-holes (evaluate holes binding) state-mask (evaluate retval-idx binding) 2))
