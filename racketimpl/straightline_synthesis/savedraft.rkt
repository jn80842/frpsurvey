#lang rosette

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../straightline.rkt")
(require "../benchmarks/savedraft.rkt")

(current-bitwidth #f)

(define (straightline-graph textChangedE saveButtonE)
  (define r1 textChangedE)
  (define r2 saveButtonE)
  (define r3 (timerE 5 r1))
  (define r4 (constantE #f r1))
  (define r5 (mergeE r3 r4))
  (define r6 (mergeE r2 r5))
  (define r7 (filterRepeatsE r6))
  (define r8 (filterE (λ (e) e) r7))
  r8)

(define v-binding (verify (assert (same saveCommand-graph
                                        straightline-graph
                                        s-textChangedE
                                        s-saveButtonE))))
(if (unsat? v-binding)
    (displayln "verified example implementation and straightline program are equivalent")
    (displayln "can't verify that straightline program matches example implementation"))

(define holes (for/list ([i (range 6)])
                (get-insn-holes)))
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

(define binding (time (synthesize #:forall (harvest s-textChangedE s-saveButtonE)
                                  #:guarantee (assert (same straightline-graph
                                                            sketch-graph
                                                            s-textChangedE s-saveButtonE)))))
(if (unsat? binding)
    (displayln "synthesis model is unsat")
    (print-from-holes (evaluate holes binding) (evaluate retval-idx binding) 2))
                                        