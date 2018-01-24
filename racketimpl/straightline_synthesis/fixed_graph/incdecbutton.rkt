#lang rosette

(require "../../dense-fjmodels.rkt")
(require "../../densefjapi.rkt")
(require "straightline.rkt")
(require "../../benchmarks/incdecbutton.rkt")

(define (straightline-graph inc dec)
  (define r1 inc)
  (define r2 dec)
  (define r3 (constantE 1 r1))
  (define r4 (constantE -1 r2))
  (define r5 (mergeE r3 r4))
  (define r6 (collectE 0 + r5))
  (define r7 (startsWith 0 r6))
  r7)

(define v-binding (verify (assert (same inc-dec-button-graph
                                        straightline-graph
                                        s-inc s-dec))))
(if (unsat? v-binding)
    (displayln "verified example implementation and straightline program are equivalent")
    (displayln "can't verify that straightline program matches example implementation"))

(define holes (for/list ([i (range 5)])
                          (get-insn-holes)))

(define (sketch-graph input1 input2)
  (define r1 input1)
  (define r2 input2)
  (define r3 (call-full-fixed-stream-insn (list-ref holes 0) r1))
  (define r4 (call-full-fixed-stream-insn (list-ref holes 1) r2))
  (define r5 (call-fixed-stream-insn (list-ref holes 2) r3 r4))
  (define r6 (call-fixed-stream-insn (list-ref holes 3) r5))
  (define r7 (call-full-fixed-stream-insn (list-ref holes 4) r6))
  r7)

(define input-names (list (list "r1")
                          (list "r2")
                          (list "r3" "r4")
                          (list "r5")
                          (list "r6")))

(define binding (time (synthesize #:forall (harvest s-inc s-dec)
                                  #:guarantee (assert (same inc-dec-button-graph
                                                            sketch-graph
                                                            s-inc s-dec)))))
(if (unsat? binding)
    (displayln "synthesis model is unsat")
    (print-from-holes (evaluate holes binding) input-names 2))
