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

(define holes (for/list ([i (range 3)])
                          (get-insn-holes)))
(define comm-insn-holes (for/list ([i (range 3)])
                          (get-comm-insn-holes)))

(define (sketch-graph input1 input2)
  (define r1 input1)
  (define r2 input2)
  (define r3 (call-fixed-comm-insn (list-ref comm-insn-holes 0) r1))
  (define r4 (call-fixed-comm-insn (list-ref comm-insn-holes 1) r2))
  (define r5 (call-fixed-stream-insn (list-ref holes 0) r3 r4))
  (define r6 (call-fixed-stream-insn (list-ref holes 1) r5))
  (define r7 (call-fixed-stream-insn (list-ref holes 2) r6))
  (define r8 (call-fixed-comm-insn (list-ref comm-insn-holes 2) r7))
  r8)

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
    (displayln (format "(define (synthesized-function input1 input2)\n  (define r1 input1)\n  (define r2 input2)\n~a\n~a\n~a\n~a\n~a\n~a\n  r8)"
                       (print-single-insn (list-ref (evaluate comm-insn-holes binding) 0) "r3" "r1")
                       (print-single-insn (list-ref (evaluate comm-insn-holes binding) 1) "r4" "r2")
                       (print-single-insn (list-ref (evaluate holes binding) 0) "r5" "r3" "r4")
                       (print-single-insn (list-ref (evaluate holes binding) 1) "r6" "r5")
                       (print-single-insn (list-ref (evaluate holes binding) 2) "r7" "r6")
                       (print-single-insn (list-ref (evaluate comm-insn-holes binding) 2) "r8" "r7"))))
