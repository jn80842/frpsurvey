#lang rosette

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../straightline.rkt")
(require "../benchmarks/savedraft.rkt")

(current-bitwidth #f)

#;(define (saveCommand-graph textChangedE saveButtonE)
  (filterE (λ (e) e) (filterRepeatsE (mergeE saveButtonE
                                            (mergeE (timerE 5 textChangedE)
                                                    (constantE #f textChangedE))))))

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

(displayln "save draft benchmark")

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

(define state-mask (list->vector (list #t #f #f #f #t #f)))

(define (synth-graph state-mask)
  (time (synthesize #:forall (harvest s-textChangedE s-saveButtonE)
                    #:guarantee (assert (same straightline-graph
                                              (recursive-sketch holes retval-idx state-mask)
                                              s-textChangedE s-saveButtonE)))))

(define binding (synth-graph state-mask))

(if (unsat? binding)
    (displayln "synthesis model is unsat")
    (print-from-holes (evaluate holes binding) state-mask (evaluate retval-idx binding) 2))
                                        