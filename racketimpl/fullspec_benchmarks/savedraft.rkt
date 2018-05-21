#lang rosette

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../sketch.rkt")
(require "../operators.rkt")
(require "../specifications.rkt")
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

(displayln "save draft benchmark")

(define v-binding (time (verify (assert (same saveCommand-graph
                                        straightline-graph
                                        s-textChangedE
                                        s-saveButtonE)))))
(if (unsat? v-binding)
    (displayln "verified example implementation and straightline program are equivalent")
    (displayln "can't verify that straightline program matches example implementation"))

(define state-mask (list->vector (list #f #t #f #f #t #f)))

(define sym-inputs-list (list (sym-input "textChangedE" s-textChangedE)
                              (sym-input "saveButtonE" s-saveButtonE)))

(define sd-sketch (sketch (get-holes-list 6) state-mask (get-retval-idx)
                          stateless-operator-list stateful-operator-list 2))

(synth-from-ref-impl sd-sketch straightline-graph s-textChangedE s-saveButtonE)