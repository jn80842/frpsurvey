#lang rosette

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../operators.rkt")
(require "../sketch.rkt")
(require "../specifications.rkt")
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

(define state-mask (list->vector (list #f #f #f #t #t)))
(define idb-sketch (sketch (get-holes-list 5) state-mask
                           stateless-operator-list stateful-operator-list 2))
(define sym-input-list (list (sym-input "inc-clicks" s-inc)
                             (sym-input "dec-clicks" s-dec)))

(synth-from-ref-impl idb-sketch straightline-graph s-inc s-dec)