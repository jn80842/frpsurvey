#lang rosette

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../straightline.rkt")
(require "../benchmarks/draganddrop.rkt")

(current-bitwidth #f)

(define (straightline-graph mouse-up mouse-down mouse-pos)
  (define r1 mouse-up)
  (define r2 mouse-down)
  (define r3 mouse-pos)
  (define r4 (constantE #f r1))
  (define r5 (constantE #t r2))
  (define r6 (mergeE r4 r5))
  (define r7 (startsWith #f r6))
  (define r8 (snapshotE r3 r7))
  ;; this is bitmask -- could be its own operator
  (define r9 (mapE2 (λ (dragging pos) (if dragging pos 'no-evt)) r8 r3))
  r9)

(displayln "drag and drop benchmark")

(define v-binding (verify (assert (same drag-and-drop-graph
                                        straightline-graph
                                        s-mouse-up s-mouse-down s-mouse-pos))))

(if (unsat? v-binding)
    (displayln "verified example implementation and straightline program are equivalent")
    (displayln "can't verify that straightline program matches example implementation"))

(define holes (for/list ([i (range 6)])
                (get-insn-holes)))
(define-symbolic* retval-idx integer?)

(define concrete-state-mask (list->vector (list #f #f #f #t #f #f)))
(define concrete-holes (list
                        (stream-insn 1 0 0 0 3)
                        (stream-insn 1 1 0 0 2)
                        (stream-insn 2 4 3 0 0)
                        (stream-insn 16 5 0 0 3)
                        (stream-insn 10 2 6 0 0)
                        (stream-insn 11 7 6 2 0)))
(define concrete-retval-idx 8)

(define (synth-graph state-mask)
  (time (synthesize #:forall (harvest s-mouse-up s-mouse-down s-mouse-pos)
                    #:guarantee (assert (same straightline-graph
                                              (recursive-sketch holes retval-idx state-mask)
                                              s-mouse-up s-mouse-down s-mouse-pos)))))

(define binding (synth-graph concrete-state-mask))
(if (unsat? binding)
    (displayln "no solution for sketch")
    (print-from-holes (evaluate holes binding) concrete-state-mask
                      (evaluate retval-idx binding) 3))