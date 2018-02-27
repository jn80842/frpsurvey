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

(define (sketch-graph input1 input2 input3)
  (define r1 input1)
  (define r2 input2)
  (define r3 input3)
  (define r4 (call-stateless-stream-insn (list-ref holes 0) (list r1 r2 r3)))
  (define r5 (call-stateless-stream-insn (list-ref holes 1) (list r1 r2 r3 r4)))
  (define r6 (call-stream-insn (list-ref holes 2) (list r1 r2 r3 r4 r5)))
  (define r7 (call-stream-insn (list-ref holes 3) (list r1 r2 r3 r4 r5 r6)))
  (define r8 (call-stream-insn (list-ref holes 4) (list r1 r2 r3 r4 r5 r6 r7)))
  (define r9 (call-stream-insn (list-ref holes 5) (list r1 r2 r3 r4 r5 r6 r7 r8)))
  (list-ref (list r1 r2 r3 r4 r5 r6 r7 r8) retval-idx))

(define binding (time (synthesize #:forall (append (harvest s-mouse-up s-mouse-down) (harvest-coords-stream s-mouse-pos))
                                  #:guarantee (assert (same straightline-graph
                                                            sketch-graph
                                                            s-mouse-up s-mouse-down s-mouse-pos)))))

(if (unsat? binding)
    (displayln "synthesis model is unsat")
    (displayln "synthesis model is sat"));;(print-from-holes (evaluate holes binding) (evaluate retval-idx binding) 3))