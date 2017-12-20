#lang rosette
(require rosette/lib/synthax)

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../straightline.rkt")
(require "../benchmarks/incdecbutton.rkt")

(define (straightline-graph inc dec)
  (define r1 inc)
  (define r2 dec)
  (define r3 (constantE 1 r1))
  (define r4 (constantE -1 r2))
  (define r5 (mergeE r3 r4))
  (define r6 (collectE 0 + r5))
  (define r7 (startsWith 0 r6))
  r7)

;; pass input streams here as args
(define (holes-based-synthesis depth)
  (define holes-structure (for/list ([i (range depth)])
                            (get-insn-holes)))
  ;; need to generate this 
  (define (sketch-graph input1 input2)
    (define r1 input1)
    (define r2 input2)
    (define r3 (single-insn (list-ref holes-structure 0) (list r1 r2)))
    (define r4 (single-insn (list-ref holes-structure 1) (list r1 r2 r3)))
    (define r5 (single-insn (list-ref holes-structure 2) (list r1 r2 r3 r4)))
    (define r6 (single-insn (list-ref holes-structure 3) (list r1 r2 r3 r4 r5)))
    (define r7 (single-insn (list-ref holes-structure 4) (list r1 r2 r3 r4 r5 r6)))
    r7)
 (define binding (time (synthesize #:forall (append (harvest s-inc) (harvest s-dec))
                              #:guarantee (assert (same inc-dec-button-graph
                                                        sketch-graph
                                                        s-inc s-dec)))))
  (if (unsat? binding)
      "unsat"
      (print-from-holes holes-structure binding depth)))
