#lang rosette
(require rosette/lib/synthax)

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../straightline.rkt")
(require "../benchmarks/mousetail.rkt")

(define (straightline-mousetail-y-graph y-stream)
  (define r1 y-stream)
  (define r2 ((curry delayE time-delay) r1))
  r2)

(define (holes-mousetail-y-graph y-stream)
  (define r1 y-stream)
  (define r2-holes (stream-insn 9 0 0 0 3))
  (single-insn r2-holes (list r1)))

(define (straightline-mousetail-x-graph x-stream)
  (define r1 x-stream)
  (define r2 (delayE time-delay r1))
  (define r3 (mapE (λ (e) (+ e x-offset)) r2))
  r3)

(define y-sol (verify (assert (same straightline-mousetail-y-graph
                                    holes-mousetail-y-graph
                                    s-mouse-y))))

(assert (mousetail-assumptions s-mouse-x s-mouse-y))

(define (holes-based-synthesis x-depth y-depth)
  (define x-holes (for/list ([i (range x-depth)])
                    (get-insn-holes)))
  (define-symbolic* x-retval-idx integer?)
  (define y-holes (for/list ([i (range y-depth)])
                    (get-insn-holes)))
  (define-symbolic* y-retval-idx integer?)
  (define (x-sketch-graph input)
    (define r1 input)
    (define r2 (single-insn (list-ref x-holes 0) (list r1)))
    (define r3 (single-insn (list-ref x-holes 1) (list r1 r2)))
    (list-ref (list r1 r2 r3) x-retval-idx))
  (define (y-sketch-graph input)
    (define r1 input)
    (define r2 (single-insn (list-ref y-holes 0) (list r1)))
    (list-ref (list r1 r2) y-retval-idx))
  (define binding (time (synthesize #:forall (append (harvest s-mouse-x) (harvest s-mouse-y))
                              #:guarantee (assert (and (same mousetail-x-graph
                                                             x-sketch-graph s-mouse-x)
                                                       (same mousetail-y-graph
                                                             y-sketch-graph s-mouse-y))))))
  (if (unsat? binding)
      (displayln "unsat")
      (begin (print-from-holes x-holes x-retval-idx binding 2 1)
             (print-from-holes y-holes y-retval-idx binding 1 1))))
