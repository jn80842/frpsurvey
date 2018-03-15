#lang rosette

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../straightline.rkt")
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

(define holes (for/list ([i (range 5)])
                          (get-insn-holes)))
(define-symbolic* retval-idx integer?)

(define state-mask (list->vector (list #f #f #f #t #t)))

(define (synth-graph state-mask)
  (time (synthesize #:forall (harvest s-inc s-dec)
                    #:guarantee (assert (same straightline-graph
                                              (recursive-sketch holes retval-idx state-mask)
                                              s-inc s-dec)))))

(define binding (synth-graph state-mask))

(if (unsat? binding)
    (displayln "synthesis model is unsat")
    (print-from-holes (evaluate holes binding) state-mask (evaluate retval-idx binding) 2))

(displayln "inc/dec benchmark via i/o examples")

(define (io-synth-graph state-mask)
  (time (synthesize #:forall (harvest s-inc s-dec)
                    #:guarantee (assert (equal? ((recursive-sketch holes retval-idx state-mask) io-spec-inc-clicks io-spec-dec-clicks) io-spec-output)))))

(define io-binding (io-synth-graph state-mask))

(if (unsat? io-binding)
    (displayln "spec synthesis model is unsat")
    (print-from-holes (evaluate holes io-binding) state-mask (evaluate retval-idx io-binding) 2))

(displayln "inc/dec benchmark via minimal i/o examples")

(define small-io-binding
  (time (synthesize #:forall (harvest s-inc s-dec)
                    #:guarantee (assert (equal? ((recursive-sketch holes retval-idx state-mask) '(no-evt) '(click)) (behavior 0 '(-1)))))))
(if (unsat? small-io-binding)
    (displayln "small io benchmark is unsat")
    (print-from-holes (evaluate holes small-io-binding) state-mask (evaluate retval-idx small-io-binding) 2))

(define small-io-binding2
  (time (synthesize #:forall (harvest s-inc s-dec)
                    #:guarantee (let ([f (recursive-sketch holes retval-idx state-mask)])
                                  (assert (and (equal? (f '(no-evt) '(click)) (behavior 0 '(-1)))
                                               (equal? (f '(click) '(no-evt)) (behavior 0 '(1)))))))))
(if (unsat? small-io-binding2)
    (displayln "small io benchmark2 is unsat")
    (print-from-holes (evaluate holes small-io-binding2) state-mask (evaluate retval-idx small-io-binding2) 2))

(define small-io-binding3
  (time (synthesize #:forall (harvest s-inc s-dec)
                    #:guarantee (let ([f (recursive-sketch holes retval-idx state-mask)])
                                  (assert (and (equal? (f '(no-evt) '(click)) (behavior 0 '(-1)))
                                               (equal? (f '(click) '(no-evt)) (behavior 0 '(1)))
                                               (equal? (f '(no-evt no-evt) '(click click)) (behavior 0 '(-1 -2)))))))))
(if (unsat? small-io-binding3)
    (displayln "small io benchmark3 is unsat")
    (print-from-holes (evaluate holes small-io-binding3) state-mask (evaluate retval-idx small-io-binding3) 2))
