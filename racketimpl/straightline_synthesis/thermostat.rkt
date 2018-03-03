#lang rosette

;;(error-print-width 100000000000)

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../straightline.rkt")
(require "../benchmarks/thermostat.rkt")

(current-bitwidth #f)

(displayln "thermostat benchmark")

(define (straightline-graph tempB clockB)
  (define r1 tempB)
  (define r2 clockB)
  (define r3 (liftB1 (λ (t) (<= t 2)) r1))
  (define r4 (liftB1 (λ (c) (or (>= c 4) (>= 2 c))) r2))
  (define r5 (andB r3 r4))
  (define r6 (constantB 'on r1))
  (define r7 (constantB 'off r2))
  (define r8 (ifB r5 r6 r7))
  r8)

(define holes (for/list ([i (range 6)]) (get-insn-holes)))
(define-symbolic* retval-idx integer?)

(define state-mask (list->vector (list #f #f #f #f #f #f)))

(define (synth-graph state-mask)
  (time (synthesize #:forall (harvest s-tempB s-clockB)
                    #:guarantee (assert (same straightline-graph
                                              (recursive-sketch holes retval-idx state-mask)
                                              s-tempB s-clockB)))))

(define binding (synth-graph state-mask))

(if (unsat? binding)
    (displayln "unsat")
    (print-from-holes (evaluate holes binding)
                      state-mask
                      (evaluate retval-idx binding) 2))