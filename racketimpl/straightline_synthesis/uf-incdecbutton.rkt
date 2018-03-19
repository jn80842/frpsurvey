#lang rosette

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../straightline.rkt")
(require "../benchmarks/incdecbutton.rkt")

(current-bitwidth #f)

(define-symbolic* f (~> integer? integer?))
(define-symbolic* f2 (~> integer? integer? integer?))

(define (straightline-graph inc dec)
  (define r1 inc)
  (define r2 dec)
  (define r3 (constantE 1 r1))
  (define r4 (constantE -1 r2))
  (define r5 (mergeE r3 r4))
  (define r6 (collectE 0 + r5))
  (define r7 (startsWith 0 r6))
  r7)

(define (uf-straightline-graph inc dec)
  (define r1 inc)
  (define r2 dec)
  (define r3 (constantE 1 r1))
  (define r4 (constantE -1 r2))
  (define r5 (mergeE r3 r4))
  (define r6 (collectE 0 f2 r5))
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

;(struct uf-insn (op-index idx int) #:transparent)

#;(define (call-uf-insn hole inputs)
  (if (= (uf-insn-op-index hole) 0)
      (mapE f (list-ref inputs (uf-insn-idx hole)))
      (collectE (uf-insn-int hole) f (list-ref inputs (uf-insn-idx hole)))))

;(define-symbolic* op integer?)
;(define-symbolic* idx integer?)
;(define-symbolic* int integer?)
;(define h (uf-insn op idx int))

#;(define (sketch-graph input1 input2)
  (define r1 input1)
  (define r2 input2)
  (define r3 (constantE 1 r1))
  (define r4 (constantE -1 r2))
  (define r5 (mergeE r3 r4))
  (define r6 (call-uf-insn h (list r1 r2 r3 r4 r5)))
  (define r7 (startsWith 0 r6))
  (list-ref (list r1 r2 r3 r4 r5 r6 r7) retval-idx))

#;(time (synthesize #:forall (cons f (harvest s-inc s-dec))
                  #:guarantee (assert (same sketch-graph straightline-graph s-inc s-dec))))