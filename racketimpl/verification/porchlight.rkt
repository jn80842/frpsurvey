#lang rosette/safe

(require "../fjmodels.rkt")
(require "../rosettefjapi.rkt")
(require "../benchmarks/porchlight.rkt")

;;;;  1 2         7 8
;;; f w o         w o
;;;               f

;;;;  1 2         7
;;; f w o         w
;;;               f

(if (>= delay-by (max-for-current-bitwidth (current-bitwidth)))
    (displayln "DELAY BY IS TOO HIGH and WILL CAUSE OVERFLOW")
    (printf "delay-by is ~a~n" delay-by))

(define concrete-motion (list (list 1 'd) (list 3 'd) (list 10 'd) (list 20 'd)))
(define m (list (list 18 'd) (list 23 'd)))

(define concrete-motion-ts '((1 1) (3 3) (10 10) (20 20)))

(if (eq? (light-graph concrete-motion)
         '((1 on) (8 off) (10 on) (15 off) (20 on) (25 off)))
    (displayln "graph is correct on concrete inputs")
    (displayln "graph fails on concrete inputs"))

(print-bitwidth-warning)
(printf "number of motion detector events: ~a\n" stream-length)

(check-existence-of-solution light-assumptions s-motion)

(define begin-time (current-seconds))
(define verified (verify #:assume (assert (light-assumptions s-motion))
                         #:guarantee (assert (light-guarantees s-motion (light-graph s-motion)))))
(define end-time (current-seconds))
(printf "time to verify: ~a seconds~n" (- end-time begin-time))
(if (unsat? verified)
    (displayln "Spec is verified")
    (printf "Model that violates spec is found: motion ~a~n" (evaluate s-motion verified)))