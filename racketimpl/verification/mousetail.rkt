#lang rosette/safe

(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")
(require "../benchmarks/mousetail.rkt")

(print-bitwidth-warning)
(printf "length of mouse-x ~a~n" (length mouse-x))
(printf "length of mouse-y ~a~n" (length mouse-y))

(check-existence-of-solution mousetail-assumptions mouse-x mouse-y)

(displayln "Verify mousetail spec")

(define begin-time (current-seconds))

(define verified (verify
                  #:assume (assert (mousetail-assumptions mouse-x mouse-y))
                  #:guarantee (assert (mousetail-guarantees mouse-x mouse-y (mouse-tail-x-graph mouse-x) (mouse-tail-y-graph mouse-y)))))

(if (unsat? verified)
    (displayln "Spec is verified.")
    (printf "Model that violates spec is found: mouse-x ~a, mouse-y ~a~n" (evaluate mouse-x verified) (evaluate mouse-y verified)))

(define end-time (current-seconds))
(printf "Took ~a seconds~n" (- end-time begin-time))