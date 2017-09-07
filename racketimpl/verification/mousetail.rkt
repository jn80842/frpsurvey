#lang rosette/safe

(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")
(require "../benchmarks/mousetail.rkt")

(print-bitwidth-warning)
(printf "length of mouse-x ~a~n" (length s-mouse-x))
(printf "length of mouse-y ~a~n" (length s-mouse-y))

(check-existence-of-solution mousetail-assumptions s-mouse-x s-mouse-y)

(displayln "Verify mousetail spec")

(define begin-time (current-seconds))

(define verified (verify
                  #:assume (assert (mousetail-assumptions s-mouse-x s-mouse-y))
                  #:guarantee (assert (mousetail-guarantees s-mouse-x s-mouse-y (mousetail-x-graph s-mouse-x) (mousetail-y-graph s-mouse-y)))))

(if (unsat? verified)
    (displayln "Spec is verified.")
    (printf "Model that violates spec is found: mouse-x ~a, mouse-y ~a~n" (evaluate s-mouse-x verified) (evaluate s-mouse-y verified)))

(define end-time (current-seconds))
(printf "Took ~a seconds~n" (- end-time begin-time))