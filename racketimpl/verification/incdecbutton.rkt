#lang s-exp rosette/safe

(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")
(require "../benchmarks/incdecbutton.rkt")

(print-bitwidth-warning)
(printf "length of increase clicks ~a~n" (length s-inc))
(printf "length of decrease clicks ~a~n" (length s-dec))

(check-existence-of-solution button-assumptions s-inc s-dec)

(define begin-time (current-seconds))
(define verified (verify
                  #:assume (assert (button-assumptions s-inc s-dec))
                  #:guarantee (assert (button-guarantees (inc-dec-button-graph s-inc s-dec)))
                  ))
(define end-time (current-seconds))
(printf "time to verify: ~a seconds~n" (- end-time begin-time))
(if (unsat? verified)
    (displayln "Spec is verified.")
    (printf "Model that violates spec is found: increase stream ~a, decrease stream ~a~n" (evaluate s-inc verified) (evaluate s-dec verified)))