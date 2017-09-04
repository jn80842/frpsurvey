#lang rosette/safe

(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")
(require "../benchmarks/thermostat.rkt")

(define concrete-temp (behavior 4 (list (list 1 2) (list 3 1) (list 15 4) (list 17 2) (list 22 5) (list 30 7))))
(define concrete-clock (behavior (vector 19 0 0) (list (list 1 (vector 20 0 0)) (list 10 (vector 23 0 0))
                                                       (list 20 (vector 7 3 0)) (list 25 (vector 8 0 5)))))

(if (eq? (thermostat-graph concrete-temp concrete-clock) (behavior 'off (list (list 1 'on) (list 3 'on)
                                                                              (list 10 'on) (list 15 'off)
                                                                              (list 17 'on) (list 20 'on)
                                                                              (list 22 'off) (list 25 'off) (list 30 'off))))
(displayln "graph is correct on concrete inputs")
(displayln "graph fails on concrete inputs"))

(print-bitwidth-warning)
(printf "number of temp changes: ~a\n" (length (behavior-changes s-tempB)))
(printf "number of clock changes: ~a\n" (length (behavior-changes s-clockB)))

(check-existence-of-solution thermostat-assumptions s-tempB s-clockB)

(define begin-time (current-seconds))
(define verified (verify #:assume (assert (thermostat-assumptions s-tempB s-clockB))
                         #:guarantee (assert (thermostat-guarantees s-tempB s-clockB))))
(define end-time (current-seconds))
(printf "time to verify: ~a seconds~n" (- end-time begin-time))
(if (unsat? verified)
    (displayln "Spec is verified.")
    (printf "Model that violates spec is found: temp behavior ~a, clock behavior ~a~n" (evaluate s-tempB verified) (evaluate s-clockB verified)))