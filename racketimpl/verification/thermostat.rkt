#lang rosette/safe

(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")
(require "../benchmarks/thermostat.rkt")

(provide thermostat-assumptions)

#;(if (>= hour-begin (sub1 (expt 2 (sub1 (current-bitwidth)))))
    (displayln "HOUR BEGIN IS TOO HIGH and WILL CAUSE OVERFLOW")
    (printf "hour-begin is ~a~n" hour-begin))

(define (thermostat-graph tempB clockB)
  (ifB (andB (liftB (λ (t) (<= t temp-floor)) tempB)
             (liftB (λ (c) (or (>= (vector-ref c 0) hour-begin) (>= hour-end (vector-ref c 0)))) clockB))
        (constantB 'on)
        (constantB 'off)))

(define concrete-temp (behavior 4 (list (list 1 2) (list 3 1) (list 15 4) (list 17 2) (list 22 5) (list 30 7))))
(define concrete-clock (behavior (vector 19 0 0) (list (list 1 (vector 20 0 0)) (list 10 (vector 23 0 0))
                                                       (list 20 (vector 7 3 0)) (list 25 (vector 8 0 5)))))

(if (eq? (thermostat-graph concrete-temp concrete-clock) (behavior 'off (list (list 1 'on) (list 3 'on)
                                                                              (list 10 'on) (list 15 'off)
                                                                              (list 17 'on) (list 20 'on)
                                                                              (list 22 'off) (list 25 'off) (list 30 'off))))
    (displayln "graph is correct on concrete inputs")
    (displayln "graph fails on concrete inputs"))

(define s-temp (new-behavior sym-integer stream-length))
(define s-clock (new-behavior sym-time-vec stream-length))

(printf "current bitwidth is: ~a\n" (current-bitwidth))
(printf "number of temp changes: ~a\n" (length (behavior-changes s-temp)))
(printf "number of clock changes: ~a\n" (length (behavior-changes s-clock)))

(check-existence-of-solution thermostat-assumptions s-temp s-clock)

(define (thermostat-guarantees temp clock)
  (let ([heater (thermostat-graph temp clock)])
    (and (valid-behavior? heater)
         (or (equal? (behavior-init heater) 'on) (equal? (behavior-init heater) 'off))
         (andmap (λ (v) (or (equal? v 'on) (equal? v 'off))) (map get-value (behavior-changes heater)))
         (behavior-check (λ (heat t) (implication (eq? 'on heat) (<= t temp-floor))) heater temp)
         (behavior-check (λ (heat c) (implication (eq? 'on heat) (or (>= (vector-ref c 0) hour-begin) (>= hour-end (vector-ref c 0)))))
                         heater clock)
         )))

(define begin-time (current-seconds))
(define verified (verify #:assume (assert (thermostat-assumptions s-temp s-clock))
                         #:guarantee (assert (thermostat-guarantees s-temp s-clock))))
(define end-time (current-seconds))
(printf "time to verify: ~a seconds~n" (- end-time begin-time))
(if (unsat? verified)
    (displayln "Spec is verified.")
    (printf "Model that violates spec is found: temp behavior ~a, clock behavior ~a~n" (evaluate s-temp verified) (evaluate s-clock verified)))