#lang rosette/safe

(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")

(current-bitwidth 7)

;; to handle bitwidth, constants don't make real-world sense
(define temp-floor 2)
;(define temp-ceiling 65)
(define hour-begin 10)
(define hour-end 5)

(define (thermostat-graph tempB clockB)
  (ifB (andB (liftB (λ (t) (<= t temp-floor)) tempB)
             (liftB (λ (c) (or (>= c hour-begin) (>= hour-end c))) clockB))
       (constantB 'on)
       (constantB 'off)))

(define concrete-temp (behavior 60 (list (list 1 59) (list 3 57) (list 15 61) (list 17 59) (list 22 61) (list 30 59))))
(define concrete-clock (behavior 1900 (list (list 1 2000) (list 10 2300) (list 20 730) (list 25 805))))

(define s-temp (positive-integer-behavior (list 1 2 3)))
(define s-clock (positive-integer-behavior (list 1 2 3)))

(printf "current bitwidth is: ~a\n" (current-bitwidth))
(printf "number of temp changes: ~a\n" (length (behavior-changes s-temp)))
(printf "number of clock changes: ~a\n" (length (behavior-changes s-clock)))
                                                      
(define (assert-thermostat-assumptions temp clock)
  (begin
    (assert (valid-behavior? temp))
    (assert (valid-behavior? clock))
    (assert (< (behavior-init temp) 32))
    (assert (andmap (λ (n) (< n 32)) (map get-value (behavior-changes temp)))) ;; this is bogus, just to make rosette instantiate a value
    (assert (positive? (behavior-init clock)))
    (assert (andmap positive? (map get-value (behavior-changes clock))))
    ))

(define solved (solve (assert-thermostat-assumptions s-temp s-clock)))

(if (unsat? solved)
    (displayln "no solution for assumptions")
    (begin
      (displayln "sample solution for assumptions:")
      (printf "temp model: ~a\n" (evaluate s-temp solved))
      (printf "clock model: ~a\n" (evaluate s-clock solved))))

(define (assert-thermostat-guarantees heater)
  (begin
    (assert (valid-behavior? heater))
    (assert (or (equal? (behavior-init heater) 'on) (equal? (behavior-init heater) 'off)))
    (assert (andmap (λ (v) (or (equal? v 'on) (equal? v 'off))) (map get-value (behavior-changes heater))))
    ))

(define begin-time (current-seconds))
(define verified (verify #:assume (assert-thermostat-assumptions s-temp s-clock)
                         #:guarantee (assert-thermostat-guarantees (thermostat-graph s-temp s-clock))))
(define end-time (current-seconds))
(printf "time to verify: ~a seconds~n" (- end-time begin-time))
(if (unsat? verified)
    (displayln "Spec is verified.")
    (displayln "Model that violates spec is found: temp behavior ~a, clock behavior ~a~n" (evaluate s-temp verified) (evaluate s-clock verified)))