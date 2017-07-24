#lang rosette/safe

(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")

(current-bitwidth 4)

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

(define stream-length 4)

(define s-temp (positive-integer-behavior stream-length))
(define s-clock (positive-integer-behavior stream-length))

(printf "current bitwidth is: ~a\n" (current-bitwidth))
(printf "number of temp changes: ~a\n" (length (behavior-changes s-temp)))
(printf "number of clock changes: ~a\n" (length (behavior-changes s-clock)))

(define (thermostat-assumptions temp clock)
  (and (valid-behavior? temp)
       (valid-behavior? clock)))
       ;(< (behavior-init temp) 32)
       ;(andmap (λ (n) (< n 32)) (map get-value (behavior-changes temp)))
       ;(positive? (behavior-init clock))
       ;(andmap positive? (map get-value (behavior-changes clock)))))

(check-existence-of-solution thermostat-assumptions s-temp s-clock)

(define (thermostat-guarantees temp clock)
  (let ([heater (thermostat-graph temp clock)])
    (and (valid-behavior? heater)
         (or (equal? (behavior-init heater) 'on) (equal? (behavior-init heater) 'off))
         (andmap (λ (v) (or (equal? v 'on) (equal? v 'off))) (map get-value (behavior-changes heater))))))

(define begin-time (current-seconds))
(define verified (verify #:assume (assert (thermostat-assumptions s-temp s-clock))
                         #:guarantee (assert (thermostat-guarantees s-temp s-clock))))
(define end-time (current-seconds))
(printf "time to verify: ~a seconds~n" (- end-time begin-time))
(if (unsat? verified)
    (displayln "Spec is verified.")
    (printf "Model that violates spec is found: temp behavior ~a, clock behavior ~a~n" (evaluate s-temp verified) (evaluate s-clock verified)))