#lang rosette/safe

(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")

(provide (all-defined-out))

(current-bitwidth 6)

;; to handle bitwidth, constants don't make real-world sense
(define temp-floor 2)
(define hour-begin 4)
(define hour-end 2)

(define temp-top-range 5)
(define temp-bottom-range -1)

(define midnight (vector 0 0 0))

(define stream-length 2)

(define s-tempB (new-behavior sym-integer stream-length (* 2 stream-length)))
(define s-clockB (new-behavior sym-time-vec stream-length (* 2 stream-length)))

(define (thermostat-graph tempB clockB)
  (ifB (andB (liftB (λ (t) (<= t temp-floor)) tempB)
             (liftB (λ (c) (or (>= (vector-ref c 0) hour-begin) (>= hour-end (vector-ref c 0)))) clockB))
        (constantB 'on)
        (constantB 'off)))

(define (thermostat-assumptions temp clock)
  (and (valid-behavior? temp)
       (valid-time-vec-behavior? clock)
       (behavior-check (λ (v) (and (= 0 (vector-ref v 1)) (= (vector-ref v 2) 2) 0)) clock) ; minutes don't matter
       (behavior-check (λ (v) (> 5 (vector-ref v 0))) clock) ; limit range of hours
       (behavior-check (λ (t) (and (< -1 t) (< t 5))) temp) ; limit range of temp
       (< temp-bottom-range (behavior-init temp))
       (< (behavior-init temp) temp-top-range)
       (andmap (λ (t) (and (< temp-bottom-range (get-value t)) (< (get-value t) temp-top-range))) (behavior-changes temp))
       (andmap (λ (ts) (> 6 ts)) (append (map get-timestamp (behavior-changes temp))
                                         (map get-timestamp (behavior-changes clock)))) ;; limit range of timestamps
       ))

(define (thermostat-guarantees temp clock)
  (let ([heater (thermostat-graph temp clock)])
    (and (valid-behavior? heater)
         (or (equal? (behavior-init heater) 'on) (equal? (behavior-init heater) 'off))
         (andmap (λ (v) (or (equal? v 'on) (equal? v 'off))) (map get-value (behavior-changes heater)))
         (behavior-check (λ (heat t) (implication (eq? 'on heat) (<= t temp-floor))) heater temp)
         (behavior-check (λ (heat c) (implication (eq? 'on heat) (or (>= (vector-ref c 0) hour-begin) (>= hour-end (vector-ref c 0)))))
                         heater clock)
         )))
