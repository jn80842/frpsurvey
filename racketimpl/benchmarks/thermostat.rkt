#lang rosette/safe

(require "../densefjapi.rkt")
(require "../dense-fjmodels.rkt")

(provide (all-defined-out))

;;                    ifB
;;            /        |         \
;;          andB    constantB    constantB
;;        /    \        |            |
;;    liftB    liftB   'on          'off
;;   /    \     /   \
;;  λ   tempB  λ   clockB

(current-bitwidth 6)

;; to handle bitwidth, constants don't make real-world sense
(define temp-floor 2)
(define hour-begin 4)
(define hour-end 2)

(define temp-top-range 5)
(define temp-bottom-range -1)

(define midnight (vector 0 0 0))

(define concrete-temps (behavior 2 '(2 2 2 4 3 2 2 4 3)))
#;(define concrete-clock (behavior midnight (list (vector 3 0 0)
                                                (vector 3 0 0)
                                                (vector 4 0 0)
                                                (vector 4 0 0)
                                                (vector 5 0 0)
                                                (vector 0 0 0)
                                                (vector 1 0 0)
                                                (vector 2 0 0)
                                                (vector 2 0 0))))
(define concrete-clock (behavior 0 (list 3 3 4 4 5 0 1 2 2)))
(define concrete-heater (behavior 'on '(off off on off off on on off off)))

(define stream-length 3)

(define s-tempB (new-behavior sym-integer stream-length))
;(define s-clockB (new-behavior sym-time-vec stream-length))
(define s-clockB (new-behavior sym-integer stream-length))
(define s-boolB (new-behavior sym-boolean stream-length))

#;(define (thermostat-graph tempB clockB)
  (ifB (andB (liftB1 (λ (t) (<= t temp-floor)) tempB)
             (liftB1 (λ (c) (or (>= (vector-ref c 0) hour-begin) (>= hour-end (vector-ref c 0)))) clockB))
        (constantB 'on)
        (constantB 'off)))
(define (thermostat-graph tempB clockB)
  (ifB (andB (liftB1 (λ (t) (<= t 2)) tempB)
             (liftB1 (λ (c) (or (>= c 4) (>= 2 c))) clockB))
       (constantB 'on)
       (constantB 'off)))

#;(define (thermostat-assumptions temp clock)
  (and (valid-time-vec-behavior? clock)
       (behavior-check (λ (v) (and (= 0 (vector-ref v 1)) (= (vector-ref v 2) 0))) clock) ; minutes don't matter
       (behavior-check (λ (v) (> 5 (vector-ref v 0))) clock) ; limit range of hours
       (behavior-check (λ (t) (and (< -1 t) (< t 5))) temp) ; limit range of temp
       ))
(define (thermostat-assumptions temp clock)
  (and (behavior-check (λ (h) (and (< -1 h) (> 5 h))) clock)
       (behavior-check (λ (t) (and (< -1 t) (< t 5))) temp)))

(define (thermostat-guarantees temp clock)
  (let ([heater (thermostat-graph temp clock)])
    (and (behavior-check (λ (v) (or (equal? v 'on) (equal? v 'off))) heater)
         (behavior-check (λ (heat t) (implication (eq? 'on heat) (<= t temp-floor))) heater temp)
         (behavior-check (λ (heat c) (implication (eq? 'on heat) (or (>= (vector-ref c 0) hour-begin) (>= hour-end (vector-ref c 0)))))
                         heater clock)
         )))
