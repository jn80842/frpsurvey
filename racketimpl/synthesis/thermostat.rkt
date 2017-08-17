#lang rosette/safe
(require rosette/lib/synthax)

(require "../fjmodels.rkt")
(require "../rosettefjapi.rkt")
;(require "../benchmarks/thermostat.rkt")
(require "grammar.rkt")

(current-bitwidth 6)
(unless (>= (current-bitwidth) 6)
  (displayln "bitwidth too low for time vec!!!!"))

(define temp-floor 2)
(define hour-begin 4)
(define hour-end 2)

(define temp-top-range 5)
(define temp-bottom-range -1)

(define-synthax (flapjax-grmrB input-behavior1 input-behavior2 depth)
  #:base (choose input-behavior1 input-behavior2)
  #:else (choose input-behavior1 input-behavior2
                (liftB (choose (λ (t) (<= t temp-floor))
                                (λ (c) (or (>= (vector-ref c 0) hour-begin) (>= hour-end (vector-ref c 0)))))
                        (flapjax-grmrB input-behavior1 input-behavior2  (sub1 depth)))
                (andB (flapjax-grmrB input-behavior1 input-behavior2 (sub1 depth)) (flapjax-grmrB input-behavior1 input-behavior2 (sub1 depth)))
                (ifB (flapjax-grmrB input-behavior1 input-behavior2 (sub1 depth))
                     (flapjax-grmrB input-behavior1 input-behavior2 (sub1 depth))
                     (flapjax-grmrB input-behavior1 input-behavior2 (sub1 depth)))
                 ))

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

(define (thermostat-graph tempB clockB)
  (ifB (andB (liftB (λ (t) (<= t temp-floor)) tempB)
             (liftB (λ (c) (or (>= (vector-ref c 0) hour-begin) (>= hour-end (vector-ref c 0)))) clockB))
       (constantB 'on)
       (constantB 'off)))

;; important -- double check constants in the grammar
(define (synth-thermostat-graph tempB clockB)
  (flapjax-grmrB tempB clockB 3))

(define s-tempB (integer-behavior 2))
(define s-clockB (time-vec-behavior 2))

(check-existence-of-solution thermostat-assumptions s-tempB s-clockB)

(assert (thermostat-assumptions s-tempB s-clockB))

(define begin-time (current-seconds))
(define binding (synthesize #:forall (append (harvest-behavior s-tempB) (harvest-behavior s-clockB))
                            #:guarantee (assert (eq? (thermostat-graph s-tempB s-clockB) (synth-thermostat-graph s-tempB s-clockB)))
                            ))
(define end-time (current-seconds))
(if (unsat? binding)
    (displayln "No binding was found.")
    (print-forms binding))
(printf "Took ~a seconds~n" (- end-time begin-time))




