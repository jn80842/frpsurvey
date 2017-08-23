#lang rosette/safe
(require rosette/lib/synthax)

(require "../fjmodels.rkt")
(require "../rosettefjapi.rkt")
(require "../benchmarks/thermostat.rkt")
(require "grammar.rkt")

(current-bitwidth 6)
(unless (>= (current-bitwidth) 6)
  (displayln "bitwidth too low for time vec!!!!"))

(define temp-floor 2)
(define hour-begin 2)
(define hour-end 1)

(define (thermostat-graph tempB clockB)
  (ifB (andB (liftB (λ (t) (<= t temp-floor)) tempB)
             (liftB (λ (c) (or (>= (vector-ref c 0) hour-begin) (>= hour-end (vector-ref c 0)))) clockB))
       (constantB 'on)
       (constantB 'off)))

(define (synth-thermostat-graph tempB clockB)
  (flapjaxB-grmr clockB tempB 3))

(define s-tempB (integer-behavior 2))
(define s-clockB (time-vec-behavior 2))

(check-existence-of-solution thermostat-assumptions s-tempB s-clockB)

(assert (thermostat-assumptions s-tempB s-clockB))

(define begin-time (current-seconds))
(define binding (synthesize #:forall (append (harvest-behavior s-tempB) (harvest-behavior s-clockB))
                            #:guarantee (assert (same thermostat-graph synth-thermostat-graph
                                                      s-tempB s-clockB))
                            ))
(define end-time (current-seconds))
(if (unsat? binding)
    (displayln "No binding was found.")
    (print-forms binding))
(printf "Took ~a seconds~n" (- end-time begin-time))




