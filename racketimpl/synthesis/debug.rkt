#lang rosette/safe
(require rosette/lib/synthax)

(require "../fjmodels.rkt")
(require "../rosettefjapi.rkt")
(require "grammar.rkt")

;; uncomment (constantB 1) to break synthesis
(define-synthax (flapjax-grmrB input-behavior depth)
  #:base input-behavior
  #:else (choose input-behavior
                (constantB 1)
                 (liftB (λ (t) (<= t 2)) (flapjax-grmrB input-behavior (sub1 depth)))
                 ))

(current-bitwidth 5)

(define (thermostat-graph tempB)
  (liftB (λ (t) (<= t 2)) tempB))

;; change depth to 2 here to break synthesis
(define (synth-thermostat-graph tempB)
  (flapjax-grmrB tempB 3))

(define s-tempB (integer-behavior 2))
(assert (valid-behavior? s-tempB))

(define begin-time (current-seconds))
(define binding (synthesize #:forall (harvest-behavior s-tempB)
                            #:guarantee (assert (equal? (thermostat-graph s-tempB) (synth-thermostat-graph s-tempB)))
                            ))
(define end-time (current-seconds))
(if (unsat? binding)
    (displayln "No binding was found.")
    (print-forms binding))
(printf "Took ~a seconds~n" (- end-time begin-time))




