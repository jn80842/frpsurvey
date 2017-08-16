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
                ;(liftB (λ (t) (<= t 2)) (flapjax-grmrB input-behavior (sub1 depth)))
                ;(liftB-no-let (λ (t) (<= t 2)) (flapjax-grmrB input-behavior (sub1 depth)))
                (liftB-no-enhanced (λ (t) (<= t 2)) (flapjax-grmrB input-behavior (sub1 depth)))
                 ))

(define-synthax (flapjax-debug-list input-list depth)
  #:base input-list
  #:else (choose input-list
                 (constantB 'on)
                 (liftB-list-no-let (λ (t) (<= t 2)) (flapjax-debug-list input-list (sub1 depth)))))

(current-bitwidth 6)

(define (thermostat-graph tempB)
  (liftB (λ (t) (<= t 2)) tempB))

;; change depth to 2 here to break synthesis
(define (synth-thermostat-graph tempB)
  (flapjax-grmrB tempB 2))

(define (thermostat-list-graph tempList)
  (liftB-list-no-let (λ (t) (<= t 2)) tempList))

;; change depth to 2 here to break synthesis
(define (synth-thermostat-list-graph tempList)
  (flapjax-debug-list tempList 2))

(define s-tempB (integer-behavior 2))
(assert (valid-behavior? s-tempB))
;(define s-temp (integer-event-stream 3))
;(assert (valid-timestamps? s-temp))

(define begin-time (current-seconds))
(define binding (synthesize #:forall (harvest-behavior s-tempB)
                            #:guarantee (assert (eq? (thermostat-graph s-tempB) (synth-thermostat-graph s-tempB)))
                            ))
#;(define binding (synthesize #:forall (harvest-events s-temp)
                            #:guarantee (assert (eq? (thermostat-list-graph s-temp) (synth-thermostat-list-graph s-temp)))))
(define end-time (current-seconds))
(if (unsat? binding)
    (displayln "No binding was found.")
    (print-forms binding))
(printf "Took ~a seconds~n" (- end-time begin-time))




