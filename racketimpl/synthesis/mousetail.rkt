#lang rosette/safe
(require rosette/lib/synthax)

(require "../fjmodels.rkt")
(require "../rosettefjapi.rkt")
(require "../benchmarks/mousetail.rkt")
(require "grammar.rkt")

(current-bitwidth 5)
(printf "Current bitwidth: ~a~n" (current-bitwidth))

(define x-offset 3)
(define time-delay 3)

(define (spec-mouse-tail-x-graph x-evt-stream)
  (mapE (λ (e) (list (get-timestamp e) (+ (get-value e) x-offset))) (delayE x-evt-stream time-delay)))
(define (spec-mouse-tail-y-graph y-evt-stream)
  (delayE y-evt-stream time-delay))

(define (int-sketch-mouse-tail-x-graph x-evt-stream)
  (mapE (λ (e) (list (get-timestamp e) (+ (get-value e) (?? integer?)))) (delayE x-evt-stream (??))))

(define (synth-mouse-tail-x-graph x-evt-stream)
  (flapjaxE-grmr x-evt-stream 3))

(define (synth-mouse-tail-y-graph y-evt-stream)
  (flapjaxE-grmr y-evt-stream 3))

;(define (op-sketch-mouse-tail-y-graph y-evt-stream)
;  (delayE y-evt-stream (integer-constants 3)))

(define s-mouse-x (integer-event-stream 3))
(define s-mouse-y (integer-event-stream 3))

(assert (mousetail-x-assumptions s-mouse-x))
(assert (mousetail-y-assumptions s-mouse-y))

(displayln "Synthesize mousetail program:")

(define begin-time (current-seconds))
(define binding
    (synthesize #:forall (append (harvest-events s-mouse-x)
                                 (harvest-events s-mouse-y))
                #:guarantee (begin (assert (same spec-mouse-tail-x-graph
                                         synth-mouse-tail-x-graph
                                         s-mouse-x))
                                  (assert (same spec-mouse-tail-y-graph
                                         synth-mouse-tail-y-graph
                                         s-mouse-y)))))
(if (unsat? binding)
    (displayln "No binding was found.")
    (print-forms binding))
(define end-time (current-seconds))
(printf "Took ~a seconds~n" (- end-time begin-time))
