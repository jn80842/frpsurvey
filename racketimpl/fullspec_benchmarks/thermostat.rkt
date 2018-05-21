#lang rosette

;;(error-print-width 100000000000)

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../sketch.rkt")
(require "../operators.rkt")
(require "../specifications.rkt")
(require "../benchmarks/thermostat.rkt")

(current-bitwidth #f)

(displayln "thermostat benchmark")

(define (straightline-graph tempB clockB)
  (define r1 tempB)
  (define r2 clockB)
  (define r3 (liftB1 (λ (t) (<= t 2)) r1))
  (define r4 (liftB1 (λ (c) (or (>= c 4) (>= 2 c))) r2))
  (define r5 (andB r3 r4))
  (define r6 (constantB 'on r1))
  (define r7 (constantB 'off r2))
  (define r8 (ifB r5 r6 r7))
  r8)

(define state-mask (list->vector (list #f #f #f #f #f #f)))

(define thermostat-sketch (sketch (get-holes-list 6) state-mask
                                  stateless-operator-list stateful-operator-list 2))

(synth-from-ref-impl thermostat-sketch straightline-graph s-tempB s-clockB)