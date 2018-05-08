#lang rosette

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../operators.rkt")
(require "../sketch.rkt")
(require "../specifications.rkt")

;(error-print-width 100000000000)

(current-bitwidth #f)

(define stream-length 5)
(define s-clockB (new-behavior get-sym-int stream-length))
(define s-motionSensorB (new-behavior get-sym-bool stream-length))
(define s-rainGaugeB (new-behavior get-sym-bool stream-length))
(define s-rainedYesterdayB (new-behavior get-sym-bool stream-length))

(define (rained-yesterday-graph clockB rainGaugeB)
  (define r1 clockB)
  (define r2 rainGaugeB)
  (define r3 (changes r1))
  (define r4 (changes r2))
  (define r5 (filterE (λ (x) (= x 0)) r3)) ;; reset at midnight
  (define r6 (filterE (λ (x) x) r4))
  (define r7 (notE r5))
  (define r8 (mergeE r6 r7))
  (define r9 (startsWith #f r8))
  r9)

(define (sprinkler-graph clockB motionSensorB rainedYesterdayB)
  (define r1 clockB)
  (define r2 motionSensorB)
  (define r3 rainedYesterdayB)
  (define r4 (liftB1 (λ (h) (= h 18)) r1)) ;; 6-7pm
  (define r5 (notB r3)) ;; didn't rained yesterday
  (define r6 (notB r2)) ;; motion sensor not triggered
  (define r7 (andB r4 r5))
  (define r8 (andB r6 r7))
  r8)

(define state-mask (list->vector (list #t #t #f #f #f #f #t)))
(define rained-sketch (sketch (get-holes-list 7) state-mask stateless-operator-list stateful-operator-list 2))
(synth-from-ref-impl rained-sketch rained-yesterday-graph s-clockB s-rainGaugeB)

(define sprinkler-mask (list->vector (list #f #f #f #f #f)))
(define sprinkler-sketch (sketch (get-holes-list 5) sprinkler-mask stateless-operator-list stateful-operator-list 3))
(synth-from-ref-impl sprinkler-sketch sprinkler-graph s-clockB s-motionSensorB s-rainedYesterdayB)