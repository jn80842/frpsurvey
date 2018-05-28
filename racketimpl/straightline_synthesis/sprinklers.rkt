#lang rosette

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../operators.rkt")
(require "../sketch.rkt")


;(error-print-width 100000000000)

(current-bitwidth #f)

(define stream-length 3)

(define s-clockE (new-event-stream get-sym-int stream-length))
(define s-raingaugeB (new-behavior get-sym-int stream-length))
(define s-motionsensorB (new-behavior get-sym-bool stream-length))
(define s-rainedyesterdayB (new-behavior get-sym-bool stream-length))

(define (rained-yesterday-graph clockE raingaugeB)
  (define r1 clockE)
  (define r2 raingaugeB)
  (define r3 (changes r2))
  (define r4 (filterE (λ (e) (= e 18)) r1)) ;; event issued at 6pm
  (define r5 (filterE (λ (e) e) r3)) ;; event every time rain gauge changed to true
  (define r6 (constantE #f r4))
  (define r7 (mergeE r5 r6))
  (define r8 (startsWith #f r7))
  r8)

(displayln "synthesize did it rain yesterday graph")

(define holes (for/list ([i (range 6)]) (get-insn-holes)))
(define-symbolic* retval-idx integer?)

(define state-mask (list->vector (list #t #f #f #f #f #t)))
(define rained-yesterday-sketch (sketch (get-holes-list 6) state-mask (get-retval-idx)
                                        stateless-operator-list stateful-operator-list 2))

;(synth-from-ref-impl rained-yesterday-sketch rained-yesterday-graph s-clockE s-raingaugeB)

(displayln "synthesize partial sprinkler graph")

(define (partial-sprinklers-graph clockE motionSensorB yesterdayB)
  (define r1 clockE)
  (define r2 motionSensorB)
  (define r3 yesterdayB)
  (define r4 (orB r2 r3))
  (define r5 (notB r4))
  (define r6 (mapE (λ (e) (= e 18)) r1))
  (define r7 (startsWith #f r6))
  (define r8 (andB r7 r5))
  r8)

(define partial-state-mask (list->vector (list #f #f #f #t #f)))

(define partial-sketch (sketch (get-holes-list 5) partial-state-mask (get-retval-idx)
                               stateless-operator-list stateful-operator-list 3))

;(synth-from-ref-impl partial-sketch partial-sprinklers-graph s-clockE s-motionsensorB s-rainedyesterdayB)


(displayln "synthesize full sprinkler graph")

#;(define (sprinklers-graph clockE motionSensorB rainGaugeB)
  (define r1 clockE)
  (define r2 motionSensorB)
  (define r3 rainGaugeB)
  (define r4 (changes r3))
  (define r5 (filterE (λ (e) (= e 18)) r1))
  (define r6 (filterE (λ (e) e) r4))
  (define r7 (constantE #f r5))
  (define r8 (mergeE r6 r7))
  (define r9 (startsWith #f r8)) ;; did it rain yesterday?
  (define r10 (orB r2 r9))
  (define r11 (notB r10))
  (define r12 (startsWith #f r5))
  (define r13 (andB r11 r12))
  r13)

(define (sprinklers-graph clockE motionSensorB rainGaugeB)
  (define r1 clockE)
  (define r2 motionSensorB)
  (define r3 rainGaugeB)
  (define r4 (notB r2))
  (define r5 (mapE (λ (e) (= e 18)) r1))
  (define r6 (startsWith #f r5))
  (define r7 (andB r4 r6)) ;; motion sensor is not on, within 6pm hour
  (define r8 (changes r3))
  (define r9 (filterE identity r8)) ;; events when rain started
  (define r10 (notE r9))
  (define r11 (filterE identity r5)) ;; events when 6pm occurred
  (define r12 (mergeE r10 r11))
  (define r13 (startsWith #f r12))
  (define r14 (andB r7 r13))
  r14)

(define full-state-mask (list->vector (list #f #f #t #f #t #f #f #f #f #t #f)))

(define sprinklers-sketch (sketch (get-holes-list 11) full-state-mask (get-retval-idx)
                                 stateless-operator-list stateful-operator-list 3))

(synth-from-ref-impl sprinklers-sketch sprinklers-graph s-clockE s-motionsensorB s-raingaugeB)
