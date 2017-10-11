#lang rosette/safe
(require rosette/lib/synthax)

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../benchmarks/thermostat.rkt")
;(require "grammar.rkt")

;;                    ifB
;;            /        |         \
;;          andB    constantB    constantB
;;        /    \        |            |
;;    liftB    liftB   'on          'off
;;   /    \     /   \
;;  λ   tempB  λ   clockB

#;(define-synthax (flapjax-grmr input ... depth)
  #:base (choose input ...)
  #:else (let ([recursive1 (flapjax-grmr input ... (sub1 depth))]
               [recursive2 (flapjax-grmr input ... (sub1 depth))]
               [recursive3 (flapjax-grmr input ... (sub1 depth))])
           (choose input ...
                 (constantB (choose 'on 'off))
                 (liftB1 (choose (λ (t) (<= t (??)))
                                 (λ (t) #t)
                                 (λ (h) (or (>= h (??)) (>= (??) h))))
                                 ;(λ (c) (or (>= (vector-ref c 0) (??)) (>= (??) (vector-ref c 0)))))
                                  recursive1)
                 (andB recursive1 recursive2)
                 (ifB recursive1 recursive2 recursive3))))

(define-synthax (flapjax-grmr input depth)
  #:base input
  #:else (choose input
                ; (constantB (choose 'on 'off))
                 (ifB (flapjax-grmr input (sub1 depth))
                      (constantB 'on)
                      (constantB 'off))))

(define (synth-thermostat-graph boolB)
  (flapjax-grmr boolB 2))

(define (component-graph boolB)
  (ifB boolB (constantB 'off) (constantB 'on)))


(assert (thermostat-assumptions s-tempB s-clockB))

(define begin-time (current-seconds))
(define binding
  (time (synthesize #:forall (harvest s-boolB)
                    #:guarantee (assert (same component-graph synth-thermostat-graph
                                              s-boolB))
                    )))

#;(define verified
  (verify #:assume (thermostat-assumptions s-tempB s-clockB)
          #:guarantee (assert (same component-graph synth-thermostat-graph s-tempB s-boolB))))
(define end-time (current-seconds))
(if (unsat? binding)
    (displayln "No binding was found.")
    (print-forms binding))
(printf "Took ~a seconds~n" (- end-time begin-time))




