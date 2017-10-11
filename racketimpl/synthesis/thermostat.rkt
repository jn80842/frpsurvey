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

(define (ifB conditionB trueB falseB)
  (behavior (if (behavior-init conditionB) (behavior-init trueB) (behavior-init falseB))
            (map (λ (c t f) (if c t f)) (behavior-changes conditionB) (behavior-changes trueB) (behavior-changes falseB))))
(define (ifB-list condition-list true-list false-list)
  (map (λ (c t f) (if c t f)) condition-list true-list false-list))

#;(define-synthax (flapjax-grmr input ... depth)
  #:base (choose input ...)
  #:else (let ([recursive1 (flapjax-grmr input ... (sub1 depth))]
               [recursive2 (flapjax-grmr input ... (sub1 depth))]
               [recursive3 (flapjax-grmr input ... (sub1 depth))])
           (choose input ...
                 ;(constantB (choose 'on 'off #t))
                   (behavior 'on '(on))
                   (behavior 'off '(off))
                   (behavior #t '(#t))
                 (liftB1 (choose (λ (t) (<= t (??)))
                                 (λ (t) #t)
                                 (λ (h) (or (>= h (??)) (>= (??) h))))
                                 ;(λ (c) (or (>= (vector-ref c 0) (??)) (>= (??) (vector-ref c 0)))))
                                  recursive1)
                 (andB recursive1 recursive2)
                 (ifB recursive1 (behavior 'on '(on)) (behavior 'off '(off))))))

(define-synthax (flapjax-grmr input depth)
  #:base input
  #:else (choose input
                ; (constantB (choose 'on 'off))
                 ;(constantB 'on)
                 (ifB-list (flapjax-grmr input (sub1 depth))
                      '(on on) '(off off))))

(define-symbolic* b1 boolean?)
(define-symbolic* b2 boolean?)
(define bool-list (list b1 b2))

(define (synth-thermostat-graph boolB)
  (flapjax-grmr boolB 2))

(define (component-graph boolB)
  (ifB-list boolB '(on on) '(off off)))


(assert (thermostat-assumptions s-tempB s-clockB))

(define begin-time (current-seconds))
(define binding
  (time (synthesize #:forall bool-list
                    #:guarantee (assert (same component-graph synth-thermostat-graph
                                              bool-list))
                    )))

#;(define verified
  (verify #:assume (thermostat-assumptions s-tempB s-clockB)
          #:guarantee (assert (same component-graph synth-thermostat-graph s-tempB s-boolB))))
(define end-time (current-seconds))
(if (unsat? binding)
    (displayln "No binding was found.")
    (print-forms binding))
(printf "Took ~a seconds~n" (- end-time begin-time))




