#lang rosette

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../operators.rkt")
(require "../sketch.rkt")
(require "../specifications.rkt")
(require "../benchmarks/mousetail.rkt")

(current-bitwidth #f)

(define (straightline-mousetail-y-graph y-stream)
  (define r1 y-stream)
  (define r2 (delayE 3 r1))
  r2)

(define (straightline-mousetail-x-graph x-stream)
  (define r1 x-stream)
  (define r2 (delayE 3 r1))
  (define r3 (mapE (λ (e) (+ e x-offset)) r2))
  r3)

(displayln "mousetail benchmark")

(define v-binding-x (verify (assert (same mousetail-x-graph straightline-mousetail-x-graph s-mouse-x))))
(if (unsat? v-binding-x)
    (displayln "verified example mouse-x implementation and straightline mouse-x program are equivalent")
    (displayln "can't verify straightline mouse-x program is equiv to example implementation"))
(define v-binding-y (verify (assert (same mousetail-y-graph straightline-mousetail-y-graph s-mouse-y))))
(if (unsat? v-binding-y)
    (displayln "verified example mouse-y implementation and straightline mouse-y program are equivalent")
    (displayln "can't verify straightline mouse-y program is equiv to example implementation"))

(define x-state-mask (list->vector (list #t #f)))
(define y-state-mask (make-vector 1 #t))

(define mouse-x-sketch (sketch (get-holes-list 2) x-state-mask (get-retval-idx)
                               stateless-operator-list stateful-operator-list 1))
(define mouse-y-sketch (sketch (get-holes-list 1) y-state-mask (get-retval-idx)
                               stateless-operator-list stateful-operator-list 1))

(synth-from-ref-impl mouse-x-sketch straightline-mousetail-x-graph s-mouse-x)
(synth-from-ref-impl mouse-y-sketch straightline-mousetail-y-graph s-mouse-y)

(define sym-x-input (list (sym-input "mouse-x" s-mouse-x)))
(define sym-y-input (list (sym-input "mouse-y" s-mouse-y)))

(define x-spec (io-specs (list '(1 no-evt no-evt no-evt))
                         '(no-evt no-evt no-evt 6)))
(define y-spec (io-specs (list '(1 no-evt no-evt no-evt))
                         '(no-evt no-evt no-evt 1)))
(define x-event-at-every-timestep (input-invariant sym-x-input (λ (e) (not (empty-event? e)))))
(define y-event-at-every-timestep (input-invariant sym-y-input (λ (e) (not (empty-event? e)))))

(specs-synthesis mouse-x-sketch (list x-spec x-event-at-every-timestep) sym-x-input)
(specs-synthesis mouse-y-sketch (list y-spec y-event-at-every-timestep) sym-y-input)