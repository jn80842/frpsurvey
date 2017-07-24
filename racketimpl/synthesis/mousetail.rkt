#lang rosette/safe
(require rosette/lib/synthax)
(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")

(define x-offset 3)
(define time-delay 3)

(define (same program1 program2 input-stream)
  (assert
   (equal? (program1 input-stream) (program2 input-stream))))

(define (spec-mouse-tail-x-graph x-evt-stream)
  (mapE (λ (e) (list (get-timestamp e) (+ (get-value e) x-offset))) (delayE x-evt-stream time-delay)))
(define (spec-mouse-tail-y-graph y-evt-stream)
  (delayE y-evt-stream time-delay))

(define (int-sketch-mouse-tail-x-graph x-evt-stream)
  (mapE (λ (e) (list (get-timestamp e) (+ (get-value e) (?? integer?)))) (delayE x-evt-stream (??))))

(define (op-sketch-mouse-tail-x-graph x-evt-stream)
  ([choose mapE delayE constantE] (λ (e) (list (get-timestamp e) (+ (get-value e) x-offset)))
                                  ([choose mapE delayE constantE] x-evt-stream time-delay)))
(define (op-sketch-mouse-tail-y-graph y-evt-stream)
  ([choose mapE delayE constantE] y-evt-stream time-delay))

(define-symbolic* timestamp1 integer?)
(define-symbolic* x-val1 integer?)
(define-symbolic* y-val1 integer?)
(define-symbolic* timestamp2 integer?)
(define-symbolic* x-val2 integer?)
(define-symbolic* y-val2 integer?)
(define-symbolic* timestamp3 integer?)
(define-symbolic* x-val3 integer?)
(define-symbolic* y-val3 integer?)

(assert (positive? timestamp1))
(assert (positive? x-val1))
(assert (positive? y-val1))
(assert (positive? timestamp2))
(assert (positive? x-val2))
(assert (positive? y-val2))
(assert (positive? timestamp3))
(assert (positive? x-val3))
(assert (positive? y-val3))

(displayln "Synthesize mousetail program:")

(define begin-time (current-seconds))
(define binding
    (synthesize #:forall (list timestamp1 x-val1 y-val1
                               timestamp2 x-val2 y-val2
                               timestamp3 x-val3 y-val3)
                #:guarantee (begin (same spec-mouse-tail-x-graph
                                         op-sketch-mouse-tail-x-graph
                                         (list (list timestamp1 x-val1)
                                               (list timestamp2 x-val2)
                                               (list timestamp3 x-val3)))
                                   (same spec-mouse-tail-y-graph
                                         op-sketch-mouse-tail-y-graph
                                         (list (list timestamp1 y-val1)
                                               (list timestamp2 y-val2)
                                               (list timestamp3 y-val3))))
                                         
                              ))
(if (unsat? binding)
    (displayln "No binding was found.")
    (print-forms binding))
(define end-time (current-seconds))
(printf "Took ~a seconds~n" (- end-time begin-time))
