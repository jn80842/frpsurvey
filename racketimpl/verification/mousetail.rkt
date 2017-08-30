#lang rosette/safe

(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")
(require "../benchmarks/mousetail.rkt")

(define (mouse-tail-graph mouse-x-event-stream mouse-y-event-stream)
  (let ([tail-x (mapE (λ (e) (list (get-timestamp e) (+ (get-value e) x-offset))) (delayE mouse-x-event-stream time-delay))]
        [tail-y (delayE mouse-y-event-stream time-delay)])
    (list (tail-x) (tail-y))))

(define (mouse-tail-y-graph y-evt-stream)
  (delayE time-delay y-evt-stream))

(define (mouse-tail-x-graph x-evt-stream)
  (mapE (λ (e) (list (get-timestamp e) (+ (get-value e) x-offset))) (delayE time-delay x-evt-stream)))

(define (concrete-eval x-input y-input)
  (let ([output (mouse-tail-graph (λ () x-input) (λ () y-input))])
    (list (first output) (second output))))
(define (concrete-eval-y y-input)
  (let ([output (mouse-tail-y-graph (λ () y-input))])
    (output)))

(define mouse-x (new-event-stream sym-integer stream-length))
(define mouse-y (new-event-stream sym-integer stream-length))

(printf "current bitwidth ~a, maximum possible value is ~a~n"
        (current-bitwidth) (max-for-current-bitwidth (current-bitwidth)))
(printf "length of mouse-x ~a~n" (length mouse-x))
(printf "length of mouse-y ~a~n" (length mouse-y))

(check-existence-of-solution mousetail-assumptions mouse-x mouse-y)

(displayln "Verify mousetail spec")

(define begin-time (current-seconds))

(define verified (verify
                  #:assume (assert (mousetail-assumptions mouse-x mouse-y))
                  #:guarantee (assert (mousetail-guarantees mouse-x mouse-y (mouse-tail-x-graph mouse-x) (mouse-tail-y-graph mouse-y)))))

(if (unsat? verified)
    (displayln "Spec is verified.")
    (printf "Model that violates spec is found: mouse-x ~a, mouse-y ~a~n" (evaluate mouse-x verified) (evaluate mouse-y verified)))

(define end-time (current-seconds))
(printf "Took ~a seconds~n" (- end-time begin-time))