#lang rosette ;/safe

(require "../rosettefjapi.rkt")

(current-bitwidth 4)

(define (get-inputs concrete-list)
  (map (λ (c)
         (define-symbolic* timestamp integer?)
         (define-symbolic* value integer?)
         (list timestamp value)) concrete-list))

(define time-delay 3)
(define x-offset 5)

(define (mouse-tail-graph mouse-x-event-stream mouse-y-event-stream)
  (let ([tail-x (mapE (λ (e) (list (get-timestamp e) (+ (get-value e) x-offset))) (delayE mouse-x-event-stream time-delay))]
        [tail-y (delayE mouse-y-event-stream time-delay)])
    (list (tail-x) (tail-y))))

(define (mouse-tail-y-graph y-evt-stream)
  (delayE y-evt-stream time-delay))

(define (mouse-tail-x-graph x-evt-stream)
  (mapE (λ (e) (list (get-timestamp e) (+ (get-value e) x-offset))) (delayE x-evt-stream time-delay)))

(define (concrete-eval x-input y-input)
  (let ([output (mouse-tail-graph (λ () x-input) (λ () y-input))])
    (list (first output) (second output))))
(define (concrete-eval-y y-input)
  (let ([output (mouse-tail-y-graph (λ () y-input))])
    (output)))

(define mouse-x (get-inputs (list 1 2)))
(define mouse-y (get-inputs (list 1 2)))

(printf "current bitwidth ~a~n" (current-bitwidth))
(printf "length of mouse-x ~a~n" (length mouse-x))
(printf "length of mouse-y ~a~n" (length mouse-y))

(define (assert-mousetail-x-assumptions mx)
  (begin
    (assert-valid-input-timestamps? mx)
    ;; x mouse coordinates cannot be negative
    (assert (andmap (λ (n) (not (negative? n))) (map get-value mx)))))

(define (assert-mousetail-y-assumptions my)
  (begin
    (assert-valid-input-timestamps? my)
    ;; y mouse coordinates cannot be negative
    (assert (andmap (λ (n) (not (negative? n))) (map get-value my)))))

(define (assert-mousetail-assumptions mx my)
  (assert-mousetail-x-assumptions mx)
  (assert-mousetail-y-assumptions my)
  ;; need a value for x and y mouse coordinates for every timestamp
  (assert (andmap (λ (x y) (equal? (get-timestamp x) (get-timestamp y))) mx my)))

;; check to make sure assumptions are satisfiable
(define solved (solve (assert-mousetail-assumptions mouse-x mouse-y)))

(if (unsat? solved)
    (displayln "no solution for assumptions")
    (begin
      (displayln "sample solution for assumptions:")
      (displayln (evaluate mouse-x solved))
      (displayln (evaluate mouse-y solved))))

(define (assert-mousetail-y-guarantees my-in my-out)
  (begin
    (assert-valid-output-timestamps? my-out)
    ;; each timestamp is delayed by time-delay value
    (assert (andmap (λ (in out) (equal? (+ (get-timestamp in) time-delay) (get-timestamp out))) my-in my-out))
    ;; y coord for each timestamp is not changed
    (assert (andmap (λ (in out) (equal? (get-value in) (get-value out))) my-in my-out))))

(define (assert-mousetail-x-guarantees mx-in mx-out)
  (begin
    (assert-valid-output-timestamps? mx-out)
    ;; each timestamp is delayed by time-delay value
    (assert (andmap (λ (in out) (equal? (+ (get-timestamp in) time-delay) (get-timestamp out))) mx-in mx-out))
    ;; x coord for each timestamp is increased by x-offset
    (assert (andmap (λ (in out) (equal? (+ (get-value in) x-offset) (get-value out))) mx-in mx-out))))

(define (assert-mousetail-guarantees mx-in my-in mx-out my-out)
  (begin
    (assert-mousetail-x-guarantees mx-in mx-out)
    (assert-mousetail-y-guarantees my-in my-out)
    ;; need a value for x and y mouse coordinates for every timestamp
    (assert (andmap (λ (in out) (equal? (get-timestamp in) (get-timestamp out))) mx-out my-out))))

(displayln "Verify mousetail spec")

(define begin-time (current-seconds))

(define verified (verify
                  #:assume (assert-mousetail-assumptions mouse-x mouse-y)
                  #:guarantee (assert-mousetail-guarantees mouse-x mouse-y ((mouse-tail-x-graph (λ () mouse-x))) ((mouse-tail-y-graph (λ () mouse-y))))
                  ))

(if (unsat? verified)
    (displayln "Spec is verified.")
    (printf "Model that violates spec is found: mouse-x ~a, mouse-y ~a~n" (evaluate mouse-x verified) (evaluate mouse-y verified)))

#;(define verified-x (verify
                    #:assume (assert-mousetail-x-assumptions mouse-x)
                    #:guarantee (assert-mousetail-x-guarantees mouse-x ((mouse-tail-x-graph (λ () mouse-x)))
                                                               )))
#;(define verified-y (verify
                    #:assume (assert-mousetail-y-assumptions mouse-y)
                    #:guarantee (assert-mousetail-y-guarantees mouse-y ((mouse-tail-y-graph (λ () mouse-y))))
                                    ))

(define end-time (current-seconds))
(printf "Took ~a seconds~n" (- end-time begin-time))