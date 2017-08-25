#lang rosette/safe

(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")

(provide mousetail-x-assumptions mousetail-y-assumptions)

(current-bitwidth 5)

(define time-delay 3)
(define x-offset 5)

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

(define stream-length 4)

(define mouse-x (new-event-stream sym-integer stream-length))
(define mouse-y (new-event-stream sym-integer stream-length))

(printf "current bitwidth ~a, maximum possible value is ~a~n"
        (current-bitwidth) (max-for-current-bitwidth (current-bitwidth)))
(printf "length of mouse-x ~a~n" (length mouse-x))
(printf "length of mouse-y ~a~n" (length mouse-y))

(define (mousetail-x-assumptions mx)
  (and (valid-timestamps? mx)
       ;; timestamp is no longer than number of events in stream
       (andmap (λ (e) (<= (get-timestamp e) stream-length)) mx)
       ;; x coord must be 0 or higher
       (andmap (λ (n) (>= n 0)) (map get-value mx))
       ;; to prevent overflow, x coord can't be greater than max minus offset
       (andmap (λ (n) (<= n (- (max-for-current-bitwidth (current-bitwidth)) x-offset)))
               (map get-value mx))    
       ))

(define (mousetail-y-assumptions my)
  (and (valid-timestamps? my)
       ;; timestamp is no longer than number of events in stream
       (andmap (λ (e) (<= (get-timestamp e) stream-length)) my)
       ;; y coord must be 0 or higher
       (andmap (λ (n) (>= n 0)) (map get-value my))
       ;; to prevent overflow, y coord can't be greater than max
       (andmap (λ (n) (<= n (max-for-current-bitwidth (current-bitwidth))))
               (map get-value my))
       ))

(define (mousetail-assumptions mx my)
  (and (mousetail-x-assumptions mx)
       (mousetail-y-assumptions my)
       ;; there's a value for x and y at every timestamp
       (andmap (λ (x y) (equal? (get-timestamp x) (get-timestamp y))) mx my)))

(check-existence-of-solution mousetail-assumptions mouse-x mouse-y)

(define (mousetail-y-guarantees my-in my-out)
  (and (valid-timestamps? my-out)
       ;; each timestamp is delayed by time-delay value
       (andmap (λ (in out) (equal? (+ (get-timestamp in) time-delay) (get-timestamp out))) my-in my-out)
       ;; y coord for each timestamp is not changed
       (andmap (λ (in out) (equal? (get-value in) (get-value out))) my-in my-out)))

(define (mousetail-x-guarantees mx-in mx-out)
  (and (valid-timestamps? mx-out)
       ;; each timestamp is delayed by time-delay value
       (andmap (λ (in out) (equal? (+ (get-timestamp in) time-delay) (get-timestamp out))) mx-in mx-out)
       ;; x coord for each timestamp is increased by x-offset
       (andmap (λ (in out) (equal? (+ (get-value in) x-offset) (get-value out))) mx-in mx-out)))

(define (mousetail-guarantees mx-in my-in mx-out my-out)
  (and (mousetail-y-guarantees my-in my-out)
       (mousetail-x-guarantees mx-in mx-out)
       ;; need a value for x and y mouse coordinates for every timestamp
       (andmap (λ (in out) (equal? (get-timestamp in) (get-timestamp out))) mx-out my-out)))

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