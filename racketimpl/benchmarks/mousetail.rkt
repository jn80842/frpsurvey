#lang rosette/safe

(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")

(provide (all-defined-out))

(current-bitwidth 5)

(define stream-length 10)
(define time-delay 3)
(define x-offset 5)

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