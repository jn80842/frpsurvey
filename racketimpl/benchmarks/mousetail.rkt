#lang rosette

(require "../densefjapi.rkt")
(require "../dense-fjmodels.rkt")

(provide (all-defined-out))

;;      mapE                    delayE
;;   /      \                   /    \
;; λ        delayE             3     mouse-y
;;         /      \
;;        3     mouse-x

(current-bitwidth 5)

(define stream-length 2)
(define time-delay 3)
(define x-offset 5)

(define (mousetail-y-graph y-evt-stream)
  (delayE3 y-evt-stream))

(define (ssa-mousetail-y-graph y-evt-stream)
  (define r1 y-evt-stream)
  (define r2 (delayE time-delay r1))
  r2)

(define (mousetail-x-graph x-evt-stream)
  (mapE (λ (e) (+ e x-offset)) (delayE3 x-evt-stream)))

(define (ssa-mousetail-x-graph x-evt-stream)
  (define r1 x-evt-stream)
  (define r2 (delayE3 r1))
  (define r3 (mapE (λ (e) (+ e x-offset)) r2))
  r3)

(define concrete-mouse-x-input (list 0 5 5 3 3 3))
(define concrete-mouse-y-input (list 0 2 2 1 1 1))
(define concrete-mouse-x-output (list 'no-evt 'no-evt 'no-evt 5 10 10 8 8 8))
(define concrete-mouse-y-output (list 'no-evt 'no-evt 'no-evt 0 2 2 1 1 1))

(define (sym-mouse-coord)
  (define-symbolic* i integer?)
  (assert (>= i 0))
  (assert (<= i (- (max-for-current-bitwidth (current-bitwidth)) x-offset)))
  i)

(define s-mouse-x (new-event-stream sym-mouse-coord stream-length))
(define s-mouse-y (new-event-stream sym-mouse-coord stream-length))

#;(define (mousetail-x-assumptions mx)
  (timestamps-sorted-and-distinct? mx))
(define (mousetail-x-assumptions mx)
  (andmap (λ (e) (not-empty-event? e)) mx))
#;(define (mousetail-y-assumptions my)
  (timestamps-sorted-and-distinct? my))
(define (mousetail-y-assumptions my)
  (andmap (λ (e) (not-empty-event? e)) my))

(define (mousetail-assumptions mx my)
  (and (mousetail-x-assumptions mx)
       (mousetail-y-assumptions my)))
       ;; there's a value for x and y at every timestamp
       ;(andmap (λ (x y) (equal? (get-timestamp x) (get-timestamp y))) mx my)))

#;(define (mousetail-y-guarantees my-in my-out)
  (and (valid-timestamps? my-out)
       ;; each timestamp is delayed by time-delay value
       (andmap (λ (in out) (equal? (+ (get-timestamp in) time-delay) (get-timestamp out))) my-in my-out)
       ;; y coord for each timestamp is not changed
       (andmap (λ (in out) (equal? (get-value in) (get-value out))) my-in my-out)))

#;(define (mousetail-x-guarantees mx-in mx-out)
  (and (valid-timestamps? mx-out)
       ;; each timestamp is delayed by time-delay value
       (andmap (λ (in out) (equal? (+ (get-timestamp in) time-delay) (get-timestamp out))) mx-in mx-out)
       ;; x coord for each timestamp is increased by x-offset
       (andmap (λ (in out) (equal? (+ (get-value in) x-offset) (get-value out))) mx-in mx-out)))

#;(define (mousetail-guarantees mx-in my-in mx-out my-out)
  (and (mousetail-y-guarantees my-in my-out)
       (mousetail-x-guarantees mx-in mx-out)
       ;; need a value for x and y mouse coordinates for every timestamp
       (andmap (λ (in out) (equal? (get-timestamp in) (get-timestamp out))) mx-out my-out)))

;; sparse encoding
#;(define concrete-mouse-x-input (list (list 1 0)
                                     (list 2 5)
                                     (list 3 5)
                                     (list 4 3)
                                     (list 5 3)
                                     (list 6 3)))
#;(define concrete-mouse-y-input (list (list 1 0)
                                     (list 2 2)
                                     (list 3 2)
                                     (list 4 1)
                                     (list 5 1)
                                     (list 6 1)))
#;(define concrete-mouse-x-output (list (list 4 5)
                                      (list 5 10)
                                      (list 6 10)
                                      (list 7 8)
                                      (list 8 8)
                                      (list 9 8)))
#;(define concrete-mouse-y-output (list (list 4 0)
                                      (list 5 2)
                                      (list 6 2)
                                      (list 7 1)
                                      (list 8 1)
                                      (list 9 1)))