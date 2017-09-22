#lang rosette

(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")

(provide (all-defined-out))

(current-bitwidth 5)
(define stream-length 2)

(define (inc-dec-button-graph inc dec)
  (startsWith 0
   (collectE 0 +
   (mergeE (constantE 1 inc) (constantE -1 dec)))))

(define concrete-inc-clicks (list (list 2 'click) (list 4 'click)))
(define concrete-dec-clicks (list (list 5 'click) (list 6 'click) (list 7 'click)))
(define concrete-counter (behavior 0 (list (list 2 1) (list 4 2) (list 5 1) (list 6 0) (list 7 -1))))

(define s-inc (new-event-stream (sym-union-constructor 'click 'no-evt) stream-length (* 2 stream-length)))
(define s-dec (new-event-stream (sym-union-constructor 'click 'no-evt) stream-length (* 2 stream-length)))

(define (button-assumptions inc-stream dec-stream)
    (and (timestamps-sorted-and-distinct? inc-stream)
         (timestamps-sorted-and-distinct? dec-stream)
         (apply distinct? (map get-timestamp (append inc-stream dec-stream)))
         ))

(define (button-guarantees output-behavior)
  (and (valid-behavior? output-behavior)
       ;; starting value of field is 0
       (equal? 0 (behavior-init output-behavior))
       ;; values at each timestamp are integers (needed?)
       (andmap integer? (map get-value (behavior-changes output-behavior)))
       ;; the last value is the difference between the number of increase clicks and the number of decrease clicks
       ;; TODO: this should actually be true for every point in the timeline
       (equal? (get-value (last (behavior-changes output-behavior)))
               (- (length (filter (λ (e) (equal? 'click (get-value e))) s-inc))
                  (length (filter (λ (e) (equal? 'click (get-value e))) s-dec))))))