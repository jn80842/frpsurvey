#lang s-exp rosette/safe

(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")

(provide button-assumptions)

(current-bitwidth 5)

(define (inc-dec-button-graph inc dec)
  (startsWith 0
   (collectE 0 +
   (mergeE (constantE 1 inc) (constantE -1 dec)))))

(define concrete-inc-clicks (list (list 1 'click) (list 4 'click)))
(define concrete-dec-clicks (list (list 2 'no-evt) (list 3 'no-evt) (list 5 'click)))

(displayln (inc-dec-button-graph concrete-inc-clicks concrete-dec-clicks))

(define stream-length 3)

(define s-inc (new-event-stream (sym-union-constructor 'click 'no-evt) stream-length))
(define s-dec (new-event-stream (sym-union-constructor 'click 'no-evt) stream-length))

(printf "current bitwidth ~a, maximum possible value is ~a~n"
        (current-bitwidth) (max-for-current-bitwidth (current-bitwidth)))
(printf "length of increase clicks ~a~n" (length s-inc))
(printf "length of decrease clicks ~a~n" (length s-dec))

(define (concrete-eval inc dec)
  (inc-dec-button-graph (λ () inc) (λ () dec)))

(define (button-assumptions inc-stream dec-stream)
    (and (valid-timestamps? inc-stream)
         (valid-timestamps? dec-stream)
         (timestamps-below-max? (* 2 stream-length) inc-stream)
         (timestamps-below-max? (* 2 stream-length) dec-stream)
         ;; TODO: figure out better solutions for simultaneous events
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

(check-existence-of-solution button-assumptions s-inc s-dec)

(define begin-time (current-seconds))
(define verified (verify
                  #:assume (assert (button-assumptions s-inc s-dec))
                  #:guarantee (assert (button-guarantees (inc-dec-button-graph s-inc s-dec)))
                  ))
(define end-time (current-seconds))
(printf "time to verify: ~a seconds~n" (- end-time begin-time))
(if (unsat? verified)
    (displayln "Spec is verified.")
    (printf "Model that violates spec is found: increase stream ~a, decrease stream ~a~n" (evaluate s-inc verified) (evaluate s-dec verified)))