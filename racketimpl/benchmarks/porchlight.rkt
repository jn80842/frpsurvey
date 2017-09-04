#lang rosette/safe

(require "../fjmodels.rkt")
(require "../rosettefjapi.rkt")

(provide (all-defined-out))

;;;;; motion detector and porch light
(define delay-by 5)
(define calm-by 5)

(current-bitwidth 6)

(define stream-length 2)

(define s-motion (new-event-stream (sym-union-constructor 'motion 'no-evt) stream-length))

(define (light-assumptions motion)
  (and (valid-timestamps? motion)
       ;; guard against overflow
       (andmap (λ (t) (> (max-for-current-bitwidth (current-bitwidth))
                         (+ delay-by t))) (map get-timestamp motion))
  ))

(define (light-guarantees motion porchlight)
  (and (valid-timestamps? porchlight)
       ;; for every on event, there is a motion event at that timestamp
       (andmap (λ (t) (member t (map get-timestamp motion)))
               (map get-timestamp (filter (λ (e) (eq? 'on (get-value e))) porchlight)))
       ;; for every off event, there is a motion event (delay) seconds earlier
       (andmap (λ (t) (member t (map (λ (e) (+ calm-by (get-timestamp e))) motion)))
               (map get-timestamp (filter (λ (e) (eq? 'off (get-value e))) porchlight)))
       ;; on and off events alternate; on should come first
       (foldl (λ (new val) (cond [(false? val) #f]
                                 [(eq? new val) #f]
                                 [else new])) 'off (map get-value porchlight))
         ))