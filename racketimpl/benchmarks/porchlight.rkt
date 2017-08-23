#lang rosette/safe

(require "../fjmodels.rkt")
(require "../rosettefjapi.rkt")


;;;;  1 2         7 8
;;; f w o         w o
;;;               f

;;;;  1 2         7
;;; f w o         w
;;;               f

;;;;; motion detector and porch light
(define delay-by 5)
(define calm-by 5)

(current-bitwidth 6)

(if (>= delay-by (max-for-current-bitwidth (current-bitwidth)))
    (displayln "DELAY BY IS TOO HIGH and WILL CAUSE OVERFLOW")
    (printf "delay-by is ~a~n" delay-by))

(define (light-graph md-events)
  (mergeE (blindE (constantE md-events 'on) delay-by) (calmE (constantE md-events 'off) calm-by)))

(define (light-graph2 md-events)
  (mergeE (blindE (ifE (notE (blindE md-events 2)) (constantE md-events 'on)) delay-by)
          (calmE (constantE md-events 'off) calm-by)))

(define concrete-motion (list (list 1 'd) (list 3 'd) (list 10 'd) (list 20 'd)))
(define m (list (list 18 'd) (list 23 'd)))

(define concrete-motion-ts '((1 1) (3 3) (10 10) (20 20)))

(if (eq? (light-graph concrete-motion)
         '((1 on) (8 off) (10 on) (15 off) (20 on) (25 off)))
    (displayln "graph is correct on concrete inputs")
    (displayln "graph fails on concrete inputs"))

(define stream-length 2)

(define s-motion (new-event-stream (sym-union-constructor 'motion 'no-evt) stream-length))

(printf "current bitwidth is: ~a\n" (current-bitwidth))
(printf "number of motion detector events: ~a\n" stream-length)

(define (light-assumptions motion)
  (and (valid-timestamps? motion)
       ;; guard against overflow
       (andmap (λ (t) (> (max-for-current-bitwidth (current-bitwidth))
                         (+ delay-by t))) (map get-timestamp motion))
  ))

(check-existence-of-solution light-assumptions s-motion)

(define (light-guarantees motion)
  (let ([porchlight (light-graph2 motion)])
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
         )))
(define begin-time (current-seconds))
(define verified (verify #:assume (assert (light-assumptions s-motion))
                         #:guarantee (assert (light-guarantees s-motion))))
(define end-time (current-seconds))
(printf "time to verify: ~a seconds~n" (- end-time begin-time))
(if (unsat? verified)
    (displayln "Spec is verified")
    (printf "Model that violates spec is found: motion ~a~n" (evaluate s-motion verified)))