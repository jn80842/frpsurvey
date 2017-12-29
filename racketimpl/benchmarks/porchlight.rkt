#lang rosette/safe

(require "../fjmodels.rkt")
(require "../rosettefjapi.rkt")

(provide (all-defined-out))

;;                    mergeE
;;               /            \
;;            blindE          calmE
;;          /      \          /    \
;;         int   constantE   int   constantE
;;               /      \           /       \
;;              'on   md-events   'off     md-events

;;;;; motion detector and porch light
(define delay-by 2)
(define calm-by 5)

(current-bitwidth 6)

(define stream-length 1)

(define s-motion (new-event-stream (sym-union-constructor 'motion 'no-evt) stream-length (* 2 stream-length)))
(define s-motionB (new-behavior (sym-union-constructor 'motion-sensed 'motion-not-sensed) stream-length (* 2 stream-length)))

(define concrete-motionB (behavior 'motion-not-sensed (list (list 3 'motion-sensed)
                                                            (list 7 'motion-not-sensed)
                                                            (list 10 'motion-sensed)
                                                            (list 11 'motion-not-sensed))))

(define (light-graph md-events)
  (mergeE (blindE delay-by (constantE 'on md-events)) (calmE calm-by (constantE 'off md-events))))

(define (light-graph2 md-events)
  (mergeE (blindE delay-by (ifE (notE (blindE 2 md-events)) (constantE 'on md-events)))
          (calmE calm-by (constantE 'off md-events))))

(define (light-graphB mdB)
  (liftB (λ (b) (if b 'on 'off))
         (orB (liftB (λ (b) (equal? b 'motion-sensed)) mdB)
              (orB (delayB 1 (liftB (λ (b) (equal? b 'motion-sensed)) mdB))
                   (delayB 2 (liftB (λ (b) (equal? b 'motion-sensed)) mdB))))))

(define (light-graphB2 mdB)
  (define off-events (constantE #f (filterE (λ (e) (equal? e 'motion-not-sensed)) (changes mdB))))
  (define on-events (constantE #t (filterE (λ (e) (equal? e 'motion-sensed)) (changes mdB))))
  (define processedB (startsWith #f (mergeE (delayE 3 off-events) on-events)))
  (orB (liftB (λ (v) (equal? 'motion-sensed v)) mdB) processedB))

(define (light-assumptions motion)
  ;(and
   (valid-timestamps? motion)
       ;; guard against overflow
   ;    (andmap (λ (t) (> (max-for-current-bitwidth (current-bitwidth))
   ;                      (+ delay-by t))) (map get-timestamp motion))
  )

(define (light-assumptionsB motionB)
  (valid-behavior? motionB))

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

;; let phi(t) be "motion detected at time t"
;; let psi(t) be "porchlight is on at time t"
;; for all t, phi(t) -> psi(t) & X psi(t) & XX psi(t) & XXX psi(t)
;; for all t, ! phi(t) & X ! phi(t) & XX ! phi(t) & XXX ! phi(t) & XXXX ! phi(t) -> XXXX psi(t)

(define (light-guaranteesB motion porchlight)
  (and (behavior-check (λ (m p) (implication (equal? m 'motion-sensed)) (equal? p 'on)) motion porchlight)
       
  ))