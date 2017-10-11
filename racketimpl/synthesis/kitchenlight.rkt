#lang rosette/safe
(require rosette/lib/synthax)

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../benchmarks/kitchenlight.rkt")

;;            liftB
;;         /    |       \
;;       λ    liftB         liftB
;;        /     |          /  |   \
;;      λ  motionSensorB  λ clockB locationB

(define-synthax (flapjax-grmr input ... depth)
  #:base (choose input ...)
  #:else (let ([recursive1 (flapjax-grmr input ... (sub1 depth))]
               [recursive2 (flapjax-grmr input ... (sub1 depth))])
           (choose input ...
                   (liftB1 (λ (e) (if e 'on 'off)) (flapjax-grmr input ... (sub1 depth)))
                   (liftB2 (choose (λ (clock location) (if (or (>= clock hour-begin) (< clock hour-end))
                                 'night
                                 (if (equal? location 'home)
                                     'home
                                     'away)))
                                   (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none)))
                           recursive1 recursive2)
                 )))



(define (synth-graph clockB userLocationB motionSensorB)
  (flapjax-grmr clockB userLocationB motionSensorB 3))

;(assert (light-color-assumptions s-clockB s-locationB s-motion-sensorB))
;; limit range of timestamps, lock down minutes
#;(assert (and (andmap (λ (ts) (>= (* 4 stream-length) ts)) (append (map get-timestamp (behavior-changes s-clockB))
                                                                  (map get-timestamp (behavior-changes s-locationB))
                                                                  (map get-timestamp (behavior-changes s-motionSensorB))))
             (behavior-check (λ (v) (and (= (vector-ref v 1) 0) (= (vector-ref v 2) 0))) s-clockB)))

(displayln "Synthesize kitchen light program")

(define begin-time (current-seconds))
(define binding
  (synthesize #:forall (append (harvest s-clockB)
                               (harvest s-locationB)
                               (harvest s-motion-sensorB))
              #:guarantee (assert ;(same kitchen-light-graph synth-graph s-clockB s-locationB s-motionSensorB)
                           (same kitchen-light-graph synth-graph s-clockB s-locationB s-motion-sensorB)
                                  )))
(define end-time (current-seconds))
(if (unsat? binding)
    (displayln "No binding was found.")
    (print-forms binding))
(printf "Took ~a seconds~n" (- end-time begin-time))
