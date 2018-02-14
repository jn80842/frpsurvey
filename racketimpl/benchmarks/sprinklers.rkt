#lang rosette/safe

(require "../densefjapi.rkt")
(require "../dense-fjmodels.rkt")

(provide (all-defined-out))

;;                              condB
;;            /                   |                      \
;;          pair                 pair                     pair
;;     /           \            /    \                   /    \
;;  motionSensorB  constantB  liftB2   constantB    constantB    constantB
;;                     |     /   |   \       |         |            |
;;                  'off   λ collectB clockB   'on    #t          'off
;;                            /  |   \
;;                          λ   #f   liftB2
;;                                 /    |    \
;;                              λ  raingaugeB clockB

;;                   ifB
;;         /          |            \
;; motionSensorB  constantB        ifB
;;                  |        /         |        \
;;                 'off   liftB2   constantB  constantB
;;                     /   |   \         |        |
;;                  λ  collectB clockB  'on      'off

;(current-bitwidth 5)
(define stream-length 3)

(define s-raingaugeB (new-behavior get-sym-bool stream-length))
(define s-clockB (new-behavior sym-time-vec stream-length))
(define s-motionSensorB (new-behavior get-sym-bool stream-length)) ;; or, should this be event stream?

(define small-raingaugeB (behavior #f (list #t #f)))
(define small-clockB (behavior (vector 17 5 9) (list (vector 18 0 1) (vector 18 2 0))))
(define small-motionSensorB (behavior #f (list #f #f)))
(define small-sprinklersB (behavior 'off '(off off)))

(define concrete-raingaugeB (behavior #f (list #t #t #f #f #f #f #f #f #f #f #f #f #f)))
(define concrete-clockB (behavior (vector 15 3 0) (list (vector 17 0 0)
                                                        (vector 18 0 0)
                                                        (vector 18 0 5)
                                                        (vector 0 0 0) ;; midnight
                                                        (vector 8 0 0)
                                                        (vector 13 0 5)
                                                        (vector 18 0 0)
                                                        (vector 18 0 3)
                                                        (vector 18 0 6)
                                                        (vector 18 0 7)
                                                        (vector 18 0 8)
                                                        (vector 18 1 1)
                                                        (vector 18 1 3))))
(define concrete-motionSensorB (behavior #f (list #f #f #t #f #t #f #f #f #t #t #f #f #f)))
(define concrete-sprinklersB (behavior 'off '(off off off off off off on on off off on off off)))

(define (saw-rain-last-dayB raingaugeB clockB)
  (collectB #f (λ (r prev) (if (eq? r 'midnight) #f
                               (if r #t prev)))
            (liftB2 (λ (rain clock) (if (is-midnight? clock) 'midnight rain)) raingaugeB clockB)))

(define (sprinklers-graph clockB motionSensorB raingaugeB)
  (condB (list (list motionSensorB (constantB 'off clockB))
               (list (liftB2 (λ (rain clock) (and (not rain)
                                                  (eq? (time-vec-hour clock) 18)
                                                  (< (time-vec-min1 clock) 1)))
                             (saw-rain-last-dayB raingaugeB clockB) clockB) (constantB 'on clockB))
               (list (constantB #t clockB) (constantB 'off clockB)))))
