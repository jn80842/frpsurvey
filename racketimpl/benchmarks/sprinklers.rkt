#lang rosette/safe

(require "../rosettefjapi.rkt")

(current-bitwidth 5)

;; sprinklers need to run every day for 10 minutes, unless it rains
;; sprinklers should go on at 1800
;; if motion sensor goes off, pause sprinklers until motion is no longer sensed

(define (sprinkler-graph clock rain-gauge motion-sensor)
  #t
  )

(define s-rain-guage (boolean-behavior (list 1 2 3)))
(define s-clock (integer-behavior (list 1 2 3)))
(define s-motion-sensor (boolean-behavior (list 1 2 3))) ;; or, should this be event stream?

(define (24h-rainedB clockE rain-gaugeB)
(startsWith
  (collectE
    (mergeE (mapE clockE (λ (c) (if (equal? (get-value c) 2359) 'midnight #f))) (changes rain-gaugeB))
    #f
    (λ (new val) (if (equal? val 'midnight) #f (or new val))))
#f))

(define (sprinklers-graph clockB motionSensorB 24h-rainedB)
  (startsWith
   (collectE
    (changes
     (condB (list (andB (notB 24h-not-rainedB) (liftB clockB (λ (t) (equal? (get-value t) 1800)))) (constantB 10))
            (list motionSensorB (constantB 'sensed))
            (list (constantB #t) (constantB #t))))
    0
    (λ (new val) (if (equal? new 10)
                     10
                     (if (equal? val 0)
                         0
                         (if (equal? new 'sensed)
                             val
                             (sub1 val))))))
   'off))

(behavior 'off (list (list 1800 'on) (list 1804 'off) (list 1805 'on) (list 1810 'off)))

;; for later: try to write a version that uses timerB



  

  