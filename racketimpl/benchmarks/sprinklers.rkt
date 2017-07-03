#lang rosette/safe

(require "../rosettefjapi.rkt")

(current-bitwidth 5)

;; sprinklers need to run every day for 10 minutes, unless it rains
;; sprinklers should go on at 1800
;; if motion sensor goes off, pause sprinklers until motion is no longer sensed

(define s-rain-guage (boolean-behavior (list 1 2 3)))
(define s-clock (integer-behavior (list 1 2 3)))
(define s-motion-sensor (boolean-behavior (list 1 2 3))) ;; or, should this be event stream?

(define (24h-rainedB clockB rain-gaugeB)
  (startsWith
   (collectE 
    (mergeE
     (mapE (λ (e) (list (get-timestamp e) 'midnight)) (filterE (changes clockB) (λ (e) (equal? 0 e))))
     (changes rain-gaugeB))
    #f
    (λ (new val) (cond [(equal? new 'midnight) #f]
                       [new #t]
                       [else val])))
   #f))

;; for later: try to write a version that uses timerB
(define (sprinklers-graph clockB motionSensorB 24h-rainedB)
  (startsWith
   (collectE
    (changes
     (condB (list (list (andB (notB 24h-rainedB) (liftB (λ (t) (equal? t 1800)) clockB)) (constantB 10))
                  (list motionSensorB (constantB 'sensed))
                  (list (constantB #t) (constantB #t)))))
    0
    (λ (new val) (if (equal? new 10)
                     10
                     (if (equal? val 0)
                         0
                         (if (equal? new 'sensed)
                             val
                             (sub1 val))))))
   'off))

(define b-raingauge (behavior #f (list (list 1 #t) (list 3 #f))))
(define b-motion-sensor (behavior #f (list (list 4 #t) (list 5 #f) (list 6 #t) (list 7 #f) (list 10 #t) (list 11 #t) (list 12 #f))))
(define b-clock (behavior 0 (list (list 1 1530) (list 2 1700) (list 3 1800) (list 4 1805) (list 5 2359) (list 6 800) (list 7 1305) (list 8 1800) (list 9 1803) (list 10 1806)
                                  (list 11 1807) (list 12 1808) (list 13 1811) (list 14 1813))))
(define expected-sprinklers (behavior 'off (list (list 1 'off) (list 2 'off) (list 3 'off) (list 4 'off) (list 5 'off) (list 6 'off) (list 7 'off) (list 8 'on) (list 9 'on)
                                                 (list 10 'off) (list 11 'off) (list 12 'on) (list 13 'on) (list 14 'off))))


  