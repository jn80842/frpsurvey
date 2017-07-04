#lang rosette/safe

(require "../rosettefjapi.rkt")

(current-bitwidth 5)

;; sprinklers need to run every day for 10 minutes, unless it rains
;; sprinklers should go on at 1800
;; if motion sensor goes off, pause sprinklers until motion is no longer sensed

(define s-raingauge (boolean-behavior (list 1 2 3)))
(define s-clock (integer-behavior (list 1 2 3)))
(define s-motion-sensor (boolean-behavior (list 1 2 3))) ;; or, should this be event stream?

;; did it rain the past 24 hours?
(define (24h-rainedB clockB rain-gaugeB)
  (startsWith
   (collectE 
    (mergeE
     (mapE (λ (e) (list (get-timestamp e) 'midnight)) (filterE (changes clockB) (λ (e) (equal? 2359 e))))
     (changes rain-gaugeB))
    #f
    (λ (new val) (cond [(equal? new 'midnight) #f]
                       [new #t]
                       [else val])))
   #f))

;; #t if sprinklers should turn on, 'sensed if motion sensors go off, #f otherwise
(define (start-or-pause-graphE clockB motionSensorB 24h-rainedB)
  (changes
   (condB (list (list (andB (notB 24h-rainedB) (liftB (λ (t) (equal? t 1800)) clockB)) (constantB 10))
                (list motionSensorB (constantB 'sensed))
                (list (constantB #t) (constantB #f))))))

;; returns # of minutes to count down for sprinklers on
(define (sprinkler-counter-graph clockB motionSensorB 24h-rainedB start-or-pauseE)
  (startsWith
   (collectE
    start-or-pauseE
    0
    (λ (new val) (if (equal? new 10)
                     10
                     (if (equal? val 0)
                         0
                         (if (equal? new 'sensed)
                             val
                             (sub1 val))))))
  0))

;; for later: try to write a version that uses timerB
(define (sprinklers-graph motionSensorB sprinkler-counterB)
  ;; mask times with sensor
  (condB (list (list motionSensorB (constantB 'off))
          (list (liftB (λ (t) (not (equal? t 0))) sprinkler-counterB) (constantB 'on))
          (list (constantB #t) (constantB 'off)))))

(define b-raingauge (behavior #f (list (list 1 #t) (list 3 #f))))
(define b-motion-sensor (behavior #f (list (list 4 #t) (list 5 #f) (list 6 #t) (list 7 #f) (list 10 #t) (list 11 #t) (list 12 #f))))
;; note: graph won't work unless clock ticks for every minute the sprinklers should be on
;; seems reasonable for a real implementation, think about how 
(define b-clock (behavior 0 (list (list 1 1530) (list 2 1700) (list 3 1800) (list 4 1805) (list 5 2359) (list 6 800) (list 7 1305)
                                  (list 8 1800) (list 9 1801) (list 10 1802) (list 11 1803) (list 12 1804) (list 13 1805) (list 14 1806)
                                  (list 15 1807) (list 16 1808) (list 17 1809) (list 18 1810) (list 19 1811) (list 20 1812) (list 21 1813))))
(define b-24h-rained (24h-rainedB b-clock b-raingauge))
(define b-cond (condB (list (list (andB (notB b-24h-rained) (liftB (λ (t) (equal? t 1800)) b-clock)) (constantB 10))
                  (list b-motion-sensor (constantB 'sensed))
                  (list (constantB #t) (constantB #f)))))

(define expected-sprinklers (behavior 'off (list (list 1 'off) (list 2 'off) (list 3 'off) (list 4 'off) (list 5 'off) (list 6 'off) (list 7 'off)
                                                 (list 8 'on) (list 9 'on) (list 10 'off) (list 11 'off) (list 12 'on) (list 13 'on) (list 14 'off)
                                                 (list 14 'on) (list 15 'on) (list 16 'on) (list 17 'on) (list 18 'on) (list 19 'on) (list 20 'off) (list 21 'off))))

(define (24h-rainedB-assumptions clockB raingaugeB)
  (and (valid-behavior? clockB)
       (valid-behavior? raingaugeB)))


(define solved (solve (assert (24h-rainedB-assumptions s-clock s-raingauge))))
