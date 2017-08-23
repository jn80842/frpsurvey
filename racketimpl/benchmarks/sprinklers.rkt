#lang rosette/safe

(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")

(define i-raingaugeB (behavior #f (list (list 1 #t) (list 3 #f))))
;; does it make more sense for motion sensor to be an event stream?
(define i-motion-sensorB (behavior #f (list (list 4 #t) (list 5 #f) (list 6 #f) (list 7 #t) (list 10 #t) (list 12 #f))))
(define i-clockE (list (list 1 (vector 15 3 0)) (list 2 (vector 17 0 0)) (list 3 (vector 18 0 0))
                      (list 4 (vector 18 0 5)) (list 5 (vector 0 0 0)) (list 6 (vector 8 0 0))
                      (list 7 (vector 13 0 5)) (list 8 (vector 18 0 0)) (list 9 (vector 18 0 3))
                      (list 10 (vector 18 0 6)) (list 11 (vector 18 0 7)) (list 12 (vector 18 0 8))
                      (list 13 (vector 18 1 1)) (list 14 (vector 18 1 3))))

(define o-24h-rained (behavior #f (list (list 1 #t) (list 5 #f))))
(define o-sprinkler (behavior 'off (list (list 8 'on) (list 10 'off) (list 12 'on) (list 14 'off))))

(current-bitwidth 5)

;; sprinklers need to run every day for 10 minutes, unless it rains
;; sprinklers should go on at 1800
;; if motion sensor goes off, pause sprinklers until motion is no longer sensed

;; did it rain during the past day?
(define (24h-rainedB clockE rain-gaugeB)
  (startsWith
   (collectE 
    (mergeE
     (mapE (λ (e) (list (get-timestamp e) 'midnight)) (filterE clockE (λ (e) (equal? 0 (time-vec->integer e)))))
     (changes rain-gaugeB))
    #f
    (λ (new val) (cond [(equal? new 'midnight) #f]
                       [new #t]
                       [else val])))
   #f))

(display "checking 24h-rainedB...")
(if (equal-behaviors? (24h-rainedB i-clockE i-raingaugeB) o-24h-rained)
    (displayln "ok!")
    (displayln "something's wrong."))

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

(define stream-length 3)

(define s-raingauge (new-behavior sym-boolean stream-length))
(define s-clock (new-behavior sym-integer stream-length))
(define s-motion-sensor (new-behavior sym-boolean stream-length)) ;; or, should this be event stream?

(define (24h-rainedB-assumptions clockE raingaugeB)
  (and (valid-timestamps? clockE)
       ;; we need midnights to appear explicitly
       ;; if any time follows a time of higher value, it must be midnight
       (foldl (λ (new val) (cond [(not val) #f]
                                 [(or (< (time-vec->integer val) (time-vec->integer (get-value new)))
                                      (eq? (time-vec->integer (get-value new)) 0)) #f]
                                 [else (get-value new)])) (get-value (first clockE)) (list-tail clockE 1))
       (valid-behavior? raingaugeB)
       ))


(define solved (solve (assert (24h-rainedB-assumptions s-clock s-raingauge))))
