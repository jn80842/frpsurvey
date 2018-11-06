#lang rosette

(require "../model.rkt")
(require "../fjapi.rkt")
(require "../operators.rkt")
(require "../sketch.rkt")

(current-bitwidth #f)

(define ENTER 0)
(define EXIT 1)

(define MOVEUP 0)
(define MOVEDOWN 1)
(define STOP 2)

(define timebound 4)
(define sTop (new-event-stream get-sym-int timebound))
(define sBottom (new-event-stream get-sym-int timebound))
(define sSteps (new-behavior get-sym-int timebound))
(define sUsers (new-behavior get-sym-int timebound))

(define (iff p q)
  (or (and p q) (and (not p) (not q))))

(define (remove-last l)
  (reverse (cdr (reverse l))))

;; LTL always/globally operator over event stream
(define (always predicate . events)
  (apply (curry andmap predicate) events))

;; redefine psi a bit
;; original psi states that various events can't occur at the same time
;; instead we state that events are enumerated values
(define (sensor sensorE)
  (andmap (λ (e) (or (empty-event? e) (equal? e ENTER) (equal? e EXIT))) sensorE))
(define (steps stepsB)
  (let ([validMovement? (λ (m) (or (equal? m STOP) (equal? m MOVEUP) (equal? m MOVEDOWN)))])
    (and (validMovement? (behavior-init stepsB))
         (andmap validMovement? (behavior-changes stepsB)))))
(define (users userCounterE)
  (andmap (λ (e) (or (empty-event? e) (equal? e 1) (equal? e -1))) userCounterE))

;; theta zero
;; ((enterEvent(bottom) && !exitEvent(top) && !(steps == MOVEDOWN))
;; || (enterEvent(top) && !exitEvent(bottom) && !(steps == MOVEUP)))
;; <-> users := users + 1
;; simplify users to a stream of +1 and -1 to user count
(define (theta0 topSensorE bottomSensorE stepsMovementB userCounterB)
  (let* ([stepsStatusE (behavior-changes stepsMovementB)]
        [calculateDiff (λ (b1 b2) (- (- b1 b2)))]
        [userCountDiffs (append (list (calculateDiff (behavior-init userCounterB) (first (behavior-changes userCounterB)))) (map calculateDiff (remove-last (behavior-changes userCounterB)) (cdr (behavior-changes userCounterB))))])
    (andmap (λ (top bottom steps userCounter)
              (iff (or (and (equal? bottom ENTER) (not (equal? top EXIT)) (not (equal? steps MOVEDOWN)))
                       (and (equal? top ENTER) (not (equal? bottom EXIT)) (not (equal? steps MOVEUP))))
                   (equal? userCounter 1))) topSensorE bottomSensorE stepsStatusE userCountDiffs)))

;; theta one
;; ((exitEvent(top) && !enterEvent(bottom) && !(steps == MOVEDOWN))
;; || (exitEvent(bottom) && !enterEvent(top) && !(steps == MOVEUP)))
;; <-> users := users - 1
(define (theta1 topSensorE bottomSensorE stepsMovementB userCounterB)
  (let* ([stepsStatusE (behavior-changes stepsMovementB)]
         [calculateDiff (λ (b1 b2) (- (- b1 b2)))]
         [userCountDiffs (append (list (calculateDiff (behavior-init userCounterB) (first (behavior-changes userCounterB)))) (map calculateDiff (remove-last (behavior-changes userCounterB)) (cdr (behavior-changes userCounterB))))])
    (andmap (λ (top bottom steps userCounter)
              (iff (or (and (equal? top EXIT) (not (equal? bottom ENTER)) (not (equal? steps MOVEDOWN)))
                       (and (equal? bottom EXIT) (not (equal? top ENTER)) (not (equal? steps MOVEUP))))
                   (equal? userCounter -1))) topSensorE bottomSensorE stepsStatusE userCountDiffs)))
;; theta two
;; (stepMovementChangeE == MOVEUP) --> userCounterB == 0 && enterEvent(bottom)
(define (theta2 topSensorE bottomSensorE stepsMovementB userCounterB)
  (let ([userCounterE (behavior-changes userCounterB)]
        [stepsMovementE (behavior-changes stepsMovementB)])
    (andmap (λ (steps users bottom)
              (implies (equal? steps MOVEUP) (and (equal? users 0) (equal? bottom ENTER))))
            stepsMovementE userCounterE bottomSensorE)))

;; theta three
;; (stepMovementChangeE == MOVEDOWN) --> userCounterB == 0 && enterEvent(top)
(define (theta3 topSensorE bottomSensorE stepsMovementB userCounterB)
  (let ([userCounterE (behavior-changes userCounterB)]
        [stepsMovementE (behavior-changes stepsMovementB)])
    (always (λ (steps users top)
              (implies (equal? steps MOVEDOWN) (and (equal? users 0) (equal? top ENTER))))
            stepsMovementE userCounterE topSensorE)))

;; theta four
;; userCounterB == 0 && enterEvent(bottom) && !enterEvent(top) --> stepsMovementChangeE == MOVEUP
(define (theta4 topSensorE bottomSensorE stepsMovementB userCounterB)
  (let ([stepsMovementChangeE (changes stepsMovementB)]
        [userCounterE (behavior-changes userCounterB)])
    (always (λ (top bottom steps users)
              (implies (and (equal? users 0) (equal? bottom ENTER) (not (equal? top ENTER)))
                       (equal? steps MOVEUP)))
            topSensorE bottomSensorE stepsMovementChangeE userCounterE)))

;; theta five
;; userCounterB == 0 && enterEvent(top) && !enterEvent(bottom) --> stepsMovementChangeE == MOVEDOWN
(define (theta5 topSensorE bottomSensorE stepsMovementB userCounterB)
  (let ([stepsMovementChangeE (changes stepsMovementB)]
        [userCounterE (behavior-changes userCounterB)])
    (always (λ (top bottom steps users)
              (implies (and (equal? users 0) (equal? top ENTER) (not (equal? bottom ENTER)))
                       (equal? steps MOVEDOWN)))
            topSensorE bottomSensorE stepsMovementChangeE userCounterE)))

;; theta six
;; userCounterB == 0 && enterEvent(top) && enterEvent(bottom) --> stepsMovementChangeE == MOVEDOWN || stepsMovementChangeE == MOVEUP
(define (theta6 topSensorE bottomSensorE stepsMovementB userCounterB)
  (let ([stepsMovementChangeE (changes stepsMovementB)]
        [userCounterE (behavior-changes userCounterB)])
    (always (λ (top bottom steps users)
              (implies (and (equal? users 0) (equal? top ENTER) (equal? bottom ENTER))
                       (or (equal? steps MOVEUP) (equal? steps MOVEDOWN))))
            topSensorE bottomSensorE stepsMovementChangeE userCounterE)))

;; theta seven
;; ! users == 0 && X (users == 0 && ! enterEvent(top) && ! enterEvent(bottom)) <-> steps := STOP
(define (theta7 topSensorE bottomSensorE stepsMovementB userCounterB)
  (let* ([userCounterE (behavior-changes userCounterB)]
         [nextUserCounterE (cdr userCounterE)]
         [nextTopSensorE (cdr topSensorE)]
         [nextBottomSensorE (cdr bottomSensorE)]
         [stepsMovementChangeE (changes stepsMovementB)])
    (always (λ (users xUsers xTop xBottom steps)
              (iff (and (not (equal? users 0)) (equal? xUsers 0) (not (equal? xTop ENTER)) (not (equal? xBottom ENTER)))
                   (equal? steps STOP)))
            (remove-last userCounterE) nextUserCounterE nextTopSensorE nextBottomSensorE (remove-last stepsMovementChangeE))))