#lang rosette

(require "../model.rkt")
(require "../fjapi.rkt")
(require "../operators.rkt")
(require "../sketch.rkt")

(current-bitwidth 5)

;; streams
;; topSensorE
;; bottomSensorE
;; stepMovementE
;; stepsMovementB
;; userCounterB
;; userCounterE

(define ENTER 0)
(define EXIT 1)

(define MOVEUP 0)
(define MOVEDOWN 1)
(define STOP 2)

(define timebound 4)

(define (iff p q)
  (or (and p q) (and (not p) (not q))))

;; LTL always/globally operator over event stream
(define (always predicate events)
  (andmap predicate events))

(define (nextE predicate events)
  #t)

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
(define (theta0 topSensorE bottomSensorE stepsMovementB userCounterE)
  (let ([stepsStatusE (behavior-changes stepsMovementB)])
    (andmap (λ (top bottom steps userCounter)
              (iff (or (and (equal? bottom ENTER) (not (equal? top EXIT)) (not (equal? steps MOVEDOWN)))
                       (and (equal? top ENTER) (not (equal? bottom EXIT)) (not (equal? steps MOVEUP))))
                   (equal? userCounter 1))) topSensorE bottomSensorE stepsStatusE userCounterE)))
(define (t0a t b s u)
  (let ([stepsStatusE (behavior-changes s)])
    (andmap (λ (top bottom steps userCounter)
              (iff (and (equal? bottom ENTER) (not (equal? top EXIT)) (not (equal? steps MOVEDOWN)))
                   (equal? userCounter 1))) t b stepsStatusE u)))

;; reference implementation that matches theta0
(define (refTheta0 top bottom steps)
  (mergeE (constantE 1 bottom) (constantE 1 top)))

;; synthesize userCounterE from topSensorE, bottomSensorE, stepsMovementB inputs
(define sTopE (new-event-stream get-sym-int timebound))
(define sBottomE (new-event-stream get-sym-int timebound))
(define sStepsB (new-behavior get-sym-int timebound))

(define (psi topE bottomE stepsB)
  (begin (assert (sensor topE))
         (assert (sensor bottomE))
         (assert (steps stepsB))))

(define sk (get-symbolic-sketch 5 (make-vector 5 #f) 3))

(define (t0syn)
  (begin
    (psi sTopE sBottomE sStepsB)
    (let* ([evaled-sk ((get-sketch-function sk) sTopE sBottomE sStepsB)]
           [b (time (synthesize #:forall (symbolics (list sTopE sBottomE sStepsB))
                                #:guarantee (assert (t0a sTopE sBottomE sStepsB evaled-sk))))])
      (if (unsat? b)
          (print "unsat")
          (print-sketch sk b)))))

;; theta one
(define (theta1 topSensorE bottomSensorE stepsMovementB userCounterE)
  (let ([stepsStatusE (behavior-changes stepsMovementB)])
    (andmap (λ (top bottom steps userCounter)
              (iff (or (and (equal? top EXIT) (not (equal? bottom ENTER)) (not (equal? steps MOVEDOWN)))
                       (and (equal? bottom EXIT) (not (equal? top ENTER)) (not (equal? steps MOVEUP))))
                   (equal? userCounter -1))) topSensorE bottomSensorE stepsStatusE userCounterE)))
;; theta two
(define (theta2 stepsMovementE userCounterB bottomSensorE)
  (let ([userCounter (behavior-changes userCounterB)])
    (andmap (λ (steps users bottom)
              (implies (equal? steps MOVEUP) (and (equal? users 0) (equal? bottom ENTER))))
            stepsMovementE userCounter bottomSensorE)))

;; theta three
(define (theta3 stepsMovementE userCounterB topSensorE)
  (let ([userCounter (behavior-changes userCounterB)])
    (andmap (λ (steps users top)
              (implies (equal? steps MOVEDOWN) (and (equal? users 0) (equal? top ENTER))))
            stepsMovementE userCounter topSensorE)))

;; theta four
