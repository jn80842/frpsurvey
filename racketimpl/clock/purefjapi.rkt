#lang rosette
(provide (all-defined-out))

(require "fjmodel.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  experiment:
;;  all operators are pure: they have no access to history
;;  all inputs are assumed to be events (empty events handled elsewhere
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (identityE e)
  e)

;; oneE

(define (zeroE e)
  'no-evt)

(define (mapE proc e)
  (proc e))

(define (mergeE e1 e2)
  (if (empty-event? e1)
      e2
      e1))

;; switchE

(define (filterE proc e)
  (if (proc e)
      e
      'no-evt))

(define (ifE guardE trueE falseE)
  (if guardE trueE falseE))

(define (constantE const e)
  const)

(define (andE e1 e2)
  (and e1 e2))

(define (orE e1 e2)
  (or e1 e2))

(define (notE e)
  (not e))

(define (snapshotE e b)
  b)

;; stateful operators need list of all history up to current timestep
(define (delayE interval lst)
  (if (>= interval (length lst))
        'no-evt
        (list-ref lst (- (length lst) (add1 interval)))))

;; blindE

;; calmE

;; stateful operators need list of all history up to current timestep
(define (timerE interval lst)
  (if (equal? 0 (modulo (length lst) interval))
      #t
      'no-evt))

(define (changes b)
  b)

(define (constantB const b)
  const)

;; delayB

;; switchB

(define (andB b1 b2)
  (and b1 b2))

(define (orB b1 b2)
  (or b1 b2))

(define (notB b)
  (not b))

(define (liftB proc . b)
  (apply proc b))

(define (ifB b1 b2 b3)
  (if b1 b2 b3))

;; timerB

;; blindB

;; calmB