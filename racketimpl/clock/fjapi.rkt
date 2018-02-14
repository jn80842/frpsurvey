#lang rosette
(provide (all-defined-out))

(require "fjmodel.rkt")

(define (identityE e)
  e)

;; oneE

(define (zeroE e)
  'no-evt)

(define (mapE proc e)
  (if (empty-event? e)
      'no-evt
      (proc e)))

(define (mergeE e1 e2)
  (if (empty-event? e1)
      e2
      e1))

;; switchE

(define (filterE proc e)
  (if (and (not (empty-event? e)) (proc e))
      e
      'no-evt))

(define (ifE guardE trueE falseE)
  (if (empty-event? guardE)
      'no-evt
      (if guardE trueE falseE)))

(define (constantE const e)
  (if (empty-event? e)
      'no-evt
      const))

;; stateful operators need list of all history up to current timestep
(define (collectE init proc lst)
  (if (empty-event? (last lst))
      'no-evt
      (foldl (λ (n m) (if (empty-event? n) m (proc n m))) init lst)))

(define (andE e1 e2)
  (if (not (or (empty-event? e1) (empty-event? e2)))
      (and e1 e2)
      'no-evt))

(define (orE e1 e2)
  (if (not (or (empty-event? e1) (empty-event? e2)))
      (or e1 e2)
      'no-evt))

(define (notE e)
  (if (empty-event? e)
      'no-evt
      (not e)))

;; stateful operators need list of all history up to current timestep
(define (filterRepeatsE lst)
  (let ([events (reverse (filter not-empty-event? (take lst (sub1 (length lst)))))])
    (if (and (not (empty? events)) (equal? (last lst) (first events)))
        'no-evt
        (last lst))))

(define (snapshotE e b)
  (if (empty-event? e)
      'no-evt
      b))

;; stateful operators need list of all history up to current timestep
(define (onceE lst)
  (if (findf (λ (e) (not (empty-event? e))) (take lst (sub1 (length lst))))
      'no-evt
      (last lst)))

;; skipFirstE

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

;; startsWith

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

(define (liftB proc b)
  (proc b))

(define (ifB b1 b2 b3)
  (if b1 b2 b3))

;; timerB

;; blindB

;; calmB