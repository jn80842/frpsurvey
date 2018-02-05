#lang rosette
(provide (all-defined-out))

(require "fjmodel.rkt")

(define (identityE e)
  e)

;; oneE

(define (zeroE e)
  'no-evt)

(define (mapE proc e)
  (if (is-empty? e)
      'no-evt
      (proc e)))

(define (mergeE e1 e2)
  (if (is-empty? e1)
      e2
      e1))

;; switchE

(define (filterE proc e)
  (if (proc e)
      e
      'no-evt))

(define (ifE guardE trueE falseE)
  (if (is-empty? guardE)
      'no-evt
      (if guardE trueE falseE)))

(define (constantE const e)
  (if (is-empty? e)
      'no-evt
      const))

;; collectE

(define (andE e1 e2)
  (if (not (or (is-empty? e1) (is-empty? e2)))
      (and e1 e2)
      'no-evt))

(define (orE e1 e2)
  (if (not (or (is-empty? e1) (is-empty? e2)))
      (or e1 e2)
      'no-evt))

(define (notE e)
  (if (is-empty? e)
      'no-evt
      (not e)))

;; filterRepeatsE

(define (snapshotE e b)
  (if (is-empty? e)
      'no-evt
      b))

;; onceE

;; skipFirstE

;; delayE

;; blindE

;; calmE

;; timerE

;; startswith

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