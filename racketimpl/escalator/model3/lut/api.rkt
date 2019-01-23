#lang rosette
(provide (all-defined-out))

(define-symbolic* bv1 (bitvector 2))
(define-symbolic* bv2 (bitvector 2))

;; note that ZERO always means that no event occurred
(define ZERO (bv 0 (bitvector 2)))
(define ONE (bv 1 (bitvector 2)))
(define TWO (bv 2 (bitvector 2)))
(define THREE (bv 3 (bitvector 2)))

(define (iff p q)
  (or (and p q) (and (not p) (not q))))

(define (maskOnE e1 e2)
  (if (and (not (equal? e1 ZERO)) (not (equal? e2 ZERO)))
      e2
      ZERO))

(define (maskOffE e1 e2)
  (if (and (equal? e1 ZERO) (not (equal? e2 ZERO)))
      e2
      ZERO))

(define (mergeE e1 e2)
  (if (not (equal? e1 ZERO))
      e1
      e2))

(define (constantE-one e1)
  (if (not (equal? e1 ZERO))
      ONE
      ZERO))

(define (constantE-two e1)
  (if (not (equal? e1 ZERO))
      TWO
      ZERO))

(define (constantE-three e1)
  (if (not (equal? e1 ZERO))
      THREE
      ZERO))

(define (filterE-eq-one e1)
  (if (equal? e1 ONE)
      ONE
      ZERO))

(define (filterE-eq-two e1)
  (if (equal? e1 TWO)
      TWO
      ZERO))

(define (filterE-eq-three e1)
  (if (equal? e1 THREE)
      THREE
      ZERO))

(define (filterE-noteq-one e1)
  (if (equal? e1 ZERO)
      ZERO
      (if (equal? e1 ONE)
          ZERO
          e1)))

(define (filterE-noteq-two e1)
  (if (equal? e1 ZERO)
      ZERO
      (if (equal? e1 TWO)
          ZERO
          e1)))

(define (filterE-noteq-three e1)
  (if (equal? e1 ZERO)
      ZERO
      (if (equal? e1 THREE)
          ZERO
          e1)))