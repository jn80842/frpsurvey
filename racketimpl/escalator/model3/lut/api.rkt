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

(define-symbolic* maskOn-f (~> (bitvector 2) (bitvector 2) (bitvector 2)))

(define maskOn-b (time (synthesize #:forall (list bv1 bv2)
                                   #:guarantee (assert (equal? (maskOn-f bv1 bv2) (maskOnE bv1 bv2))))))

(define maskOn-lut (evaluate maskOn-f maskOn-b))

(define (maskOffE e1 e2)
  (if (and (equal? e1 ZERO) (not (equal? e2 ZERO)))
      e2
      ZERO))

(define-symbolic* maskOff-f (~> (bitvector 2) (bitvector 2) (bitvector 2)))

(define maskOff-b (time (synthesize #:forall (list bv1 bv2)
                                   #:guarantee (assert (equal? (maskOff-f bv1 bv2) (maskOnE bv1 bv2))))))

(define maskOff-lut (evaluate maskOff-f maskOff-b))

(define (mergeE e1 e2)
  (if (not (equal? e1 ZERO))
      e1
      e2))

(define-symbolic* merge-f (~> (bitvector 2) (bitvector 2) (bitvector 2)))

(define merge-b (time (synthesize #:forall (list bv1 bv2)
                                   #:guarantee (assert (equal? (merge-f bv1 bv2) (mergeE bv1 bv2))))))

(define merge-lut (evaluate merge-f merge-b))

(define (constantE-one e1)
  (if (not (equal? e1 ZERO))
      ONE
      ZERO))

(define-symbolic* constant-one-f (~> (bitvector 2) (bitvector 2)))

(define constant-one-b (time (synthesize #:forall (list bv1 bv2)
                                         #:guarantee (assert (equal? (constant-one-f bv1) (constantE-one bv1))))))

(define constant-one-lut (evaluate constant-one-f constant-one-b))

(define (constantE-two e1)
  (if (not (equal? e1 ZERO))
      TWO
      ZERO))

(define-symbolic* constant-two-f (~> (bitvector 2) (bitvector 2)))

(define constant-two-b (time (synthesize #:forall (list bv1 bv2)
                                         #:guarantee (assert (equal? (constant-two-f bv1) (constantE-two bv1))))))

(define constant-two-lut (evaluate constant-two-f constant-two-b))

(define (constantE-three e1)
  (if (not (equal? e1 ZERO))
      THREE
      ZERO))

(define-symbolic* constant-three-f (~> (bitvector 2) (bitvector 2)))

(define constant-three-b (time (synthesize #:forall (list bv1 bv2)
                                         #:guarantee (assert (equal? (constant-three-f bv1) (constantE-three bv1))))))

(define constant-three-lut (evaluate constant-three-f constant-three-b))

(define (filterE-eq-one e1)
  (if (equal? e1 ONE)
      ONE
      ZERO))

(define-symbolic* filterE-eq-one-f (~> (bitvector 2) (bitvector 2)))

(define filterE-eq-one-b (time (synthesize #:forall (list bv1 bv2)
                                         #:guarantee (assert (equal? (filterE-eq-one-f bv1) (filterE-eq-one bv1))))))

(define filter-eq-one-lut (evaluate filterE-eq-one-f filterE-eq-one-b))

(define (filterE-eq-two e1)
  (if (equal? e1 TWO)
      TWO
      ZERO))

(define-symbolic* filterE-eq-two-f (~> (bitvector 2) (bitvector 2)))

(define filterE-eq-two-b (time (synthesize #:forall (list bv1 bv2)
                                         #:guarantee (assert (equal? (filterE-eq-two-f bv1) (filterE-eq-two bv1))))))

(define filter-eq-two-lut (evaluate filterE-eq-two-f filterE-eq-two-b))

(define (filterE-eq-three e1)
  (if (equal? e1 THREE)
      THREE
      ZERO))

(define-symbolic* filterE-eq-three-f (~> (bitvector 2) (bitvector 2)))

(define filterE-eq-three-b (time (synthesize #:forall (list bv1 bv2)
                                         #:guarantee (assert (equal? (filterE-eq-three-f bv1) (filterE-eq-three bv1))))))

(define filter-eq-three-lut (evaluate filterE-eq-three-f filterE-eq-three-b))

(define (filterE-noteq-one e1)
  (if (equal? e1 ZERO)
      ZERO
      (if (equal? e1 ONE)
          ZERO
          e1)))

(define-symbolic* filterE-noteq-one-f (~> (bitvector 2) (bitvector 2)))

(define filterE-noteq-one-b (time (synthesize #:forall (list bv1 bv2)
                                         #:guarantee (assert (equal? (filterE-noteq-one-f bv1) (filterE-noteq-one bv1))))))

(define filter-noteq-one-lut (evaluate filterE-noteq-one-f filterE-noteq-one-b))

(define (filterE-noteq-two e1)
  (if (equal? e1 ZERO)
      ZERO
      (if (equal? e1 TWO)
          ZERO
          e1)))

(define-symbolic* filterE-noteq-two-f (~> (bitvector 2) (bitvector 2)))

(define filterE-noteq-two-b (time (synthesize #:forall (list bv1 bv2)
                                         #:guarantee (assert (equal? (filterE-noteq-two-f bv1) (filterE-noteq-two bv1))))))

(define filter-noteq-two-lut (evaluate filterE-noteq-two-f filterE-noteq-two-b))

(define (filterE-noteq-three e1)
  (if (equal? e1 ZERO)
      ZERO
      (if (equal? e1 THREE)
          ZERO
          e1)))

(define-symbolic* filterE-noteq-three-f (~> (bitvector 2) (bitvector 2)))

(define filterE-noteq-three-b (time (synthesize #:forall (list bv1 bv2)
                                         #:guarantee (assert (equal? (filterE-noteq-three-f bv1) (filterE-noteq-three bv1))))))

(define filter-noteq-three-lut (evaluate filterE-noteq-three-f filterE-noteq-three-b))