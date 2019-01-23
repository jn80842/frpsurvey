#lang rosette

(require "api.rkt")

(provide (all-defined-out))

(struct stream-insn 
  (op-index arg-index1 arg-index2) #:transparent)

(define (always-different-bool)
  (define-symbolic* b boolean?) b)
(define (get-bool-list size)
  (for/list ([i (range size)]) (always-different-bool)))
(define (bool-lookup vals bools)
  (letrec ([f (λ (lst b-lst) (cond [(equal? (length lst) 2) (if (list-ref b-lst 0)
                                                                (list-ref lst 0)
                                                                (list-ref lst 1))]
                                   [else (if (list-ref b-lst 0)
                                             (list-ref lst 0)
                                             (f (cdr lst) (cdr b-lst)))]))])
    (f vals bools)))

(define (get-insn-holes past-var-size)
  (define-symbolic* option-index boolean?)
  (stream-insn (get-symbolic-op-idx) (get-bool-list past-var-size) (get-bool-list past-var-size)))

(define (get-holes-list count input-size)
  (for/list ([i (range count)]) (get-insn-holes (+ input-size i))))

(define (get-retval-idx insn-count input-count)
  (get-bool-list (+ insn-count input-count)))

(struct op-idx (b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11) #:transparent)

(define (get-symbolic-op-idx)
  (define-symbolic* b1 boolean?)
  (define-symbolic* b2 boolean?)
  (define-symbolic* b3 boolean?)
  (define-symbolic* b4 boolean?)
  (define-symbolic* b5 boolean?)
  (define-symbolic* b6 boolean?)
  (define-symbolic* b7 boolean?)
  (define-symbolic* b8 boolean?)
  (define-symbolic* b9 boolean?)
  (define-symbolic* b10 boolean?)
  (define-symbolic* b11 boolean?)
  (op-idx b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11))

(define (get-operator ops)
  (let ([b1 (op-idx-b1 ops)]
        [b2 (op-idx-b2 ops)]
        [b3 (op-idx-b3 ops)]
        [b4 (op-idx-b4 ops)]
        [b5 (op-idx-b5 ops)]
        [b6 (op-idx-b6 ops)]
        [b7 (op-idx-b7 ops)]
        [b8 (op-idx-b8 ops)]
        [b9 (op-idx-b9 ops)]
        [b10 (op-idx-b10 ops)]
        [b11 (op-idx-b11 ops)])
    (if b1 maskOnE-op
        (if b2 maskOffE-op
            (if b3 mergeE-op
                (if b4 constantE-one-op
                    (if b5 constantE-two-op
                        (if b6 constantE-three-op
                            (if b7 filterE-eq-one-op
                                (if b8 filterE-eq-two-op
                                    (if b9 filterE-eq-three-op
                                        (if b10 filterE-noteq-one-op
                                            (if b11 filterE-noteq-two-op
                                               filterE-noteq-three-op)))))))))))))

(struct consts-idx (b1 b2 b3 b4) #:transparent)

(define (get-symbolic-consts-idx)
  (define-symbolic* b1 boolean?)
  (define-symbolic* b2 boolean?)
  (define-symbolic* b3 boolean?)
  (define-symbolic* b4 boolean?)
  (consts-idx b1 b2 b3 b4))

(define (get-consts consts)
  (let ([b1 (consts-idx-b1 consts)]
        [b2 (consts-idx-b2 consts)]
        [b3 (consts-idx-b3 consts)]
        [b4 (consts-idx-b4 consts)])
    (if b1 (bv 0 (bitvector 1))
        (if b2 (bv 1 (bitvector 1))
            (if b3 (bv 0 (bitvector 2))
                (if b4 (bv 1 (bitvector 2))
                    (bv 2 (bitvector 2))))))))

(define (get-consts-string consts)
  (let ([b1 (consts-idx-b1 consts)]
        [b2 (consts-idx-b2 consts)]
        [b3 (consts-idx-b3 consts)]
        [b4 (consts-idx-b4 consts)])
    (if b1 "(bv 0 (bitvector 1))"
        (if b2 "(bv 1 (bitvector 1))"
            (if b3 "(bv 0 (bitvector 2))"
                (if b4 "(bv 1 (bitvector 2))"
                    "(bv 2 (bitvector 2))"))))))

(define (get-funcs b)
  (if b (λ (placeholder i) (equal? i placeholder))
      (λ (placeholder i) (not (equal? i placeholder)))))

(define (get-funcs-string b)
  (if (term? b) "symbolic ~a"
      (if b "(λ (i) (equal? i ~a))"
          "(λ (i) (not (equal? i ~a)))")))

(struct operator
  (name call print) #:transparent)

(define maskOnE-op
  (operator "maskOnE"
            (λ (insn past-vars) (maskOnE (get-input-stream insn past-vars)
                                         (get-input-stream2 insn past-vars)))
            (λ (insn past-vars) (format "~a ~a"
                                        (get-input-stream insn past-vars)
                                        (get-input-stream2 insn past-vars)))))

(define maskOffE-op
  (operator "maskOffE"
            (λ (insn past-vars) (maskOffE (get-input-stream insn past-vars)
                                          (get-input-stream2 insn past-vars)))
            (λ (insn past-vars) (format "~a ~a"
                                        (get-input-stream insn past-vars)
                                        (get-input-stream2 insn past-vars)))))

(define mergeE-op
  (operator "mergeE"
            (λ (insn past-vars) (mergeE (get-input-stream insn past-vars) (get-input-stream2 insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (get-input-stream insn past-vars) (get-input-stream2 insn past-vars)))))

(define constantE-one-op
  (operator "constantE"
            (λ (insn past-vars) (constantE-one (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "ONE ~a" (get-input-stream insn past-vars)))))

(define constantE-two-op
  (operator "constantE"
            (λ (insn past-vars) (constantE-two (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "TWO ~a" (get-input-stream insn past-vars)))))

(define constantE-three-op
  (operator "constantE"
            (λ (insn past-vars) (constantE-three (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "THREE ~a" (get-input-stream insn past-vars)))))

(define filterE-eq-one-op
  (operator "filterE"
            (λ (insn past-vars) (filterE-eq-one (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "(λ (e) (equal? e ONE))" (get-input-stream insn past-vars)))))

(define filterE-eq-two-op
  (operator "filterE"
            (λ (insn past-vars) (filterE-eq-two (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "(λ (e) (equal? e TWO))" (get-input-stream insn past-vars)))))

(define filterE-eq-three-op
  (operator "filterE"
            (λ (insn past-vars) (filterE-eq-three (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "(λ (e) (equal? e THREE))" (get-input-stream insn past-vars)))))

(define filterE-noteq-one-op
  (operator "filterE"
            (λ (insn past-vars) (filterE-noteq-one (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "(λ (e) (not (equal? e ONE)))" (get-input-stream insn past-vars)))))

(define filterE-noteq-two-op
  (operator "filterE"
            (λ (insn past-vars) (filterE-noteq-two (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "(λ (e) (not (equal? e TWO)))" (get-input-stream insn past-vars)))))

(define filterE-noteq-three-op
  (operator "filterE"
            (λ (insn past-vars) (filterE-noteq-three (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "(λ (e) (not (equal? e THREE)))" (get-input-stream insn past-vars)))))

(define (get-input-stream insn past-vars)
  (bool-lookup past-vars (stream-insn-arg-index1 insn)))

(define (get-input-stream2 insn past-vars)
  (bool-lookup past-vars (stream-insn-arg-index2 insn)))

(define (call-stream-insn op insn past-vars)
  ((operator-call op) insn past-vars))

(define (print-stream-insn op insn varname past-vars)
    (format "  (define ~a (~a ~a))" varname (operator-name op) ((operator-print op) insn past-vars)))

