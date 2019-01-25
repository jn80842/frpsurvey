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
    (if b1 constantE-one-op
        (if b2 constantE-two-op
            (if b3 constantE-three-op
                (if b4 mergeE-op
                    (if b5 filterE-noteq-three-op
                        (if b6 filterE-eq-one-op
                            (if b7 filterE-eq-two-op
                                (if b8 filterE-eq-three-op
                                    (if b9 filterE-noteq-one-op
                                        (if b10 filterE-noteq-two-op
                                            (if b11 maskOnE-op
                                                maskOffE-op)))))))))))))

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

#;(define constantE-imm-op
  (operator "constantE"
            (λ (insn past-vars) (constantE (get-integer-arg insn)
                                           (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (stream-insn-arg-int insn)
                                        (get-input-stream insn past-vars)))))
(define constantE-one-op
  (operator "constantE"
            (λ (insn past-vars) (constant-one-lut (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "ONE ~a" (get-input-stream insn past-vars)))))
(define constantE-two-op
  (operator "constantE"
            (λ (insn past-vars) (constant-two-lut (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "TWO ~a" (get-input-stream insn past-vars)))))
(define constantE-three-op
  (operator "constantE"
            (λ (insn past-vars) (constant-three-lut (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "THREE ~a" (get-input-stream insn past-vars)))))
(define mergeE-op
  (operator "mergeE"
            (λ (insn past-vars) (merge-lut (get-input-stream insn past-vars) (get-input-stream2 insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (get-input-stream insn past-vars) (get-input-stream2 insn past-vars)))))

#;(define mapE-op
  (operator "mapE"
            (λ (insn past-vars) (mapE (curry (get-funcs (stream-insn-option-index insn))
                                             (get-consts (stream-insn-consts-idx insn)))
                                      (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a"
                                        (format (get-funcs-string (stream-insn-option-index insn))
                                                (get-consts-string (stream-insn-consts-idx insn)))
                                        (get-input-stream insn past-vars)))))

#;(define mapE-twoconst-op
  (operator "mapE"
            (λ (insn past-vars) (mapE (curry (list-ref inttoboolfuncs-twoconst (stream-insn-option-index insn))
                                             (get-integer-arg insn) (get-integer-arg2 insn))
                                      (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a"
                                        (format (list-ref inttoboolsfuncs-twoconst-string (stream-insn-option-index insn))
                                                (get-integer-arg insn) (get-integer-arg2 insn))
                                        (get-input-stream insn past-vars)))))

#;(define ifE-op
  (operator "ifE"
            (λ (insn past-vars) (ifE (get-input-stream insn past-vars)
                                     (get-input-stream2 insn past-vars)
                                     (get-input-stream3 insn past-vars)))
            (λ (insn past-vars) (format "~a ~a ~a" (get-input-stream insn past-vars)
                                        (get-input-stream2 insn past-vars)
                                        (get-input-stream3 insn past-vars)))))
#;(define andE-op
  (operator "andE"
            (λ (insn past-vars) (andE (get-input-stream insn past-vars)
                                      (get-input-stream2 insn past-vars)))
            (λ (insn past-vars) (format "~a ~a"
                                        (get-input-stream insn past-vars)
                                        (get-input-stream2 insn past-vars)))))

#;(define orE-op
  (operator "orE"
            (λ (insn past-vars) (orE (get-input-stream insn past-vars)
                                     (get-input-stream2 insn past-vars)))
            (λ (insn past-vars) (format "~a ~a"
                                        (get-input-stream insn past-vars)
                                        (get-input-stream2 insn past-vars)))))
#;(define notE-op
  (operator "notE"
            (λ (insn past-vars) (notE (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a" (get-input-stream insn past-vars)))))

(define maskOnE-op
  (operator "maskOnE"
            (λ (insn past-vars) (maskOn-lut (get-input-stream insn past-vars)
                                         (get-input-stream2 insn past-vars)))
            (λ (insn past-vars) (format "~a ~a"
                                        (get-input-stream insn past-vars)
                                        (get-input-stream2 insn past-vars)))))

(define maskOffE-op
  (operator "maskOffE"
            (λ (insn past-vars) (maskOff-lut (get-input-stream insn past-vars)
                                          (get-input-stream2 insn past-vars)))
            (λ (insn past-vars) (format "~a ~a"
                                        (get-input-stream insn past-vars)
                                        (get-input-stream2 insn past-vars)))))

#;(define mapE2-op
  (operator "mapE"
            (λ (insn past-vars) (mapE2 (list-ref function-2arg-list (stream-insn-arg-index2 insn))
                                       (get-input-stream insn past-vars)
                                       (list-ref past-vars (stream-insn-arg-index3 insn))))
            (λ (insn past-vars) (format "~a ~a ~a" (list-ref function-2arg-list-string (stream-insn-arg-index2 insn))
                                        (get-input-stream insn past-vars)
                                        (list-ref past-vars (stream-insn-arg-index3 insn))))))

#;(define filterE-op
  (operator "filterE"
            (λ (insn past-vars) (filterE identity
                                         (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" "identity"
                                        (get-input-stream insn past-vars)))))
(define filterE-eq-one-op
  (operator "filterE"
            (λ (insn past-vars) (filter-eq-one-lut  (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "(λ (e) (equal? e ONE)) ~a" (get-input-stream insn past-vars)))))
(define filterE-eq-two-op
  (operator "filterE"
            (λ (insn past-vars) (filter-eq-two-lut (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "(λ (e) (equal? e ONE)) ~a" (get-input-stream insn past-vars)))))
(define filterE-eq-three-op
  (operator "filterE"
            (λ (insn past-vars) (filter-eq-three-lut (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "(λ (e) (equal? e THREE)) ~a" (get-input-stream insn past-vars)))))
(define filterE-noteq-one-op
  (operator "filterE"
            (λ (insn past-vars) (filter-noteq-one-lut  (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "(λ (e) (not (equal? e ONE))) ~a" (get-input-stream insn past-vars)))))
(define filterE-noteq-two-op
  (operator "filterE"
            (λ (insn past-vars) (filter-noteq-two-lut  (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "(λ (e) (not (equal? e TWO))) ~a" (get-input-stream insn past-vars)))))
(define filterE-noteq-three-op
  (operator "filterE"
            (λ (insn past-vars) (filter-noteq-three-lut  (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "(λ (e) (not (equal? e THREE))) ~a" (get-input-stream insn past-vars)))))

#;(define identityE-op
  (operator "identityE"
            (λ (insn past-vars) (identityE (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a" (get-input-stream insn past-vars)))))

#;(define zeroE-op
  (operator "zeroE"
            (λ (insn past-vars) (zeroE (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a" (get-input-stream insn past-vars)))))

(define (get-input-stream insn past-vars)
  (bool-lookup past-vars (stream-insn-arg-index1 insn)))

(define (get-input-stream2 insn past-vars)
  (bool-lookup past-vars (stream-insn-arg-index2 insn)))

(define (call-stream-insn op insn past-vars)
  ((operator-call op) insn past-vars))

(define (print-stream-insn op insn varname past-vars)
    (format "  (define ~a (~a ~a))" varname (operator-name op) ((operator-print op) insn past-vars)))

;; predicate function library

;; ? -> ?
(define genericfuncs (list identity
                           identity
                           identity
                           identity
                           identity))

(define genericfuncs-string (list "(λ (e) e)"
                                  "(λ (e) e)"
                                  "(λ (e) e)"
                                  "(λ (e) e)"
                                  "(λ (e) e)"))

;; int -> int
(define inttointfuncs (list (λ (placeholder i) (+ i placeholder))
                            (λ (placeholder i) (- i placeholder))
                            (λ (placeholder i) (- placeholder i))
                            (λ (placeholder i) (+ i placeholder))
                            (λ (placeholder i) (+ i placeholder))
                           ; (λ (placeholder i) (* i placeholder))
                           ; (λ (i) (/ i placeholder)) ;; leave out division?
                           ; (λ (i) (/ placeholder i))
                            ))
(define inttointfuncs-string (list "(λ (i) (+ i ~a))"
                                   "(λ (i) (- i ~a))"
                                   "(λ (i) (- ~a i))"
                                   "(λ (i) (+ i ~a))"
                                   "(λ (i) (+ i ~a))"
                                  ; "(λ (i) (* i ~a))"
                                   ))


;; note that these can be composed from the inttoboolfuncs
(define inttoboolfuncs-twoconst (list
                                 ;; outside of range
                                 (λ (placeholder placeholder2 i) (or (>= i placeholder) (<= i placeholder2)))
                                 ;; inside of range
                                 (λ (placeholder placeholder2 i) (and (>= i placeholder) (<= i placeholder2)))
                                 ;; outside of range
                                 (λ (placeholder placeholder2 i) (or (>= i placeholder) (<= i placeholder2)))
                                 ;; inside of range
                                 (λ (placeholder placeholder2 i) (and (>= i placeholder) (<= i placeholder2)))
                                 ;; outside of range
                                 (λ (placeholder placeholder2 i) (or (>= i placeholder) (<= i placeholder2)))
                                 ))

(define inttoboolsfuncs-twoconst-string (list
                                         "(λ (i) (or (>= i ~a) (<= i ~a)))"
                                         "(λ (i) (and (>= i ~a) (<= i ~a)))"
                                         "(λ (i) (or (>= i ~a) (<= i ~a)))"
                                         "(λ (i) (and (>= i ~a) (<= i ~a)))"
                                         "(λ (i) (or (>= i ~a) (<= i ~a)))"
                                         ))

;; int -> int -> int
(define inttointtointfuncs (list +
                                 -
                                 ; *
                                 ; /
                                 min
                                 max
                                 +
                                 ))
(define inttointtointfuncs-strings (list "+"
                                         "-"
                                         ; "*"
                                         ; "/"
                                         "min"
                                         "max"
                                         "+"
                                         ))

(define constantB-consts (list 'on 'off #t #f 'test (bv 0 (bitvector 1)) (bv 1 (bitvector 1))))

(println "synthesized all LUT functions")