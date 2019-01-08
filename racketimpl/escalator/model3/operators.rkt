#lang rosette

(require "fjapi.rkt")

(provide (all-defined-out))

(struct stream-insn 
  (op-index arg-index1 arg-index2 arg-index3 option-index arg-int arg-int2) #:transparent)

(define (get-insn-holes)
  (define-symbolic* op integer?)
  (define-symbolic* streamidx integer?)
  (define-symbolic* arg2 integer?)
  (define-symbolic* arg3 integer?)
  (define-symbolic* option-index integer?)
  (define-symbolic* arg-int integer?)
  (define-symbolic* arg-int2 integer?)
  (stream-insn op streamidx arg2 arg3 option-index arg-int arg-int2))

(define (get-holes-list count)
  (for/list ([i (range count)]) (get-insn-holes)))

(define (get-retval-idx)
  (define-symbolic* retval-idx integer?)
  retval-idx)

(struct operator
  (name call print) #:transparent)

(define constantE-imm-op
  (operator "constantE"
            (λ (insn past-vars) (constantE (get-integer-arg insn)
                                           (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (stream-insn-arg-int insn)
                                        (get-input-stream insn past-vars)))))
(define constantE-op
  (operator "constantE"
            (λ (insn past-vars) (constantE (list-ref constantB-consts (stream-insn-option-index insn))
                                           (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (list-ref constantB-consts (stream-insn-option-index insn))
                                        (get-input-stream insn past-vars)))))
(define mergeE-op
  (operator "mergeE"
            (λ (insn past-vars) (mergeE (get-input-stream insn past-vars) (list-ref past-vars (stream-insn-arg-index2 insn))))
            (λ (insn past-vars) (format "~a ~a" (get-input-stream insn past-vars) (list-ref past-vars (stream-insn-arg-index2 insn))))))

(define mapE-op
  (operator "mapE"
            (λ (insn past-vars) (mapE (curry (list-ref inttoboolfuncs (stream-insn-option-index insn))
                                             (list-ref filterE-consts (get-integer-arg insn)))
                                      (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a"
                                        (format (list-ref inttoboolfuncs-string
                                                          (stream-insn-option-index insn))
                                                (list-ref filterE-consts (get-integer-arg insn)))
                                        (get-input-stream insn past-vars)))))

(define mapE-twoconst-op
  (operator "mapE"
            (λ (insn past-vars) (mapE (curry (list-ref inttoboolfuncs-twoconst (stream-insn-option-index insn))
                                             (get-integer-arg insn) (get-integer-arg2 insn))
                                      (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a"
                                        (format (list-ref inttoboolsfuncs-twoconst-string (stream-insn-option-index insn))
                                                (get-integer-arg insn) (get-integer-arg2 insn))
                                        (get-input-stream insn past-vars)))))

(define ifE-op
  (operator "ifE"
            (λ (insn past-vars) (ifE (get-input-stream insn past-vars)
                                     (get-input-stream2 insn past-vars)
                                     (get-input-stream3 insn past-vars)))
            (λ (insn past-vars) (format "~a ~a ~a" (get-input-stream insn past-vars)
                                        (get-input-stream2 insn past-vars)
                                        (get-input-stream3 insn past-vars)))))
(define andE-op
  (operator "andE"
            (λ (insn past-vars) (andE (get-input-stream insn past-vars)
                                      (get-input-stream2 insn past-vars)))
            (λ (insn past-vars) (format "~a ~a"
                                        (get-input-stream insn past-vars)
                                        (get-input-stream2 insn past-vars)))))

(define orE-op
  (operator "orE"
            (λ (insn past-vars) (orE (get-input-stream insn past-vars)
                                     (get-input-stream2 insn past-vars)))
            (λ (insn past-vars) (format "~a ~a"
                                        (get-input-stream insn past-vars)
                                        (get-input-stream2 insn past-vars)))))
(define notE-op
  (operator "notE"
            (λ (insn past-vars) (notE (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a" (get-input-stream insn past-vars)))))

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

#;(define mapE2-op
  (operator "mapE"
            (λ (insn past-vars) (mapE2 (list-ref function-2arg-list (stream-insn-arg-index2 insn))
                                       (get-input-stream insn past-vars)
                                       (list-ref past-vars (stream-insn-arg-index3 insn))))
            (λ (insn past-vars) (format "~a ~a ~a" (list-ref function-2arg-list-string (stream-insn-arg-index2 insn))
                                        (get-input-stream insn past-vars)
                                        (list-ref past-vars (stream-insn-arg-index3 insn))))))

(define filterE-op
  (operator "filterE"
            (λ (insn past-vars) (filterE (list-ref genericfuncs (stream-insn-option-index insn))
                                         (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (list-ref genericfuncs-string (stream-insn-option-index insn))
                                        (get-input-stream insn past-vars)))))
(define filterE-const-op
  (operator "filterE"
            (λ (insn past-vars) (filterE (curry (list-ref inttoboolfuncs (stream-insn-option-index insn))
                                                (list-ref filterE-consts (get-integer-arg insn)))
                                         (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a"
                                        (format (list-ref inttoboolfuncs-string (stream-insn-option-index insn))
                                                (list-ref filterE-consts (get-integer-arg insn)))
                                        (get-input-stream insn past-vars)))))

(define identityE-op
  (operator "identityE"
            (λ (insn past-vars) (identityE (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a" (get-input-stream insn past-vars)))))

(define zeroE-op
  (operator "zeroE"
            (λ (insn past-vars) (zeroE (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a" (get-input-stream insn past-vars)))))

(define operator-list
  (list identityE-op ;; 0
        zeroE-op ;; 1
        mapE-op ;; 2
        mapE-twoconst-op ;; 3
        ;mapE2-op
        mergeE-op ;; 4
        filterE-op ;; 5
        filterE-const-op ;; 6
        ifE-op ;;7 
        constantE-op ;; 8
        constantE-imm-op ;; 9
        andE-op ;; 10
        orE-op ;; 11
        notE-op ;; 12
        maskOnE-op ;; 13
        maskOffE-op)) ;; 14

(define (get-input-stream insn past-vars)
  (list-ref past-vars (stream-insn-arg-index1 insn)))

(define (get-input-stream2 insn past-vars)
  (list-ref past-vars (stream-insn-arg-index2 insn)))

(define (get-input-stream3 insn past-vars)
  (list-ref past-vars (stream-insn-arg-index3 insn)))

(define (get-integer-arg insn)
  (stream-insn-arg-int insn))

(define (get-integer-arg2 insn)
  (stream-insn-arg-int2 insn))

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
;; int -> bool
(define inttoboolfuncs (list ; (λ (placeholder i) (<= i placeholder))
                            ; (λ (placeholder i) (>= i placeholder))
                            ; (λ (placeholder i) (< i placeholder))
                            ; (λ (placeholder i) (> i placeholder))
                             (λ (placeholder i) (equal? i placeholder))
                             (λ (placeholder i) (not (equal? i placeholder)))
                             ))

(define inttoboolfuncs-string (list ;"(λ (i) (<= i ~a))"
                                    ;"(λ (i) (>= i ~a))"
                                    ;"(λ (i) (< i ~a))"
                                    ;"(λ (i) (> i ~a))"
                                    "(λ (i) (= i ~a))"
                                    "(λ (i) (not (= i ~a)))"
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
(define filterE-consts (list (bv 0 (bitvector 1))
                             (bv 1 (bitvector 1))
                             (bv 0 (bitvector 2))
                             (bv 1 (bitvector 2))
                             (bv 2 (bitvector 2))))

(define (get-filterE-consts lookup)
  (if (equal? (bv 0 (bitvector 3)) lookup)
      (bv 0 (bitvector 1))
      (if (equal? (bv 1 (bitvector 3)) lookup)
          (bv 1 (bitvector 1))
          (if (equal? (bv 2 (bitvector 3)) lookup)
              (bv 0 (bitvector 2))
              (if (equal? (bv 3 (bitvector 3)) lookup)
                  (bv 1 (bitvector 2))
                  (if (equal? (bv 4 (bitvector 3)) lookup)
                      (bv 2 (bitvector 2))
                      'INVALID))))))
