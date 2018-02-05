#lang rosette

(provide (all-defined-out))

(require "fjapi.rkt")
(require "fjmodel.rkt")

(struct operator
  (name call print) #:transparent)

(define zeroE-op
  (operator "zeroE"
            (λ (i reg) (zeroE (list-ref reg (insn-idx1 i))))
            (λ (i reg) (list-ref reg (insn-idx1 i)))))
(define mapE-op
  (operator "mapE"
            (λ (i reg) (mapE (list-ref function-list (insn-idx2 i))
                             (list-ref reg (insn-idx1 i))))
            (λ (i reg) (format "~a ~a" (list-ref function-list-string (insn-idx2 i))
                               (list-ref reg (insn-idx1 i))))))
(define mergeE-op
  (operator "mergeE"
            (λ (i reg) (mergeE (list-ref reg (insn-idx1 i)) (list-ref reg (insn-idx2 i))))
            (λ (i reg) (format "~a ~a" (list-ref reg (insn-idx1 i)) (list-ref reg (insn-idx2 i))))))
(define filterE-op
  (operator "filterE"
            (λ (i reg) (filterE (list-ref function-list (insn-idx2 i))
                                (list-ref reg (insn-idx1 i))))
            (λ (i reg) (format "~a ~a" (list-ref function-list-string (insn-idx2 i))
                               (list-ref reg (insn-idx1 i))))))
(define constantE-op
  (operator "constantE"
            (λ (i reg) (constantE (list-ref constant-list (insn-idx2 i))
                                  (list-ref reg (insn-idx1 i))))
            (λ (i reg) (format "~a ~a" (list-ref constant-list (insn-idx2 i))
                               (list-ref reg (insn-idx1 i))))))
(define constantE-imm-op
  (operator "constantE"
            (λ (i reg) (constantE (insn-int i) (list-ref reg (insn-idx1 i))))
            (λ (i reg) (format "~a ~a" (insn-int i) (list-ref reg (insn-idx1 i))))))
(define constantB-op
  (operator "constantB"
            (λ (i reg) (constantB (list-ref constant-list (insn-idx2 i))
                                  (list-ref reg (insn-idx1 i))))
            (λ (i reg) (format "~a ~a" (list-ref constant-list (insn-idx2 i))
                               (list-ref reg (insn-idx1 i))))))
(define andB-op
  (operator "andB"
            (λ (i reg) (andB (list-ref reg (insn-idx1 i))
                             (list-ref reg (insn-idx2 i))))
            (λ (i reg) (format "~a ~a" (list-ref reg (insn-idx1 i))
                               (list-ref reg (insn-idx2 i))))))
(define liftB-op
  (operator "liftB"
            (λ (i reg) (liftB (list-ref function-list (insn-idx2 i))
                              (list-ref reg (insn-idx1 i))))
            (λ (i reg) (format "~a ~a" (list-ref function-list-string (insn-idx2 i))
                               (list-ref reg (insn-idx1 i))))))
(define ifB-op
  (operator "ifB"
            (λ (i reg) (ifB (list-ref reg (insn-idx1 i))
                            (list-ref reg (insn-idx2 i))
                            (list-ref reg (insn-idx3 i))))
            (λ (i reg) (format "~a ~a ~a" (list-ref reg (insn-idx1 i))
                               (list-ref reg (insn-idx2 i))
                               (list-ref reg (insn-idx3 i))))))

(define operator-list
  (list constantB-op
        andB-op
        ifB-op
        liftB-op))

(define function-list '())
(define function-list-string '())
(define constant-list '())

(struct insn
  (op-index idx1 idx2 idx3 int) #:transparent)

(define (get-insn-holes)
  (define-symbolic* op integer?)
  (define-symbolic* idx1 integer?)
  (define-symbolic* idx2 integer?)
  (define-symbolic* idx3 integer?)
  (define-symbolic* int integer?)
  (insn op idx1 idx2 idx3 int))

(define (call-insn i reg)
  (let ([op (list-ref operator-list (insn-op-index i))])
    ((operator-call op) i reg)))
(define (print-insn i reg)
  (let ([op (list-ref operator-list (insn-op-index i))])
    ((operator-print op) i reg)))

(define (eval-graph graph . inputs)
  (apply (curry map graph) inputs))
