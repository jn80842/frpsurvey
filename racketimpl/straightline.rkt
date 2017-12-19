#lang rosette
(require rosette/lib/synthax)
(require "dense-fjmodels.rkt")
(require "densefjapi.rkt")

(provide (all-defined-out))

;; since only 1 insn can be chosen per line,
;; some of these could be conflated
(define constantE-const-idx 0)
(define mergeE-index-idx 1)
(define collectE-val-idx 2)
(define startsWith-val-idx 3)
(define op-index-idx 4)
(define input-index-idx 5)
(define liftB1-func-idx 6)
(define andB-index-idx 7)

(define op-name-hash (make-hash (list (cons 0 "constantE")
                                   (cons 1 "mergeE")
                                   (cons 2 "collectE")
                                   (cons 3 "startsWith")
                                   (cons 4 "liftB1")
                                   (cons 5 "andB"))))

(define (op-lookup idx)
  (hash-ref op-name-hash idx))

(define liftB1-func (list (λ (t) (<= t 2))
                          (λ (c) (or (>= c 4) (>= 2 c)))))
(define constantB-consts (list 'on 'off))

(define (single-insn holes past-vars)
  ((list-ref (list (curry constantE (list-ref holes constantE-const-idx))
                   (curry mergeE (list-ref past-vars (list-ref holes mergeE-index-idx)))
                   (curry collectE (list-ref holes collectE-val-idx) +)
                   (curry startsWith (list-ref holes startsWith-val-idx))
                   (curry liftB1 (list-ref liftB1-func (list-ref holes liftB1-func-idx)))
                   (curry andB (list-ref past-vars (list-ref holes andB-index-idx)))) (list-ref holes op-index-idx))
                   (list-ref past-vars (list-ref holes input-index-idx))))

;; what's better way to structure this?
;; struct?
;; shriram-inspired macro/continuation/???
;; look at rosette source code (since this is kind of just reimplementing parts of rosette)
(define (get-holes)
  (define-symbolic* constantE-const integer?)
  (define-symbolic* mergeE-index integer?)
  (define-symbolic* collectE-start-val integer?)
  (define-symbolic* startsWith-start-val integer?)
  (define-symbolic* operator-index integer?)
  (define-symbolic* input-index integer?)
  (define-symbolic* liftB1-func-idx integer?)
  (list constantE-const mergeE-index collectE-start-val startsWith-start-val operator-index input-index liftB1-func-idx))

(define (print-single-insn insn-holes binding varname past-vars)
  (define op (op-lookup (evaluate (list-ref insn-holes op-index-idx) binding)))
  (define op-args (get-args-by-op op insn-holes past-vars binding))
  (define input-name (list-ref past-vars (evaluate  (list-ref insn-holes input-index-idx) binding)))
  (format "  (define ~a (~a ~a ~a)" varname op op-args input-name))

(define (get-args-by-op op-name holes past-vars binding)
  (case op-name
    [("constantE") (format "~a" (evaluate  (list-ref holes constantE-const-idx) binding))]
    [("mergeE") (format "~a" (list-ref past-vars (evaluate  (list-ref holes mergeE-index-idx) binding)))]
    [("collectE") (format "~a +" (evaluate  (list-ref holes collectE-val-idx) binding))]
    [("startsWith") (format "~a" (evaluate  (list-ref holes startsWith-val-idx) binding))]
    [else "fail"]))


;;;; previous, macro style ;;;;;


(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))
;; top level definitions of SSA variables
;; hack to make eval of rewrites work
(define r1 'r1)
(define r2 'r2)
(define r3 'r3)
(define r4 'r4)
(define r5 'r5)
(define r6 'r6)
(define r7 'r7)
(define r8 'r8)

(define (insn-printer insn-stx)
  (eval (syntax-case insn-stx ()
          [(define r1 ((curry op ARG) INPUT-STREAM))
          #'(format "  (define ~a (~a ~a ~a))" 'r1 'op ARG INPUT-STREAM)]
          [(define r1 ((curry collectE int-arg op-arg) INPUT-STREAM))
          #'(format "  (define ~a (collectE ~a ~a ~a))" 'r1 'int-arg 'op-arg INPUT-STREAM)]
          [(define r1 (op INPUT-STREAM)) #'(format "  (define ~a (~a ~a))" 'r1 'op INPUT-STREAM)]
          [(define r1 v1) #'(format "  (define ~a ~a)" 'r1 'v1)]
          [retval #'(format "  ~a)" 'retval)]
          [_ #'(format "no match")]) ns))

(define (function-printer binding)
  (let ([syntax-lst (syntax-e (list-ref (generate-forms binding) 0))])
    (begin
      (displayln (format "(~a ~a" (syntax->datum (list-ref syntax-lst 0))
                         (syntax->datum (list-ref syntax-lst 1))))
      (for ([s (list-tail syntax-lst 2)])
        (displayln (insn-printer s))))))