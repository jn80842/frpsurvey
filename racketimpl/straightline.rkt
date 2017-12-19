#lang rosette
(require rosette/lib/synthax)
(require "dense-fjmodels.rkt")
(require "densefjapi.rkt")

(provide (all-defined-out))

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

(struct stream-insn (op-index arg-index1 arg-index2 arg-index3 arg-int))

;; TODO add asserts to limit range of indexes
(define (get-insn-holes)
  (define-symbolic* op integer?)
  (define-symbolic* arg1 integer?)
  (define-symbolic* arg2 integer?)
  (define-symbolic* arg3 integer?)
  (define-symbolic* arg4 integer?)
  (stream-insn op arg1 arg2 arg3 arg4))

(define (single-insn holes past-vars)
  ((list-ref (list (curry constantE (stream-insn-arg-int holes))
                   (curry mergeE (list-ref past-vars (stream-insn-arg-index2 holes)))
                   (curry collectE (stream-insn-arg-int holes) +)
                   (curry startsWith (stream-insn-arg-int holes))) (stream-insn-op-index holes))
             (list-ref past-vars (stream-insn-arg-index1 holes))))

;; what's better way to structure this?
;; struct?
;; shriram-inspired macro/continuation/???
;; look at rosette source code (since this is kind of just reimplementing parts of rosette)

(define (print-single-insn insn-holes binding varname past-vars)
  (define op (op-lookup (evaluate (stream-insn-op-index insn-holes) binding)))
  (define op-args (get-args-by-op op insn-holes binding past-vars))
  (format "  (define ~a (~a ~a)" varname op op-args))

(define (get-args-by-op op-name holes binding past-vars)
  (case op-name
    [("constantE") (format "~a ~a" (evaluate (stream-insn-arg-int holes) binding)
                           (list-ref past-vars (evaluate (stream-insn-arg-index1 holes) binding)))]
    [("mergeE") (format "~a ~a" (list-ref past-vars (evaluate (stream-insn-arg-index2 holes) binding))
                        (list-ref past-vars (evaluate (stream-insn-arg-index1 holes) binding)))]
    [("collectE") (format "~a + ~a" (evaluate (stream-insn-arg-int holes) binding)
                          (list-ref past-vars (evaluate (stream-insn-arg-index1 holes) binding)))]
    [("startsWith") (format "~a ~a" (evaluate (stream-insn-arg-int holes) binding)
                            (list-ref past-vars (evaluate (stream-insn-arg-index1 holes) binding)))]
    [else "fail"]))

;; better parameterize the number of input streams
(define (print-from-holes holes binding depth)
  (displayln "(define (synthesized-function input1 input2)")
  (displayln "  (define r1 input1)")
  (displayln "  (define r2 input2)")

  (define varlist (for/list ([i (range (+ 2 depth))])
                    (format "r~a" (add1 i))))
  (define middle-insns (for/list ([i (range depth)])
                        (displayln (print-single-insn (list-ref holes i) binding (list-ref varlist (+ 2 i)) (take varlist (+ 2 i))))))
  
  (displayln (format "  r~a)" (+ 2 depth))))
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