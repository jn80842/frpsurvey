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
  (list constantE-imm-op
        constantE-op
        constantB-op
        andB-op
        ifB-op
        liftB-op))

(define function-list (list (λ (e) (+ e 5))
                            (λ (t) (<= t 2))
                            (λ (c) (or (>= c 4) (>= 2 c)))
                            (λ (e) (if e 'on 'off))
                            ))
(define function-list-string (list "(λ (e) (+ e 5))"
                                   "(λ (t) (<= t 2))"
                                   "(λ (c) (or (>= c 4) (>= 2 c)))"
                                   "(λ (e) (if e 'on 'off))"
                                   ))
(define constant-list (list 'on 'off #t #f 'test))

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

(define (print-single-insn bound-holes varname past-vars)
  (define op (operator-name (list-ref operator-list (insn-op-index bound-holes))))
  (define op-args (print-struct-insn bound-holes past-vars))
  (format "  (define ~a (~a ~a))" varname op op-args))

(define (print-struct-insn i past-vars)
  (let ([op (list-ref operator-list (insn-op-index i))])
    ((operator-print op) i past-vars)))

(define (string-from-holes bound-holes retval input-count)
  (let* ([arg-list (for/list ([i (range input-count)])
                    (format "input~a" (add1 i)))]
        [input-stmt-list (for/list ([i (range input-count)])
                           (format "  (define r~a input~a)" (add1 i) (add1 i)))]
        [depth (length bound-holes)]
        [varlist (for/list ([i (range (add1 (+ input-count depth)))])
                                            (format "r~a" (add1 i)))]
        [synthed-stmt-list (for/list ([i (range depth)])
                             (print-single-insn (list-ref bound-holes i) (list-ref varlist (+ input-count i))
                                                (take varlist (+ input-count i))))]
        [return-stmt (format "  ~a)" (list-ref varlist retval))])
    (string-append (format "(define (synthesized-function ~a)\n" (string-join arg-list))
                   (string-join input-stmt-list "\n")
                   "\n"
                   (string-join synthed-stmt-list "\n")
                   "\n"
                   return-stmt)))

(define (print-from-holes bound-holes retval input-count)
  (displayln (string-from-holes bound-holes retval input-count)))

(define (eval-graph graph . inputs)
  (behavior (apply graph (map behavior-init inputs))
            (apply (curry map graph) (map behavior-changes inputs))))
