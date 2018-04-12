#lang rosette

(require "api.rkt")

(provide (all-defined-out))

(struct dc-insn (op-index input-idx function-idx int-input) #:transparent)

(define (get-insn-holes)
  (define-symbolic* op integer?)
  (define-symbolic* input-idx integer?)
  (define-symbolic* function-idx integer?)
  (define-symbolic* int-input integer?)
  (dc-insn op input-idx function-idx int-input))

(define (get-holes-list count)
  (for/list ([i (range count)]) (get-insn-holes)))

(define (get-retval-idx)
  (define-symbolic* retval-idx integer?)
  retval-idx)

(struct operator (name call print) #:transparent)

(define (get-op insn)
  (list-ref operator-list (dc-insn-op-index insn)))

(define (call-dc-insn insn vars)
  ((operator-call (get-op insn)) insn vars))

(define (print-dc-insn insn varname vars)
  (let ([op (get-op insn)])
    (format "  (define ~a (~a ~a))" varname (operator-name op)
          ((operator-print op) insn vars))))

(define (get-input-list insn vars)
  (list-ref vars (dc-insn-input-idx insn)))

(define (get-2nd-input-list insn vars)
  (list-ref vars (dc-insn-int-input insn)))

;; important! all operators that take ints are args cannot use
;; fields from the instruction -- they have to come from the vars

(define head-op
  (operator "head"
            (λ (insn vars) (head-dc (get-input-list insn vars)))
            (λ (insn vars) (format "~a" (get-input-list insn vars)))))
(define last-op
  (operator "last"
            (λ (insn vars) (last-dc (get-input-list insn vars)))
            (λ (insn vars) (format "~a" (get-input-list insn vars)))))
(define take-op
  (operator "take"
            (λ (insn vars) (take-dc (get-2nd-input-list insn vars) (get-input-list insn vars)))
            (λ (insn vars) (format "~a ~a" (get-2nd-input-list insn vars) (get-input-list insn vars)))))
(define drop-op
  (operator "drop"
            (λ (insn vars) (drop-dc (get-2nd-input-list insn vars) (get-input-list insn vars)))
            (λ (insn vars) (format "~a ~a" (get-2nd-input-list insn vars) (get-input-list insn vars)))))
(define access-op
  (operator "access"
            (λ (insn vars) (access-dc (get-2nd-input-list insn vars) (get-input-list insn vars)))
            (λ (insn vars) (format "~a ~a" (get-2nd-input-list insn vars) (get-input-list insn vars)))))
(define minimum-op
  (operator "minimum"
            (λ (insn vars) (minimum-dc (get-input-list insn vars)))
            (λ (insn vars) (format "~a" (get-input-list insn vars)))))
(define maximum-op
  (operator "maximum"
            (λ (insn vars) (maximum-dc (get-input-list insn vars)))
            (λ (insn vars) (format "~a" (get-input-list insn vars)))))
(define reverse-op
  (operator "reverse"
            (λ (insn vars) (reverse-dc (get-input-list insn vars)))
            (λ (insn vars) (format "~a" (get-input-list insn vars)))))
(define sort-op
  (operator "sort"
            (λ (insn vars) (sort-dc (get-input-list insn vars)))
            (λ (insn vars) (format "~a" (get-input-list insn vars)))))
(define sum-op
  (operator "sum"
            (λ (insn vars) (sum-dc (get-input-list insn vars)))
            (λ (insn vars) (format "~a" (get-input-list insn vars)))))
(define map-op
  (operator "map"
            (λ (insn vars) (map-dc (list-ref int-to-int-funcs (dc-insn-function-idx insn))
                                   (get-input-list insn vars)))
            (λ (insn vars) (format "~a ~a" (list-ref int-to-int-funcs-string (dc-insn-function-idx insn))
                                   (get-input-list insn vars)))))
(define filter-op
  (operator "filter"
            (λ (insn vars) (filter-dc (list-ref int-to-bool-funcs (dc-insn-function-idx insn))
                                      (get-input-list insn vars)))
            (λ (insn vars) (format "~a ~a" (list-ref int-to-bool-funcs-string (dc-insn-function-idx insn))
                                   (get-input-list insn vars)))))
(define count-op
  (operator "count"
            (λ (insn vars) (count-dc (list-ref int-to-bool-funcs (dc-insn-function-idx insn))
                                     (get-input-list insn vars)))
            (λ (insn vars) (format "~a ~a" (list-ref int-to-bool-funcs-string (dc-insn-function-idx insn))
                                   (get-input-list insn vars)))))
(define zipwith-op
  (operator "zipwith"
            (λ (insn vars) (zipwith-dc (list-ref int-to-int-to-int-funcs (dc-insn-function-idx insn))
                                       (get-input-list insn vars) (list-ref vars (dc-insn-int-input insn))))
            (λ (insn vars) (format "~a ~a ~a" (list-ref int-to-int-to-int-funcs-string (dc-insn-function-idx insn))
                                   (get-input-list insn vars) (list-ref vars (dc-insn-int-input insn))))))
(define scanl1-op
  (operator "scanl1"
            (λ (insn vars) (scanl1-dc (list-ref int-to-int-to-int-funcs (dc-insn-function-idx insn))
                                      (get-input-list insn vars)))
            (λ (insn vars) (format "~a ~a" (list-ref int-to-int-to-int-funcs-string (dc-insn-function-idx insn))
                                   (get-input-list insn vars)))))

(define operator-list (list head-op
                            last-op
                            take-op
                            drop-op
                            access-op
                            minimum-op
                            maximum-op
                            reverse-op
                            sort-op
                            sum-op
                            map-op
                            filter-op
                            count-op
                            zipwith-op
                            scanl1-op))

(define int-to-int-funcs (list (λ (i) (+ i 1))
                               (λ (i) (- i 1))
                               (λ (i) (* i 2))
                               (λ (i) (/ i 2))
                               (λ (i) (expt i 2))
                               (λ (i) (* i 3))
                               (λ (i) (/ i 3))
                               (λ (i) (* i 4))
                               (λ (i) (/ i 4))))
(define int-to-int-funcs-string (list "(λ (i) (+ i 1))"
                                      "(λ (i) (- i 1))"
                                      "(λ (i) (* i 2))"
                                      "(λ (i) (/ i 2))"
                                      "(λ (i) (expt i 2))"
                                      "(λ (i) (* i 3))"
                                      "(λ (i) (/ i 3))"
                                      "(λ (i) (* i 4))"
                                      "(λ (i) (/ i 4))"))
(define int-to-bool-funcs (list even?
                                odd?
                                positive?
                                negative?))
(define int-to-bool-funcs-string (list "even?"
                                       "odd?"
                                       "positive?"
                                       "negative?"))
(define int-to-int-to-int-funcs (list +
                                      -
                                      *
                                      min
                                      max))
(define int-to-int-to-int-funcs-string (list "+"
                                             "-"
                                             "*"
                                             "min"
                                             "max"))