#lang rosette

(require "api.rkt")

(provide (all-defined-out))

(struct dc-insn (op-index
                 input-idx
                 input2-idx
                 λi2i-idx
                 λi2b-idx
                 λi2i2i-idx) #:transparent)

(define (get-insn-holes)
  (define-symbolic* op integer?)
  (define-symbolic* input-idx integer?)
  (define-symbolic* input2-idx integer?)
  (define-symbolic* λi2i-idx integer?)
  (define-symbolic* λi2b-idx integer?)
  (define-symbolic* λi2i2i-idx integer?)
  (define-symbolic* int-input integer?)
  (dc-insn op input-idx input2-idx λi2i-idx λi2b-idx λi2i2i-idx))

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
    (format "  (define ~a (~a-dc ~a))" varname (operator-name op)
          ((operator-print op) insn vars))))

(define (get-input-list insn vars)
  (list-ref vars (dc-insn-input-idx insn)))

(define (get-2nd-input-list insn vars)
  (list-ref vars (dc-insn-input2-idx insn)))

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
            (λ (insn vars) (map-dc (list-ref int-to-int-funcs (dc-insn-λi2i-idx insn))
                                   (get-input-list insn vars)))
            (λ (insn vars) (format "~a ~a" (list-ref int-to-int-funcs-string (dc-insn-λi2i-idx insn))
                                   (get-input-list insn vars)))))
(define filter-op
  (operator "filter"
            (λ (insn vars) (filter-dc (list-ref int-to-bool-funcs (dc-insn-λi2b-idx insn))
                                      (get-input-list insn vars)))
            (λ (insn vars) (format "~a ~a" (list-ref int-to-bool-funcs-string (dc-insn-λi2b-idx insn))
                                   (get-input-list insn vars)))))
(define count-op
  (operator "count"
            (λ (insn vars) (count-dc (list-ref int-to-bool-funcs (dc-insn-λi2b-idx insn))
                                     (get-input-list insn vars)))
            (λ (insn vars) (format "~a ~a" (list-ref int-to-bool-funcs-string (dc-insn-λi2b-idx insn))
                                   (get-input-list insn vars)))))
(define zipwith-op
  (operator "zipwith"
            (λ (insn vars) (zipwith-dc (list-ref int-to-int-to-int-funcs (dc-insn-λi2i2i-idx insn))
                                       (get-input-list insn vars) (get-2nd-input-list insn vars)))
            (λ (insn vars) (format "~a ~a ~a" (list-ref int-to-int-to-int-funcs-string (dc-insn-λi2i2i-idx insn))
                                   (get-input-list insn vars) (get-2nd-input-list insn vars)))))
(define scanl1-op
  (operator "scanl1"
            (λ (insn vars) (scanl1-dc (list-ref int-to-int-to-int-funcs (dc-insn-λi2i2i-idx insn))
                                      (get-input-list insn vars)))
            (λ (insn vars) (format "~a ~a" (list-ref int-to-int-to-int-funcs-string (dc-insn-λi2i2i-idx insn))
                                   (get-input-list insn vars)))))

(define int-operator-list (list head-op
                                last-op
                                access-op
                                minimum-op
                                maximum-op))
(define list-operator-list (list take-op
                                 drop-op
                                 reverse-op
                                 sort-op
                                 sum-op
                                 map-op
                                 filter-op
                                 count-op
                                 zipwith-op
                                 scanl1-op))

(define operator-list (append int-operator-list list-operator-list))

#;(define operator-list (list head-op
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
                            scanl1-op
                            ))