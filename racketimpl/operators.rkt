#lang rosette

(require "dense-fjmodels.rkt")
(require "densefjapi.rkt")
(require "specifications.rkt")

(provide (all-defined-out))

(struct stream-insn 
  (op-index arg-index1 arg-index2 arg-index3 λi2i2i-idx λmap-idx
            λi2b2c-idx λi2b-idx λfilter-idx const-idx
            arg-int arg-int2) #:transparent)

(define (get-insn-holes)
  (define-symbolic* op integer?)
  (define-symbolic* stream-idx integer?)
  (define-symbolic* stream2-idx integer?)
  (define-symbolic* stream3-idx integer?)
  (define-symbolic* λi2i2i-idx integer?)
  (define-symbolic* λmap-idx integer?)
  (define-symbolic* λi2b2c-idx integer?)
  (define-symbolic* λi2b-idx integer?)
  (define-symbolic* λfilter-idx integer?)
  (define-symbolic* const-idx integer?)
  (define-symbolic* arg-int integer?)
  (define-symbolic* arg-int2 integer?)
  (stream-insn op stream-idx stream2-idx stream3-idx λi2i2i-idx λmap-idx λi2b2c-idx λi2b-idx λfilter-idx const-idx arg-int arg-int2))

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
            (λ (insn past-vars) (constantE (list-ref constantB-consts (stream-insn-const-idx insn))
                                           (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (list-ref constantB-consts (stream-insn-const-idx insn))
                                        (get-input-stream insn past-vars)))))
(define mergeE-op
  (operator "mergeE"
            (λ (insn past-vars) (mergeE (get-input-stream insn past-vars)
                                        (get-input-stream2 insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (get-input-stream insn past-vars)
                                        (get-input-stream2 insn past-vars)))))

(define collectE-imm-op
  (operator "collectE"
            (λ (insn past-vars) (collectE (get-integer-arg insn) (list-ref
                                                                  inttointtointfuncs
                                                                  (stream-insn-λi2i2i-idx insn))
                                          (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a ~a" (get-integer-arg insn)
                                        (list-ref
                                         inttointtointfuncs-strings
                                         (stream-insn-λi2i2i-idx insn))
                                        (get-input-stream insn past-vars)))))
#;(define collectE-op
  (operator "collectE"
            (λ (insn past-vars) (collectE (list-ref constantB-consts (stream-insn-arg-index2 insn))
                                          (list-ref function-2arg-list (stream-insn-arg-index3 insn))
                                          (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a ~a" (list-ref constantB-consts (stream-insn-arg-index2 insn))
                                        (list-ref function-2arg-list-string (stream-insn-arg-int insn))
                                        (get-input-stream insn past-vars)))))
(define startsWith-imm-op
  (operator "startsWith"
            (λ (insn past-vars) (startsWith (get-integer-arg insn) (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (get-integer-arg insn)
                                        (get-input-stream insn past-vars)))))
(define startsWith-op
  (operator "startsWith"
            (λ (insn past-vars) (startsWith (list-ref constantB-consts (stream-insn-arg-int insn))
                                            (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (list-ref constantB-consts (stream-insn-arg-int insn))
                                        (get-input-stream insn past-vars)))))
(define mapE-op
  (operator "mapE"
            (λ (insn past-vars) (mapE (list-ref (append inttointfuncs inttoboolfuncs) (stream-insn-λmap-idx insn))
                                      (get-integer-arg insn)
                                      (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a"
                                        (format (list-ref (append inttointfuncs-string inttoboolfuncs-string)
                                                          (stream-insn-λmap-idx insn))
                                                (get-integer-arg insn))
                                        (get-input-stream insn past-vars)))))

(define mapE-twoconst-op
  (operator "mapE"
            (λ (insn past-vars) (mapE (list-ref inttoboolfuncs-twoconst (stream-insn-λi2b2c-idx insn))
                                      (get-integer-arg insn) (get-integer-arg2 insn)
                                      (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a"
                                        (format (list-ref inttoboolsfuncs-twoconst-string (stream-insn-λi2b2c-idx insn))
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
(define liftB-op
  (operator "liftB"
            (λ (insn past-vars) (liftB1 (curry (list-ref (append inttointfuncs inttoboolfuncs) (stream-insn-λmap-idx insn)) (get-integer-arg insn))
                                        (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a"
                                        (format (list-ref (append inttointfuncs-string inttoboolfuncs-string) (stream-insn-λmap-idx insn)) (get-integer-arg insn))
                                        (get-input-stream insn past-vars)))))

(define liftB-twoconst-op
  (operator "liftB"
            (λ (insn past-vars) (liftB1 (curry (list-ref inttoboolfuncs-twoconst (stream-insn-λi2b2c-idx insn))
                                               (get-integer-arg insn) (get-integer-arg2 insn))
                                        (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a"
                                        (format (list-ref inttoboolsfuncs-twoconst-string (stream-insn-λi2b2c-idx insn))
                                                (get-integer-arg insn) (get-integer-arg2 insn))
                                        (get-input-stream insn past-vars)))))
            
#;(define liftB2-op
  (operator "liftB2"
            (λ (insn past-vars) (liftB2 (list-ref function-2arg-list (stream-insn-arg-index2 insn))
                                        (list-ref past-vars (stream-insn-arg-index3 insn)) 
                                        (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a ~a" (list-ref function-2arg-list-string (stream-insn-arg-index2 insn))
                                        (list-ref past-vars (stream-insn-arg-index3 insn))
                                        (list-ref past-vars (stream-insn-arg-index1 insn))))))
(define andB-op
  (operator "andB"
            (λ (insn past-vars) (andB (get-input-stream insn past-vars)
                                      (get-input-stream2 insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (get-input-stream insn past-vars)
                                        (get-input-stream2 insn past-vars)))))
(define ifB-op
  (operator "ifB"
            (λ (insn past-vars) (ifB (get-input-stream insn past-vars)
                                     (get-input-stream2 insn past-vars)
                                     (get-input-stream3 insn past-vars)))
            (λ (insn past-vars) (format "~a ~a ~a" (get-input-stream insn past-vars)
                                        (get-input-stream2 insn past-vars)
                                        (get-input-stream3 insn past-vars)))))
(define constantB-imm-op
  (operator "constantB"
            (λ (insn past-vars) (constantB (get-integer-arg insn) (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (get-integer-arg insn) (get-input-stream insn past-vars)))))
(define constantB-op
  (operator "constantB"
            (λ (insn past-vars) (constantB (list-ref constantB-consts (stream-insn-const-idx insn))
                                           (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (list-ref constantB-consts (stream-insn-const-idx insn))
                                        (get-input-stream insn past-vars)))))

(define collectB-op
  (operator "collectB"
            (λ (insn past-vars) (collectB (list-ref constantB-consts (stream-insn-const-idx insn))
                                          (list-ref inttointtointfuncs (stream-insn-λi2i2i-idx insn))
                                          (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a ~a" (list-ref constantB-consts (stream-insn-const-idx insn))
                                        (list-ref inttointtointfuncs (stream-insn-λi2i2i-idx insn))
                                        (get-input-stream insn past-vars)))))
(define collectB-imm-op
  (operator "collectB"
            (λ (insn past-vars) (collectB (get-integer-arg insn) (list-ref
                                                                  inttointtointfuncs
                                                                  (stream-insn-λi2i2i-idx insn))
                                          (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a ~a" (get-integer-arg insn)
                                        (list-ref
                                        inttointtointfuncs-strings
                                         (stream-insn-λi2i2i-idx insn))
                                        (get-input-stream insn past-vars)))))
(define snapshotE-op
  (operator "snapshotE"
            (λ (insn past-vars) (snapshotE (get-input-stream insn past-vars)
                                           (get-input-stream2 insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (get-input-stream insn past-vars)
                                        (get-input-stream2 insn past-vars)))))
#;(define mapE2-op
  (operator "mapE"
            (λ (insn past-vars) (mapE2 (list-ref function-2arg-list (stream-insn-arg-index2 insn))
                                       (get-input-stream insn past-vars)
                                       (list-ref past-vars (stream-insn-arg-index3 insn))))
            (λ (insn past-vars) (format "~a ~a ~a" (list-ref function-2arg-list-string (stream-insn-arg-index2 insn))
                                        (get-input-stream insn past-vars)
                                        (list-ref past-vars (stream-insn-arg-index3 insn))))))
(define delayE-op
  (operator "delayE"
            (λ (insn past-vars) (delayE (get-integer-arg insn) (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (get-integer-arg insn) (get-input-stream insn past-vars)))))
(define filterRepeatsE-op
  (operator "filterRepeatsE"
            (λ (insn past-vars) (filterRepeatsE (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a" (get-input-stream insn past-vars)))))
(define timerE-op
  (operator "timerE"
            (λ (insn past-vars) (timerE (get-integer-arg insn) (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (get-integer-arg insn) (get-input-stream insn past-vars)))))
(define filterE-op
  (operator "filterE"
            (λ (insn past-vars) (filterE (list-ref genericfuncs (stream-insn-λfilter-idx insn)) 0
                                         (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (list-ref genericfuncs-string (stream-insn-λfilter-idx insn))
                                        (get-input-stream insn past-vars)))))
(define filterE-const-op
  (operator "filterE"
            (λ (insn past-vars) (filterE (list-ref inttoboolfuncs (stream-insn-λi2b-idx insn))
                                         (get-integer-arg insn)
                                         (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a"
                                        (format (list-ref inttoboolfuncs-string (stream-insn-λi2b-idx insn))
                                                (get-integer-arg insn))
                                        (get-input-stream insn past-vars)))))
(define changes-op
  (operator "changes"
            (λ (insn past-vars) (changes (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a" (get-input-stream insn past-vars)))))
(define notB-op
  (operator "notB"
            (λ (insn past-vars) (notB (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a" (get-input-stream insn past-vars)))))

(define stateless-operator-list
  (list constantE-imm-op
        constantE-op
        mergeE-op
        mapE-op
        liftB-op
        liftB-twoconst-op
       ; liftB2-op
        andB-op
        ifB-op
        constantB-imm-op
        constantB-op
        snapshotE-op
        ; mapE2-op
        filterE-op
        filterE-const-op
        notB-op
        ifE-op
        ))

(define stateful-operator-list
  (list collectE-imm-op
        ; collectE-op
        startsWith-imm-op
        startsWith-op
        delayE-op
        filterRepeatsE-op
        timerE-op
        ; collectB-op
        collectB-imm-op
        changes-op
        ))

(define operator-list
  (append stateless-operator-list stateful-operator-list))

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
(define genericfuncs (list (λ (c i) (identity i))))

(define genericfuncs-string (list "(λ (e) e)"))

;; int -> int
(define inttointfuncs (list (λ (placeholder p2 i) (+ i placeholder))
                            (λ (placeholder p2 i) (- i placeholder))
                            (λ (placeholder p2 i) (- placeholder i))
                           ; (λ (placeholder i) (* i placeholder))
                           ; (λ (i) (/ i placeholder)) ;; leave out division?
                           ; (λ (i) (/ placeholder i))
                            ))
(define inttointfuncs-string (list "(λ (i) (+ i ~a))"
                                   "(λ (i) (- i ~a))"
                                   "(λ (i) (- ~a i))"
                                  ; "(λ (i) (* i ~a))"
                                   ))
;; int -> bool
(define inttoboolfuncs (list (λ (placeholder p2 i) (<= i placeholder))
                             (λ (placeholder p2 i) (>= i placeholder))
                             (λ (placeholder p2 i) (< i placeholder))
                             (λ (placeholder p2 i) (> i placeholder))
                             (λ (placeholder p2 i) (= i placeholder))
                             ))

(define inttoboolfuncs-string (list "(λ (i) (<= i ~a))"
                                    "(λ (i) (>= i ~a))"
                                    "(λ (i) (< i ~a))"
                                    "(λ (i) (> i ~a))"
                                    "(λ (i) (= i ~a))"
                                    ))

;; note that these can be composed from the inttoboolfuncs
(define inttoboolfuncs-twoconst (list
                                 ;; outside of range
                                 (λ (placeholder placeholder2 i) (or (>= i placeholder) (<= i placeholder2)))
                                 ;; inside of range
                                 (λ (placeholder placeholder2 i) (and (>= i placeholder) (<= i placeholder2)))
                                 ))

(define inttoboolsfuncs-twoconst-string (list
                                         "(λ (i) (or (>= i ~a) (<= i ~a)))"
                                         "(λ (i) (and (>= i ~a) (<= i ~a)))"
                                         ))
;; bool -> bool
;; no need for these because and, or, not are all baked into the operators

;; int -> int -> int
(define inttointtointfuncs (list +
                                 -
                                 ; *
                                 ; /
                                 min
                                 max
                                 ))
(define inttointtointfuncs-strings (list "+"
                                         "-"
                                         ; "*"
                                         ; "/"
                                         "min"
                                         "max"
                                         ))

(define constantB-consts (list 'on 'off #t #f 'test))
