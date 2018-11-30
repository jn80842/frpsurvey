#lang rosette

(require "model.rkt")
(require "fjapi.rkt")
;(require "specifications.rkt")

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

(define collectE-imm-op
  (operator "collectE"
            (λ (insn past-vars) (collectE (get-integer-arg insn) (list-ref
                                                                  inttointtointfuncs
                                                                  (stream-insn-option-index insn))
                                          (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a ~a" (get-integer-arg insn)
                                        (list-ref
                                         inttointtointfuncs-strings
                                         (stream-insn-option-index insn))
                                        (get-input-stream insn past-vars)))))

(define startsWith-imm-op
  (operator "startsWith"
            (λ (insn past-vars) (startsWith (get-integer-arg insn) (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (get-integer-arg insn)
                                        (get-input-stream insn past-vars)))))
(define startsWith-op
  (operator "startsWith"
            (λ (insn past-vars) (startsWith (list-ref constantB-consts (stream-insn-option-index insn))
                                            (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (list-ref constantB-consts (stream-insn-option-index insn))
                                        (get-input-stream insn past-vars)))))
(define mapE-op
  (operator "mapE"
            (λ (insn past-vars) (mapE (curry (list-ref (append inttointfuncs inttoboolfuncs) (stream-insn-option-index insn))
                                             (get-integer-arg insn))
                                      (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a"
                                        (format (list-ref (append inttointfuncs-string inttoboolfuncs-string)
                                                          (stream-insn-option-index insn))
                                                (get-integer-arg insn))
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

(define liftB-op
  (operator "liftB"
            (λ (insn past-vars) (liftB (curry (list-ref (append inttointfuncs inttoboolfuncs) (stream-insn-option-index insn)) (get-integer-arg insn)) (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a"
                                        (format (list-ref (append inttointfuncs-string inttoboolfuncs-string) (stream-insn-option-index insn)) (get-integer-arg insn))
                                        (get-input-stream insn past-vars)))))

(define liftB-twoconst-op
  (operator "liftB"
            (λ (insn past-vars) (liftB (curry (list-ref inttoboolfuncs-twoconst (stream-insn-option-index insn))
                                               (get-integer-arg insn) (get-integer-arg2 insn))
                                        (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a"
                                        (format (list-ref inttoboolsfuncs-twoconst-string (stream-insn-option-index insn))
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
            (λ (insn past-vars) (andB (get-input-stream insn past-vars) (list-ref past-vars (stream-insn-arg-index2 insn))))
            (λ (insn past-vars) (format "~a ~a" (get-input-stream insn past-vars) (list-ref past-vars (stream-insn-arg-index2 insn))))))

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
            (λ (insn past-vars) (format "~a" (get-integer-arg insn)))))
(define constantB-op
  (operator "constantB"
            (λ (insn past-vars) (constantB (list-ref constantB-consts (stream-insn-option-index insn))
                                           (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a" (list-ref constantB-consts (stream-insn-option-index insn))))))
#;(define collectB-op
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
                                                                  (stream-insn-option-index insn))
                                          (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a ~a" (get-integer-arg insn)
                                        (list-ref
                                        inttointtointfuncs-strings
                                         (stream-insn-option-index insn))
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
#;(define timerE-op
  (operator "timerE"
            (λ (insn past-vars) (timerE (get-integer-arg insn) (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (get-integer-arg insn) (get-input-stream insn past-vars)))))
(define filterE-op
  (operator "filterE"
            (λ (insn past-vars) (filterE (list-ref genericfuncs (stream-insn-option-index insn))
                                         (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (list-ref genericfuncs-string (stream-insn-option-index insn))
                                        (get-input-stream insn past-vars)))))
(define filterE-const-op
  (operator "filterE"
            (λ (insn past-vars) (filterE (curry (list-ref inttoboolfuncs (stream-insn-option-index insn))
                                                (get-integer-arg insn))
                                         (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a"
                                        (format (list-ref inttoboolfuncs-string (stream-insn-option-index insn))
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
  (list constantE-imm-op ;0
        constantE-op ;1
        mergeE-op ;2
        mapE-op ;3
        mapE-twoconst-op ;4
        liftB-op ;5
        liftB-twoconst-op ;6
        ;liftB2-op
        andB-op ;7
        ifB-op ;8
        constantB-imm-op ;9
        constantB-op ;10
        snapshotE-op ;11
        ; mapE2-op
        filterE-op ;12
        filterE-const-op ;13
        notB-op ;14
        ifE-op ;15
        andE-op ;16
        orE-op ;17
        maskOnE-op ;18
        maskOffE-op ;19
        ))

(define stateful-operator-list
  (list collectE-imm-op ;16
        ; collectE-op
        startsWith-imm-op ;17
        startsWith-op ;18
        delayE-op ;19
        filterRepeatsE-op ;20
        ;timerE-op ;21
        ; collectB-op
        collectB-imm-op ;22
        changes-op ;23
        ))

#;(define operator-list
  (append stateless-operator-list stateful-operator-list))
(define operator-list
  (list andE-op ;0
       ; collectE-imm-op
        constantE-imm-op ;1
        constantE-op ;2
       ; delayE-op
        filterE-const-op ;3
        filterE-op ;4
       ; filterRepeatsE-op
        ifE-op ;5
        mapE-op ;6
       ; mapE-twoconst-op
        maskOffE-op ;7
        maskOnE-op ;8
        mergeE-op ;9
        notE-op ;10
        orE-op ;11
       ; snapshotE-op
        ))

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
(define inttoboolfuncs (list (λ (placeholder i) (<= i placeholder))
                             (λ (placeholder i) (>= i placeholder))
                             (λ (placeholder i) (< i placeholder))
                             (λ (placeholder i) (> i placeholder))
                             (λ (placeholder i) (= i placeholder))
                             (λ (placeholder i) (not (= i placeholder)))
                             ))

(define inttoboolfuncs-string (list "(λ (i) (<= i ~a))"
                                    "(λ (i) (>= i ~a))"
                                    "(λ (i) (< i ~a))"
                                    "(λ (i) (> i ~a))"
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

(define constantB-consts (list 'on 'off #t #f 'test))
