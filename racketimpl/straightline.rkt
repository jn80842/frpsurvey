#lang rosette

(require "dense-fjmodels.rkt")
(require "densefjapi.rkt")

(provide (all-defined-out))

(struct stream-insn 
  (op-index arg-index1 arg-index2 arg-index3 arg-int arg-index4) #:transparent)

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
            (λ (insn past-vars) (constantE (guarded-access constantB-consts (stream-insn-arg-int insn))
                                           (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (guarded-access constantB-consts (stream-insn-arg-int insn))
                                        (get-input-stream insn past-vars)))))
(define mergeE-op
  (operator "mergeE"
            (λ (insn past-vars) (mergeE (guarded-access past-vars (stream-insn-arg-index2 insn))
                                        (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (list-ref past-vars (stream-insn-arg-index2 insn))
                                        (get-input-stream insn past-vars)))))
(define collectE-imm-op
  (operator "collectE"
            (λ (insn past-vars) (collectE (get-integer-arg insn) (guarded-access function-2arg-list (stream-insn-arg-index2 insn))
                                          (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a ~a" (get-integer-arg insn)
                                        (guarded-access function-2arg-list-string (stream-insn-arg-index2 insn))
                                        (get-input-stream insn past-vars)))))
(define collectE-op
  (operator "collectE"
            (λ (insn past-vars) (collectE (guarded-access constantB-consts (stream-insn-arg-index2 insn))
                                          (guarded-access function-2arg-list (stream-insn-arg-index3 insn))
                                          (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a ~a" (guarded-access constantB-consts (stream-insn-arg-index2 insn))
                                        (guarded-access function-2arg-list-string (stream-insn-arg-int insn))
                                        (get-input-stream insn past-vars)))))
(define startsWith-imm-op
  (operator "startsWith"
            (λ (insn past-vars) (startsWith (get-integer-arg insn) (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (stream-insn-arg-int insn)
                                        (get-input-stream insn past-vars)))))
(define startsWith-op
  (operator "startsWith"
            (λ (insn past-vars) (startsWith (guarded-access constantB-consts (stream-insn-arg-int insn))
                                            (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (guarded-access constantB-consts (stream-insn-arg-int insn))
                                        (get-input-stream insn past-vars)))))
(define mapE-op
  (operator "mapE"
            (λ (insn past-vars) (mapE (guarded-access function-list (stream-insn-arg-index2 insn))
                                      (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (guarded-access function-list-string (stream-insn-arg-index2 insn))
                                        (get-input-stream insn past-vars)))))
(define liftB1-op
  (operator "liftB1"
            (λ (insn past-vars) (liftB1 (guarded-access function-list (stream-insn-arg-index2 insn))
                                        (guarded-access past-vars (stream-insn-arg-index1 insn))))
            (λ (insn past-vars) (format "~a ~a" (guarded-access function-list-string (stream-insn-arg-index2 insn))
                                        (get-input-stream insn past-vars)))))
(define liftB2-op
  (operator "liftB2"
            (λ (insn past-vars) (liftB2 (guarded-access function-2arg-list (stream-insn-arg-index2 insn))
                                        (guarded-access past-vars (stream-insn-arg-index3 insn)) 
                                        (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a ~a" (guarded-access function-2arg-list-string (stream-insn-arg-index2 insn))
                                        (list-ref past-vars (stream-insn-arg-index3 insn))
                                        (list-ref past-vars (stream-insn-arg-index1 insn))))))
(define andB-op
  (operator "andB"
            (λ (insn past-vars) (andB (guarded-access past-vars (stream-insn-arg-index2 insn))
                                      (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (list-ref past-vars (stream-insn-arg-index2 insn))
                                        (list-ref past-vars (stream-insn-arg-index1 insn))))))
(define ifB-op
  (operator "ifB"
            (λ (insn past-vars) (ifB (guarded-access past-vars (stream-insn-arg-index1 insn))
                                     (guarded-access past-vars (stream-insn-arg-index2 insn))
                                     (guarded-access past-vars (stream-insn-arg-index3 insn))))
            (λ (insn past-vars) (format "~a ~a ~a" (list-ref past-vars (stream-insn-arg-index1 insn))
                                        (list-ref past-vars (stream-insn-arg-index2 insn))
                                        (list-ref past-vars (stream-insn-arg-index3 insn))))))
(define constantB-imm-op
  (operator "constantB"
            (λ (insn past-vars) (constantB (stream-insn-arg-int insn) (guarded-access past-vars (stream-insn-arg-index1 insn))))
            (λ (insn past-vars) (format "~a" (stream-insn-arg-int insn)))))
(define constantB-op
  (operator "constantB"
            (λ (insn past-vars) (constantB (guarded-access constantB-consts (stream-insn-arg-index2 insn))
                                           (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a" (guarded-access constantB-consts (stream-insn-arg-index2 insn))))))
(define collectB-op
  (operator "collectB"
            (λ (insn past-vars) (collectB (guarded-access constantB-consts (stream-insn-arg-index2 insn))
                                          (guarded-access function-2arg-list (stream-insn-arg-index3 insn))
                                          (guarded-access past-vars (stream-insn-arg-index1 insn))))
            (λ (insn past-vars) (format "~a ~a ~a" (guarded-access constantB-consts (stream-insn-arg-index2 insn))
                                        (guarded-access function-2arg-list-string (stream-insn-arg-index3 insn))
                                        (get-input-stream insn past-vars)))))
(define collectB-imm-op
  (operator "collectB"
            (λ (insn past-vars) (collectB (get-integer-arg insn) (guarded-access function-2arg-list (stream-insn-arg-index2 insn))
                                          (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a ~a" (get-integer-arg insn)
                                        (guarded-access function-2arg-list-string (stream-insn-arg-index2 insn))
                                        (get-input-stream insn past-vars)))))

(define operator-list (list constantE-imm-op
                            constantE-op
                            mergeE-op
                            collectE-imm-op
                            collectE-op
                            startsWith-imm-op
                            startsWith-op
                            mapE-op
                            liftB1-op
                            liftB2-op
                            andB-op
                            ifB-op
                            constantB-imm-op
                            constantB-op
                           ; collectB-op
                            collectB-imm-op
                            ))

(define (get-insn-holes)
  (define-symbolic* op integer?)
  (define-symbolic* arg1 integer?)
  (define-symbolic* arg2 integer?)
  (define-symbolic* arg3 integer?)
  (define-symbolic* arg4 integer?)
  (define-symbolic* arg5 integer?)
  (stream-insn op arg1 arg2 arg3 arg4 arg5))

(define (get-input-stream insn past-vars)
  (guarded-access past-vars (stream-insn-arg-index1 insn)))
(define (get-integer-arg insn)
  (stream-insn-arg-int insn))

;; what's better way to structure this?
;; struct?
;; shriram-inspired macro/continuation/???
;; look at rosette source code (since this is kind of just reimplementing parts of rosette)

;; for both: what is desired behavior if a bad index is chosen?
;; or, better to constrain with asserts?

;; note: putting index1 arg on every insn is repetitive, but not every instruction has one
;; maybe factor out and add a check that insn needs this arg?
;; also if we DON'T factor out index1 arg, no need for currying

;; note: preferrable to use asserts to guard size of indexes rather than using guarded-access (?)
(define (call-stream-insn insn past-vars)
  (struct-call-stream-insn insn past-vars))
(define (dumb-call-stream-insn insn past-vars)
  (let ([evalled-insn (list-ref (list (λ () (constantE (get-integer-arg insn) (get-input-stream insn past-vars)))
                                      (λ () (constantE (guarded-access constantB-consts (stream-insn-arg-int insn))
                                             (get-input-stream insn past-vars)))
                                      (λ () (mergeE (guarded-access past-vars (stream-insn-arg-index2 insn))
                                          (get-input-stream insn past-vars)))
                                      (λ () (collectE (get-integer-arg insn) (guarded-access function-2arg-list (stream-insn-arg-index2 insn))
                                            (get-input-stream insn past-vars)))
                                      )
                                (stream-insn-op-index insn))])
    (evalled-insn)))
(define (struct-call-stream-insn insn past-vars)
  (let ([op (list-ref operator-list (stream-insn-op-index insn))])
    ((operator-call op) insn past-vars)))
(define (list-style-call-stream-insn insn past-vars)
  (let ([evalled-insn (list-ref (list (λ () (constantE (get-integer-arg insn) (get-input-stream insn past-vars)))
                            (λ () (constantE (guarded-access constantB-consts (stream-insn-arg-int insn))
                                             (get-input-stream insn past-vars)))
                            (λ () (mergeE (guarded-access past-vars (stream-insn-arg-index2 insn))
                                          (get-input-stream insn past-vars)))
                            (λ () (collectE (get-integer-arg insn) (guarded-access function-2arg-list (stream-insn-arg-index2 insn))
                                            (get-input-stream insn past-vars)))
                            (λ () (collectE (guarded-access constantB-consts (stream-insn-arg-index2 insn))
                                            (guarded-access function-2arg-list (stream-insn-arg-index3 insn))
                                            (get-input-stream insn past-vars)))
                            (λ () (startsWith (get-integer-arg insn) (get-input-stream insn past-vars)))
                            (λ () (startsWith (guarded-access constantB-consts (stream-insn-arg-int insn))
                                              (get-input-stream insn past-vars)))
                            (λ () (mapE (guarded-access function-list (stream-insn-arg-index2 insn))
                                        (get-input-stream insn past-vars)))
                            (λ () (liftB1 (guarded-access function-list (stream-insn-arg-index2 insn))
                                          (guarded-access past-vars (stream-insn-arg-index1 insn))))
                            (λ () (andB (guarded-access past-vars (stream-insn-arg-index2 insn))
                                        (get-input-stream insn past-vars)))
                            (λ () (ifB (guarded-access past-vars (stream-insn-arg-index1 insn))
                                       (guarded-access past-vars (stream-insn-arg-index2 insn))
                                       (guarded-access past-vars (stream-insn-arg-index3 insn))))
                            (λ () (constantB (stream-insn-arg-int insn) (guarded-access past-vars (stream-insn-arg-index1 insn))))
                            (λ () (constantB (guarded-access constantB-consts (stream-insn-arg-index2 insn))
                                             (get-input-stream insn past-vars)))
                            (λ () (liftB2 (guarded-access function-2arg-list (stream-insn-arg-index2 insn))
                                          (guarded-access past-vars (stream-insn-arg-index3 insn)) 
                                          (get-input-stream insn past-vars)))
                            (λ () (collectB (guarded-access constantB-consts (stream-insn-arg-index2 insn))
                                            (guarded-access function-2arg-list (stream-insn-arg-index3 insn))
                                            (guarded-access past-vars (stream-insn-arg-index1 insn))))
                            (λ () (collectB (get-integer-arg insn) (guarded-access function-2arg-list (stream-insn-arg-index2 insn))
                                            (get-input-stream insn past-vars)))) (stream-insn-op-index insn))])
    (evalled-insn))
  )
(define (orig-call-stream-insn insn past-vars)
  (case (op-lookup (stream-insn-op-index insn))
    [("constantE-imm") (constantE (get-integer-arg insn) (get-input-stream insn past-vars))]
    [("constantE") (constantE (guarded-access constantB-consts (stream-insn-arg-int insn))
                              (get-input-stream insn past-vars))]
    [("mergeE") (mergeE (guarded-access past-vars (stream-insn-arg-index2 insn))
                        (get-input-stream insn past-vars))]
    [("collectE-imm") (collectE (get-integer-arg insn) (guarded-access function-2arg-list (stream-insn-arg-index2 insn))
                                    (get-input-stream insn past-vars))]
    [("collectE") (collectE (guarded-access constantB-consts (stream-insn-arg-index2 insn))
                            (guarded-access function-2arg-list (stream-insn-arg-index3 insn))
                            (get-input-stream insn past-vars))]
    [("startsWith-imm") (startsWith (get-integer-arg insn) (get-input-stream insn past-vars))]
    [("startsWith") (startsWith (guarded-access constantB-consts (stream-insn-arg-int insn))
                                (get-input-stream insn past-vars))]
    [("mapE") (mapE (guarded-access function-list (stream-insn-arg-index2 insn))
                    (get-input-stream insn past-vars))]
    [("liftB1") (liftB1 (guarded-access function-list (stream-insn-arg-index2 insn))
                        (guarded-access past-vars (stream-insn-arg-index1 insn)))]
    [("andB") (andB (guarded-access past-vars (stream-insn-arg-index2 insn))
                    (get-input-stream insn past-vars))]
    [("ifB") (ifB (guarded-access past-vars (stream-insn-arg-index1 insn))
                  (guarded-access past-vars (stream-insn-arg-index2 insn))
                  (guarded-access past-vars (stream-insn-arg-index3 insn)))]
    [("constantB-imm") (constantB (stream-insn-arg-int insn) (guarded-access past-vars (stream-insn-arg-index1 insn)))]
    [("constantB") (constantB (guarded-access constantB-consts (stream-insn-arg-index2 insn))
                              (get-input-stream insn past-vars))]
    [("delayE") (delayE (get-integer-arg insn) (get-input-stream insn past-vars))]
    [("liftB2") (liftB2 (guarded-access function-2arg-list (stream-insn-arg-index2 insn))
                        (guarded-access past-vars (stream-insn-arg-index3 insn)) 
                        (get-input-stream insn past-vars))]
    [("condB") (condB (list (list (guarded-access past-vars (stream-insn-arg-index1 insn))
                                  (guarded-access past-vars (stream-insn-arg-index2 insn)))
                            (list (guarded-access past-vars (stream-insn-arg-index3 insn))
                                  (guarded-access past-vars (stream-insn-arg-index4 insn)))
                            (list (constantB #t (guarded-access past-vars (stream-insn-arg-index1 insn)))
                                  (guarded-access past-vars (stream-insn-arg-int insn)))))]
    [("collectB") (collectB (guarded-access constantB-consts (stream-insn-arg-index2 insn))
                            (guarded-access function-2arg-list (stream-insn-arg-index3 insn))
                            (guarded-access past-vars (stream-insn-arg-index1 insn)))]
    [("collectB-imm") (collectB (get-integer-arg insn) (guarded-access function-2arg-list (stream-insn-arg-index2 insn))
                                    (get-input-stream insn past-vars))]
    [("delayE1") (delayE1 (get-input-stream insn past-vars))]
    [("delayE2") (delayE2 (get-input-stream insn past-vars))]
    [("delayE3") (delayE3 (get-input-stream insn past-vars))]
    [("snapshotE") (snapshotE (get-input-stream insn past-vars)
                              (guarded-access past-vars (stream-insn-arg-index2 insn)))]
    [("mapE2") (mapE2 (guarded-access function-2arg-list (stream-insn-arg-index1 insn))
                      (guarded-access past-vars (stream-insn-arg-index2 insn))
                      (guarded-access past-vars (stream-insn-arg-index3 insn)))]
    ))

(define (call-stream-insn-full insn past-vars)
  (case (full-lookup (stream-insn-op-index insn))
    [("delayE") ((curry delayE (stream-insn-arg-int insn))
                 (get-input-stream insn past-vars))]
    [("constantE") ((curry constantE (stream-insn-arg-int insn)) 
                    (get-input-stream insn past-vars))]
    [else (call-stream-insn insn past-vars)]))

(define (print-single-insn bound-holes varname past-vars)
  (define op (operator-name (list-ref operator-list (stream-insn-op-index bound-holes))))
    ;(op-lookup (stream-insn-op-index bound-holes)))
  (define op-args (print-struct-stream-insn bound-holes past-vars))
  (format "  (define ~a (~a ~a))" varname op op-args))

(define (print-struct-stream-insn insn past-vars)
  (let ([op (list-ref operator-list (stream-insn-op-index insn))])
    ((operator-print op) insn past-vars)))

(define (print-stream-insn insn past-vars)
  (case (op-lookup (stream-insn-op-index insn))
    [("constantE-imm") (format "~a ~a" (stream-insn-arg-int insn)
                               (get-input-stream insn past-vars))]
    [("constantE") (format "~a ~a" (guarded-access constantB-consts (stream-insn-arg-int insn))
                           (get-input-stream insn past-vars))]
    [("mergeE") (format "~a ~a" (list-ref past-vars (stream-insn-arg-index2 insn))
                        (get-input-stream insn past-vars))]
    [("collectE-imm") (format "~a ~a ~a" (get-integer-arg insn) (guarded-access function-2arg-list-string (stream-insn-arg-index2 insn))
                              (get-input-stream insn past-vars))]
    [("collectE") (format "~a ~a ~a" (guarded-access constantB-consts (stream-insn-arg-index2 insn))
                          (guarded-access function-2arg-list-string (stream-insn-arg-int insn))
                          (get-input-stream insn past-vars))]
    [("startsWith-imm") (format "~a ~a" (stream-insn-arg-int insn)
                            (get-input-stream insn past-vars))]
    [("startsWith") (format "~a ~a" (guarded-access constantB-consts (stream-insn-arg-int insn))
                                     (get-input-stream insn past-vars))]
    [("mapE") (format "~a ~a" (guarded-access function-list-string (stream-insn-arg-index2 insn))
                      (get-input-stream insn past-vars))]
    [("liftB1") (format "~a ~a" (guarded-access function-list-string (stream-insn-arg-index2 insn))
                      (get-input-stream insn past-vars))]
    [("andB") (format "~a ~a" (list-ref past-vars (stream-insn-arg-index2 insn))
                      (list-ref past-vars (stream-insn-arg-index1 insn)))]
    [("ifB") (format "~a ~a ~a" (list-ref past-vars (stream-insn-arg-index1 insn))
                     (list-ref past-vars (stream-insn-arg-index2 insn))
                     (list-ref past-vars (stream-insn-arg-index3 insn)))]
    [("constantB-imm") (format "~a" (stream-insn-arg-int insn))]
    ;; NB: doesn't print symbols correctly
    [("constantB") (format "~a" (guarded-access constantB-consts (stream-insn-arg-index2 insn)))]
    [("delayE") (format "~a ~a" (stream-insn-arg-int insn)
                        (get-input-stream insn past-vars))]
    [("liftB2") (format "~a ~a ~a" (guarded-access function-2arg-list-string (stream-insn-arg-index2 insn))
                        (list-ref past-vars (stream-insn-arg-index3 insn))
                        (list-ref past-vars (stream-insn-arg-index1 insn)))]
    [("condB") (format "(~a ~a) (~a ~a) ((constantB #t) ~a)" (list-ref past-vars (stream-insn-arg-index1 insn))
                       (list-ref past-vars (stream-insn-arg-index2 insn))
                       (list-ref past-vars (stream-insn-arg-index3 insn))
                       (list-ref past-vars (stream-insn-arg-index4 insn))
                       (list-ref past-vars (stream-insn-arg-int insn)))]
    [("collectB") (format "~a ~a ~a" (guarded-access constantB-consts (stream-insn-arg-index2 insn))
                          (guarded-access function-2arg-list-string (stream-insn-arg-index3 insn))
                          (get-input-stream insn past-vars))]
    [("collectB-imm") (format "~a ~a ~a" (get-integer-arg insn) (guarded-access function-2arg-list-string (stream-insn-arg-index2 insn))
                              (get-input-stream insn past-vars))]
    [("delayE1") (format "~a" (get-input-stream insn past-vars))]
    [("delayE2") (format "~a" (get-input-stream insn past-vars))]
    [("delayE3") (format "~a" (get-input-stream insn past-vars))]
    [("snapshotE") (format "~a ~a" (get-input-stream insn past-vars)
                           (guarded-access past-vars (stream-insn-arg-index2 insn)))]
    [("mapE2") (format "~a ~a ~a" (guarded-access function-2arg-list-string (stream-insn-arg-index1 insn))
                       (guarded-access past-vars (stream-insn-arg-index2 insn))
                       (guarded-access past-vars (stream-insn-arg-index3 insn)))]
    ))

(define op-list (list "mergeE"
                      "collectE"
                      "collectE-imm"
                      "startsWith"
                      "startsWith-imm"
                      "mapE"
                      "liftB1"
                      "andB"
                      "ifB"
                      "constantB"
                      "constantB-imm"
                      "liftB2"
                    ;  "condB"
                    ;  "collectB"
                      "collectB-imm"
                      "constantE"
                      "constantE-imm"
                     ; "delayE"
                      ;"snapshotE"
                      ;"mapE2"
                      ))


(define (op-lookup idx)
  (case idx
    [(0) "mergeE"]
    [(1) "collectE"]
    [(2) "collectE-imm"]
    [(3) "startsWith"]
    [(4) "startsWith-imm"]
    [(5) "mapE"]
   ; [(6) "mapE2"]
    [(7) "liftB1"]
    [(8) "liftB2"]
    [(9) "andB"]
    [(10) "ifB"]
    [(11) "constantB"]
    [(12) "constantB-imm"]
   ; [(13) "collectB"]
    [(14) "collectB-imm"]
   ; [(15) "snapshotE"]
    [(-1) "constantE"]
    [(-2) "constantE-imm"]
    ;[(-3) "delayE"]
    ))

(define (full-lookup idx)
  (case idx
    [(-1) "delayE"]
    [(-2) "constantE"]
    [else (op-lookup idx)]))


;; these lists are very unsatisfactory
(define function-list (list (λ (e) (+ e 5))
                            (λ (t) (<= t 2))
                            (λ (c) (or (>= c 4) (>= 2 c)))
                            (λ (e) (if e 'on 'off))
                            (λ (c) (or (>= (time-vec-hour c) 4)
                                       (>= 2 (time-vec-hour c))))
                            ))
(define function-2arg-list (list (λ (clock location) (if (or (>= clock 4) (< clock 2))
                                 'night
                                 (if (equal? location 'home)
                                     'home
                                     'away)))
                                 (λ (elt1 elt2) (+ elt1 elt2))
                                 (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))
                                 (λ (rain clock) (if (is-midnight? clock) 'midnight rain))
                                 (λ (r prev) (if (eq? r 'midnight) #f
                                                 (if r #t prev)))
                                 (λ (rain clock) (and (not rain)
                                                  (eq? (time-vec-hour clock) 18)
                                                  (< (time-vec-min1 clock) 1)))
                                 (λ (x y) (if x y 'no-evt))
                                 (λ (x y) (if x y x))
                                 ))
(define function-list-string (list "(λ (e) (+ e 5))"
                                   "(λ (t) (<= t 2))"
                                   "(λ (c) (or (>= c 4) (>= 2 c)))"
                                   "(λ (e) (if e 'on 'off))"
                                   "(λ (c) (or (>= (time-vec-hour c) 4)
                                               (>= 2 (time-vec-hour c))))"
                                   ))
(define function-2arg-list-string (list "(λ (clock location) (if (or (>= clock hour-begin) (< clock hour-end))
                                 'night
                                 (if (equal? location 'home)
                                     'home
                                     'away)))"
                                        "(λ (elt1 elt2) (+ elt1 elt2))"
                                        "(λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))"
                                        "(λ (rain clock) (if (is-midnight? clock) 'midnight rain))"
                                        "(λ (r prev) (if (eq? r 'midnight) #f
                                                     (if r #t prev)))"
                                        "(λ (rain clock) (and (not rain)
                                                  (eq? (time-vec-hour clock) 18)
                                                  (< (time-vec-min1 clock) 1)))"
                                        "(λ (x y) (if x y 'no-evt))"
                                        "(λ (x y) (if x y x))"
                                        ))

(define constantB-consts (list 'on 'off #t #f 'test))

;; prevent rosette from picking illegal indexes
;; (unless asserts are used to do this)

(define (guarded-access lst idx)
 ; (if (<= (length lst) idx)
 ;     "bad input"
      (list-ref lst idx));)

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

;; better parameterize the number of input streams
(define (print-from-holes bound-holes retval input-count)
  (displayln (string-from-holes bound-holes retval input-count)))
