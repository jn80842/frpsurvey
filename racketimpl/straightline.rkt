#lang rosette

(require "dense-fjmodels.rkt")
(require "densefjapi.rkt")

(provide (all-defined-out))

(struct stream-insn 
  (op-index arg-index1 arg-index2 arg-index3 arg-int) #:transparent)

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
            (λ (insn past-vars) (constantE (list-ref constantB-consts (stream-insn-arg-int insn))
                                           (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (list-ref constantB-consts (stream-insn-arg-int insn))
                                        (get-input-stream insn past-vars)))))
(define mergeE-op
  (operator "mergeE"
            (λ (insn past-vars) (mergeE (list-ref past-vars (stream-insn-arg-index2 insn))
                                        (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (list-ref past-vars (stream-insn-arg-index2 insn))
                                        (get-input-stream insn past-vars)))))
(define collectE-imm-op
  (operator "collectE"
            (λ (insn past-vars) (collectE (get-integer-arg insn) (list-ref function-2arg-list (stream-insn-arg-index2 insn))
                                          (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a ~a" (get-integer-arg insn)
                                        (list-ref function-2arg-list-string (stream-insn-arg-index2 insn))
                                        (get-input-stream insn past-vars)))))
(define collectE-op
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
            (λ (insn past-vars) (format "~a ~a" (stream-insn-arg-int insn)
                                        (get-input-stream insn past-vars)))))
(define startsWith-op
  (operator "startsWith"
            (λ (insn past-vars) (startsWith (list-ref constantB-consts (stream-insn-arg-int insn))
                                            (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (list-ref constantB-consts (stream-insn-arg-int insn))
                                        (get-input-stream insn past-vars)))))
(define mapE-op
  (operator "mapE"
            (λ (insn past-vars) (mapE (list-ref function-list (stream-insn-arg-index2 insn))
                                      (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (list-ref function-list-string (stream-insn-arg-index2 insn))
                                        (get-input-stream insn past-vars)))))
(define liftB1-op
  (operator "liftB1"
            (λ (insn past-vars) (liftB1 (list-ref function-list (stream-insn-arg-index2 insn))
                                        (list-ref past-vars (stream-insn-arg-index1 insn))))
            (λ (insn past-vars) (format "~a ~a" (list-ref function-list-string (stream-insn-arg-index2 insn))
                                        (get-input-stream insn past-vars)))))
(define liftB2-op
  (operator "liftB2"
            (λ (insn past-vars) (liftB2 (list-ref function-2arg-list (stream-insn-arg-index2 insn))
                                        (list-ref past-vars (stream-insn-arg-index3 insn)) 
                                        (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a ~a" (list-ref function-2arg-list-string (stream-insn-arg-index2 insn))
                                        (list-ref past-vars (stream-insn-arg-index3 insn))
                                        (list-ref past-vars (stream-insn-arg-index1 insn))))))
(define andB-op
  (operator "andB"
            (λ (insn past-vars) (andB (list-ref past-vars (stream-insn-arg-index2 insn))
                                      (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (list-ref past-vars (stream-insn-arg-index2 insn))
                                        (list-ref past-vars (stream-insn-arg-index1 insn))))))
(define ifB-op
  (operator "ifB"
            (λ (insn past-vars) (ifB (get-input-stream insn past-vars)
                                     (list-ref past-vars (stream-insn-arg-index2 insn))
                                     (list-ref past-vars (stream-insn-arg-index3 insn))))
            (λ (insn past-vars) (format "~a ~a ~a" (get-input-stream insn past-vars)
                                        (list-ref past-vars (stream-insn-arg-index2 insn))
                                        (list-ref past-vars (stream-insn-arg-index3 insn))))))
(define constantB-imm-op
  (operator "constantB"
            (λ (insn past-vars) (constantB (stream-insn-arg-int insn) (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a" (stream-insn-arg-int insn)))))
(define constantB-op
  (operator "constantB"
            (λ (insn past-vars) (constantB (list-ref constantB-consts (stream-insn-arg-index2 insn))
                                           (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a" (list-ref constantB-consts (stream-insn-arg-index2 insn))))))
(define collectB-op
  (operator "collectB"
            (λ (insn past-vars) (collectB (list-ref constantB-consts (stream-insn-arg-index2 insn))
                                          (list-ref function-2arg-list (stream-insn-arg-index3 insn))
                                          (list-ref past-vars (stream-insn-arg-index1 insn))))
            (λ (insn past-vars) (format "~a ~a ~a" (list-ref constantB-consts (stream-insn-arg-index2 insn))
                                        (list-ref function-2arg-list-string (stream-insn-arg-index3 insn))
                                        (get-input-stream insn past-vars)))))
(define collectB-imm-op
  (operator "collectB"
            (λ (insn past-vars) (collectB (get-integer-arg insn) (list-ref function-2arg-list (stream-insn-arg-index2 insn))
                                          (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a ~a" (get-integer-arg insn)
                                        (list-ref function-2arg-list-string (stream-insn-arg-index2 insn))
                                        (get-input-stream insn past-vars)))))
(define snapshotE-op
  (operator "snapshotE"
            (λ (insn past-vars) (snapshotE (get-input-stream insn past-vars)
                                           (list-ref past-vars (stream-insn-arg-index2 insn))))
            (λ (insn past-vars) (format "~a ~a" (get-input-stream insn past-vars)
                                        (list-ref past-vars (stream-insn-arg-index2 insn))))))
(define mapE2-op
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
            (λ (insn past-vars) (filterE (list-ref function-list (get-integer-arg insn))
                                         (get-input-stream insn past-vars)))
            (λ (insn past-vars) (format "~a ~a" (list-ref function-list-string (get-integer-arg insn))
                                        (get-input-stream insn past-vars)))))

(define stateless-operator-list (list constantE-imm-op
                                      constantE-op
                                      mergeE-op
                                      mapE-op
                                      liftB1-op
                                      liftB2-op
                                      andB-op
                                      ifB-op
                                      constantB-imm-op
                                      constantB-op
                                      snapshotE-op
                                      mapE2-op
                                      filterE-op
                                      ))

(define operator-list (list constantE-imm-op
                            constantE-op
                            mergeE-op
                            mapE-op
                            liftB1-op
                            liftB2-op
                            andB-op
                            ifB-op
                            constantB-imm-op
                            constantB-op
                            snapshotE-op
                            mapE2-op
                            filterE-op
                            collectE-imm-op
                            collectE-op
                            startsWith-imm-op
                            startsWith-op
                            delayE-op
                            filterRepeatsE-op
                            timerE-op
                            collectB-op
                            collectB-imm-op
                            ))

;; these are collectE specific var names
;; change back after experiments
(define (get-insn-holes)
  (define-symbolic* op integer?)
  (define-symbolic* streamidx integer?)
  (define-symbolic* λidx integer?)
  (define-symbolic* arg3 integer?)
  (define-symbolic* arg-int integer?)
  (stream-insn op streamidx λidx arg3 arg-int))

(define (get-input-stream insn past-vars)
  (list-ref past-vars (stream-insn-arg-index1 insn)))
(define (get-integer-arg insn)
  (stream-insn-arg-int insn))

(define (call-stateless-stream-insn insn past-vars)
  (let ([op (list-ref stateless-operator-list (stream-insn-op-index insn))])
    ((operator-call op) insn past-vars)))

(define (call-any-stream-insn insn past-vars)
  (let ([op (list-ref operator-list (stream-insn-op-index insn))])
    ((operator-call op) insn past-vars)))

(define (call-stream-insn state-flag insn past-vars)
  (if state-flag
      (call-any-stream-insn insn past-vars)
      (call-stateless-stream-insn insn past-vars)))

(define (print-stream-insn state-flag insn varname past-vars)
  (let ([op (if state-flag
                (list-ref operator-list (stream-insn-op-index insn))
                (list-ref stateless-operator-list (stream-insn-op-index insn)))])
    (format "  (define ~a (~a ~a))" varname (operator-name op) ((operator-print op) insn past-vars))))

;; these lists are very unsatisfactory
(define function-list (list (λ (e) (+ e 5))
                            (λ (t) (<= t 2))
                            (λ (c) (or (>= c 4) (>= 2 c)))
                            (λ (e) (if e 'on 'off))
                            (λ (c) (or (>= (time-vec-hour c) 4)
                                       (>= 2 (time-vec-hour c))))
                            ;(λ (t) (list-ref table t))
                            ))

(define table (for/list ([i (range 3)]) (define-symbolic* table-sv integer?) table-sv))

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
;(define function-2arg-list (list + -))
;(define function-2arg-list-string (list "+" "-"))
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

(define (string-from-holes bound-holes state-mask retval input-count)
  (let* ([arg-list (for/list ([i (range input-count)])
                    (format "input~a" (add1 i)))]
         [input-stmt-list (for/list ([i (range input-count)])
                            (format "  (define r~a input~a)" (add1 i) (add1 i)))]
         [depth (length bound-holes)]
         [varlist (for/list ([i (range (add1 (+ input-count depth)))])
                    (format "r~a" (add1 i)))]
         [synthed-stmt-list (for/list ([i (range depth)])
                              (print-stream-insn (list-ref state-mask i) (list-ref bound-holes i) (list-ref varlist (+ input-count i))
                                                 (take varlist (+ input-count i))))]
         [return-stmt (format "  ~a)" (list-ref varlist retval))])
    (string-append (format "(define (synthesized-function ~a)\n" (string-join arg-list))
                   (string-join input-stmt-list "\n")
                   "\n"
                   (string-join synthed-stmt-list "\n")
                   "\n"
                   return-stmt)))

;; better parameterize the number of input streams
(define (print-from-holes bound-holes state-mask retval input-count)
  (displayln (string-from-holes bound-holes state-mask retval input-count)))
