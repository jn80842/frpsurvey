#lang rosette

(require "dense-fjmodels.rkt")
(require "densefjapi.rkt")

(provide (all-defined-out))

(struct stream-insn 
  (op-index arg-index1 arg-index2 arg-index3 arg-int arg-index4) #:transparent)

(define (get-insn-holes)
  (define-symbolic* op integer?)
  (define-symbolic* arg1 integer?)
  (define-symbolic* arg2 integer?)
  (define-symbolic* arg3 integer?)
  (define-symbolic* arg4 integer?)
  (define-symbolic* arg5 integer?)
  (stream-insn op arg1 arg2 arg3 arg4 arg5))

;; what's better way to structure this?
;; struct?
;; shriram-inspired macro/continuation/???
;; look at rosette source code (since this is kind of just reimplementing parts of rosette)

;; for both: what is desired behavior if a bad index is chosen?
;; or, better to constrain with asserts?

;; note: putting index1 arg on every insn is repetitive, but not every instruction has one
;; maybe factor out and add a check that insn needs this arg?
;; also if we DON'T factor out index1 arg, no need for currying

;; note: preferrable to use asserts to guard size of indexes rather than using guarded-access
(define (call-stream-insn insn past-vars)
  (case (op-lookup (stream-insn-op-index insn))
    [("constantE") ((curry constantE (stream-insn-arg-int insn))
                    (guarded-access past-vars (stream-insn-arg-index1 insn)))]
    [("mergeE") ((curry mergeE (guarded-access past-vars (stream-insn-arg-index2 insn)))
                 (guarded-access past-vars (stream-insn-arg-index1 insn)))]
    [("collectE") ((curry collectE (stream-insn-arg-int insn) +)
                   (guarded-access past-vars (stream-insn-arg-index1 insn)))]
    [("startsWith") ((curry startsWith (stream-insn-arg-int insn))
                     (guarded-access past-vars (stream-insn-arg-index1 insn)))]
    [("mapE") ((curry mapE (guarded-access function-list (stream-insn-arg-index2 insn)))
               (guarded-access past-vars (stream-insn-arg-index1 insn)))]
    [("liftB1") ((curry liftB1 (guarded-access function-list (stream-insn-arg-index2 insn)))
                 (guarded-access past-vars (stream-insn-arg-index1 insn)))]
    [("andB") ((curry andB (guarded-access past-vars (stream-insn-arg-index2 insn)))
               (guarded-access past-vars (stream-insn-arg-index1 insn)))]
    [("ifB") (ifB (guarded-access past-vars (stream-insn-arg-index1 insn))
                  (guarded-access past-vars (stream-insn-arg-index2 insn))
                  (guarded-access past-vars (stream-insn-arg-index3 insn)))]
    [("constantB") (constantB (guarded-access constantB-consts (stream-insn-arg-index2 insn))
                              (guarded-access past-vars (stream-insn-arg-index1 insn)))]
    [("delayE") ((curry delayE (stream-insn-arg-int insn))
                 (guarded-access past-vars (stream-insn-arg-index1 insn)))]
    [("liftB2") ((curry liftB2 (guarded-access function-2arg-list (stream-insn-arg-index2 insn))
                        (guarded-access past-vars (stream-insn-arg-index3 insn))) 
                 (guarded-access past-vars (stream-insn-arg-index1 insn)))]
    [("condB") (condB (list (list (guarded-access past-vars (stream-insn-arg-index1 insn))
                                  (guarded-access past-vars (stream-insn-arg-index2 insn)))
                            (list (guarded-access past-vars (stream-insn-arg-index3 insn))
                                  (guarded-access past-vars (stream-insn-arg-index4 insn)))
                            (list (constantB #t (guarded-access past-vars (stream-insn-arg-index1 insn)))
                                  (guarded-access past-vars (stream-insn-arg-int insn)))))]
    [("collectB") (collectB (guarded-access constantB-consts (stream-insn-arg-index2 insn))
                            (guarded-access function-2arg-list (stream-insn-arg-index3 insn))
                            (guarded-access past-vars (stream-insn-arg-index1 insn)))]
    [("delayE1") (delayE1 (guarded-access past-vars (stream-insn-arg-index1 insn)))]
    [("delayE2") (delayE2 (guarded-access past-vars (stream-insn-arg-index1 insn)))]
    [("delayE3") (delayE3 (guarded-access past-vars (stream-insn-arg-index1 insn)))]
    ))

(define (call-stream-insn-full insn past-vars)
  (case (full-lookup (stream-insn-op-index insn))
    [("delayE") ((curry delayE (stream-insn-arg-int insn))
                 (guarded-access past-vars (stream-insn-arg-index1 insn)))]
    [("constantE") ((curry constantE (stream-insn-arg-int insn)) 
                    (guarded-access past-vars (stream-insn-arg-index1 insn)))]
    [else (call-stream-insn insn past-vars)]))

(define (print-single-insn bound-holes varname past-vars)
  (define op (op-lookup (stream-insn-op-index bound-holes)))
  (define op-args (print-stream-insn bound-holes past-vars))
  (format "  (define ~a (~a ~a))" varname op op-args))

(define (print-stream-insn insn past-vars)
  (case (op-lookup (stream-insn-op-index insn))
    [("constantE") (format "~a ~a" (stream-insn-arg-int insn) (list-ref past-vars (stream-insn-arg-index1 insn)))]
    [("mergeE") (format "~a ~a" (list-ref past-vars (stream-insn-arg-index2 insn))
                        (list-ref past-vars (stream-insn-arg-index1 insn)))]
    [("collectE") (format "~a + ~a" (stream-insn-arg-int insn)
                          (list-ref past-vars (stream-insn-arg-index1 insn)))]
    [("startsWith") (format "~a ~a" (stream-insn-arg-int insn)
                            (list-ref past-vars (stream-insn-arg-index1 insn)))]
    [("mapE") (format "~a ~a" (guarded-access function-list-string (stream-insn-arg-index2 insn))
                      (list-ref past-vars (stream-insn-arg-index1 insn)))]
    [("liftB1") (format "~a ~a" (guarded-access function-list-string (stream-insn-arg-index2 insn))
                      (list-ref past-vars (stream-insn-arg-index1 insn)))]
    [("andB") (format "~a ~a" (list-ref past-vars (stream-insn-arg-index2 insn))
                      (list-ref past-vars (stream-insn-arg-index1 insn)))]
    [("ifB") (format "~a ~a ~a" (list-ref past-vars (stream-insn-arg-index1 insn))
                     (list-ref past-vars (stream-insn-arg-index2 insn))
                     (list-ref past-vars (stream-insn-arg-index3 insn)))]
    ;; NB: doesn't print symbols correctly
    [("constantB") (format "~a" (guarded-access constantB-consts (stream-insn-arg-index2 insn)))]
    [("delayE") (format "~a ~a" (stream-insn-arg-int insn)
                        (guarded-access past-vars (stream-insn-arg-index1 insn)))]
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
                          (list-ref past-vars (stream-insn-arg-index1 insn)))]
    [("delayE1") (format "~a" (guarded-access past-vars (stream-insn-arg-index1 insn)))]
    [("delayE2") (format "~a" (guarded-access past-vars (stream-insn-arg-index1 insn)))]
    [("delayE3") (format "~a" (guarded-access past-vars (stream-insn-arg-index1 insn)))]
    ))

(define op-list (list "mergeE" ;; 0
                      "collectE" ;; 1
                      "startsWith" ;; 2
                      "mapE" ;; 3
                      "liftB1" ;; 4
                      "andB" ;; 5
                      "ifB" ;; 6
                      "constantB" ;; 7
                      "liftB2" ;; 8
                      "condB" ;; 9
                      "collectB" ;; 10
                      "constantE" ;; 11
                   ;   "delayE" ;; 12
                      ))


(define (op-lookup idx)
  (list-ref op-list idx))
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
                                        ))

(define constantB-consts (list 'on 'off #t #f 0))

;; prevent rosette from picking illegal indexes
;; (unless asserts are used to do this)

(define (guarded-access lst idx)
  (if (<= (length lst) idx)
      "bad input"
      (list-ref lst idx)))

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
