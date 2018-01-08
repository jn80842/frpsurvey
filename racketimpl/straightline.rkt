#lang rosette
(require rosette/lib/synthax)
(require "dense-fjmodels.rkt")
(require "densefjapi.rkt")

(provide (all-defined-out))

;; a stream insn needs to know
;; the 5-odd (symbolic) integers that define it
;; how to execute itself
;; how to print itself

(define op-name-hash (make-hash (list (cons 0 "constantE")
                                   (cons 1 "mergeE")
                                   (cons 2 "collectE")
                                   (cons 3 "startsWith")
                                   (cons 4 "mapE")
                                   (cons 5 "liftB1")
                                   (cons 6 "andB")
                                   (cons 7 "ifB")
                                   (cons 8 "constantB")
                                   (cons 9 "delayE")
                                   (cons 10 "liftB2")
                                   (cons 11 "condB")
                                   (cons 12 "collectB")
                                   )))

(define (op-lookup idx)
  (hash-ref op-name-hash idx))

(define function-list (list (λ (e) (+ e 5))
                            (λ (t) (<= t 2))
                            (λ (c) (or (>= c 4) (>= 2 c)))
                            (λ (e) (if e 'on 'off))))
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
                                   "(λ (c) (or (>= c 4) (>= 2 c)))))"
                                   "(λ (e) (if e 'on 'off))"))
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

(define constantB-consts (list 'on 'off #t #f))

(struct stream-insn 
  (op-index arg-index1 arg-index2 arg-index3 arg-int arg-index4) #:transparent)

;; TODO add asserts to limit range of indexes
(define (get-insn-holes)
  (define-symbolic* op integer?)
  (define-symbolic* arg1 integer?)
  (define-symbolic* arg2 integer?)
  (define-symbolic* arg3 integer?)
  (define-symbolic* arg4 integer?)
  (define-symbolic* arg5 integer?)
  (stream-insn op arg1 arg2 arg3 arg4 arg5))

(define (single-insn holes past-vars)
  ((list-ref (list (curry constantE (stream-insn-arg-int holes)) ;; 0
                   (curry mergeE (guarded-access past-vars (stream-insn-arg-index2 holes))) ;; 1
                   (curry collectE (stream-insn-arg-int holes) +) ;; 2
                   (curry startsWith (stream-insn-arg-int holes)) ;; 3
                   (curry mapE (guarded-access function-list (stream-insn-arg-index2 holes))) ;; 4
                   (curry liftB1 (guarded-access function-list (stream-insn-arg-index2 holes))) ;; 5
                   (curry andB (guarded-access past-vars (stream-insn-arg-index2 holes)))  ;; 6
                   (curry ifB (guarded-access past-vars (stream-insn-arg-index2 holes)) ;; 7
                          (guarded-access past-vars (stream-insn-arg-index3 holes)))
                   (curry constantB (guarded-access constantB-consts (stream-insn-arg-index2 holes))) ;; 8
                   (curry delayE (stream-insn-arg-int holes)) ;; 9
                   (curry liftB2 (guarded-access function-2arg-list (stream-insn-arg-index2 holes))
                          (guarded-access past-vars (stream-insn-arg-index3 holes))) ;; 10
                   (curry condB (list (list (guarded-access past-vars (stream-insn-arg-index1 holes))
                                            (guarded-access past-vars (stream-insn-arg-index2 holes)))
                                      (list (guarded-access past-vars (stream-insn-arg-index3 holes))
                                            (guarded-access past-vars (stream-insn-arg-index4 holes)))
                                      (list (constantB #t)
                                            (guarded-access past-vars (stream-insn-arg-int holes))))) ;; 11
                   (curry collectB (stream-insn-arg-int holes) (guarded-access function-2arg-list (stream-insn-arg-index2 holes))) ;; 12
                   ) (stream-insn-op-index holes))
   (guarded-access past-vars (stream-insn-arg-index1 holes))))

;; the currys above are not lazily evaluated, so a lot of them are nonsense for any insn
;; abuse dynamic typing below to carry on with nonsense values

(define (guarded-access lst idx)
  (if (<= (length lst) idx)
      "bad input"
      (list-ref lst idx)))

;; what's better way to structure this?
;; struct?
;; shriram-inspired macro/continuation/???
;; look at rosette source code (since this is kind of just reimplementing parts of rosette)

(define (print-single-insn insn-holes binding varname past-vars)
  (define op (op-lookup (evaluate (stream-insn-op-index insn-holes) binding)))
  (define op-args (get-args-by-op op insn-holes binding past-vars))
  (format "  (define ~a (~a ~a))" varname op op-args))

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
    [("mapE") (format "~a ~a" (guarded-access function-list-string (evaluate (stream-insn-arg-index2 holes) binding))
                      (list-ref past-vars (evaluate (stream-insn-arg-index1 holes) binding)))]
    [("liftB1") (format "~a ~a" (guarded-access function-list-string (evaluate (stream-insn-arg-index2 holes) binding))
                      (list-ref past-vars (evaluate (stream-insn-arg-index1 holes) binding)))]
    [("andB") (format "~a ~a" (list-ref past-vars (evaluate (stream-insn-arg-index2 holes) binding))
                      (list-ref past-vars (evaluate (stream-insn-arg-index1 holes) binding)))]
    [("ifB") (format "~a ~a ~a" (list-ref past-vars (evaluate (stream-insn-arg-index2 holes) binding))
                     (list-ref past-vars (evaluate (stream-insn-arg-index3 holes) binding))
                     (list-ref past-vars (evaluate (stream-insn-arg-index1 holes) binding)))]
    ;; NB: doesn't print symbols correctly
    [("constantB") (format "~a" (guarded-access constantB-consts (evaluate (stream-insn-arg-index2 holes) binding)))]
    [("delayE") (format "~a ~a" (evaluate (stream-insn-arg-int holes) binding)
                        (guarded-access past-vars (evaluate (stream-insn-arg-index1 holes) binding)))]
    [("liftB2") (format "~a ~a ~a" (guarded-access function-2arg-list-string (evaluate (stream-insn-arg-index2 holes) binding))
                        (list-ref past-vars (evaluate (stream-insn-arg-index3 holes) binding))
                        (list-ref past-vars (evaluate (stream-insn-arg-index1 holes) binding)))]
    [("condB") (format "(~a ~a) (~a ~a) ((constantB #t) ~a)" (list-ref past-vars (evaluate (stream-insn-arg-index1 holes) binding))
                       (list-ref past-vars (evaluate (stream-insn-arg-index2 holes) binding))
                       (list-ref past-vars (evaluate (stream-insn-arg-index3 holes) binding))
                       (list-ref past-vars (evaluate (stream-insn-arg-index4 holes) binding))
                       (list-ref past-vars (evaluate (stream-insn-arg-int holes) binding)))]
    [("collectB") (format "~a ~a ~a" (evaluate (stream-insn-arg-int holes) binding)
                          (guarded-access function-2arg-list-string (evaluate (stream-insn-arg-index2 holes) binding))
                          (list-ref past-vars (evaluate (stream-insn-arg-index1 holes) binding)))]
    [else "fail"]))

;; better parameterize the number of input streams
(define (print-from-holes holes retval binding depth input-count)
  (define arg-list (for/list ([i (range input-count)])
    (format "input~a" (add1 i))))
  (displayln (format "(define (synthesized-function ~a)" (string-join arg-list)))

  (for ([i (range input-count)])
    (displayln (format "  (define r~a input~a)" (add1 i) (add1 i))))

  (define varlist (for/list ([i (range (+ input-count depth))])
                    (format "r~a" (add1 i))))
  (for/list ([i (range depth)])
    (displayln (print-single-insn (list-ref holes i) binding (list-ref varlist (+ input-count i))
                                  (take varlist (+ input-count i)))))
  (displayln (format "  ~a)" (list-ref varlist (evaluate retval binding)))))
