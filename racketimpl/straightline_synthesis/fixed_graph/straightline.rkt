#lang rosette

(require "../../dense-fjmodels.rkt")
(require "../../densefjapi.rkt")

(provide (all-defined-out))

(struct fixed-stream-insn
  (op-index arg-index arg-int) #:transparent)
(struct fixed-comm-insn
  (delay-int constant-idx constant-int) #:transparent)

(define (get-insn-holes)
  (define-symbolic* op integer?)
  (define-symbolic* arg1 integer?)
  (define-symbolic* arg2 integer?)
  (fixed-stream-insn op arg1 arg2))

(define (get-comm-insn-holes)
  (define-symbolic* delay-int integer?)
  (define-symbolic* constant-idx integer?)
  (define-symbolic* constant-int integer?)
  (fixed-comm-insn delay-int constant-idx constant-int))

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
(define (call-fixed-stream-insn insn [input1 '()] [input2 '()] [input3 '()])
  (case (op-lookup (fixed-stream-insn-op-index insn))
    [("identityE") input1]
    [("constantE") (constantE (fixed-stream-insn-arg-int insn) input1)]
    [("mergeE") (mergeE input1 input2)]
    [("collectE") (collectE (fixed-stream-insn-arg-int insn) + input1)]
    [("startsWith") (startsWith (fixed-stream-insn-arg-int insn) input1)]
    [("mapE") (mapE (guarded-access function-list (fixed-stream-insn-arg-index insn)) input1)]
    [("liftB1") (liftB1 (guarded-access function-list (fixed-stream-insn-arg-index insn)) input1)]
    [("andB") (andB input1 input2)]
    [("ifB") (ifB input1 input2 input3)]
    [("constantB") (constantB (guarded-access constantB-consts (fixed-stream-insn-arg-index insn)))]
    [("delayE") (delayE (fixed-stream-insn-arg-int insn) input1)]
    [("liftB2") (liftB2 (guarded-access function-2arg-list (fixed-stream-insn-arg-index insn))
                        input1 input2)]
    [("collectB") (collectB (guarded-access constantB-consts (fixed-stream-insn-arg-index insn))
                            (guarded-access function-2arg-list (fixed-stream-insn-arg-int insn))
                            input1)]
    [("delayE1") (delayE1 input1)]
    [("delayE2") (delayE2 input1)]
    [("delayE3") (delayE3 input1)]
    ))

(define (call-full-fixed-stream-insn insn  [input1 '()] [input2 '()] [input3 '()])
  (case (comm-op-lookup (fixed-stream-insn-op-index insn))
    [("constantE") (constantE (fixed-stream-insn-arg-int insn) input1)]
    [("delayE") (delayE (fixed-stream-insn-arg-int insn) input1)]
    [else (call-fixed-stream-insn insn input1 input2 input3)]))

(define (call-fixed-comm-insn insn input1)
  (delayE (fixed-comm-insn-delay-int insn)
          (if (equal? (fixed-comm-insn-constant-idx insn) -2)
              input1
              (constantE (get-constant (fixed-comm-insn-constant-idx insn) (fixed-comm-insn-constant-int insn))
                         input1))))

(define (print-single-insn insn varname [input-name ""] [input-name2 ""] [input-name3 ""])
  (if (fixed-stream-insn? insn)
      (begin
        (define op (full-op-lookup (fixed-stream-insn-op-index insn)))
        (define op-args (print-fixed-stream-insn insn input-name input-name2 input-name3))
        (format "  (define ~a (~a ~a))" varname op op-args))
      (begin
        (define comm-string (print-fixed-comm-insn insn input-name))
        (format "  (define ~a ~a)" varname comm-string))))

(define (print-fixed-stream-insn insn [input-name ""] [input-name2 ""] [input-name3 ""])
  (case (full-op-lookup (fixed-stream-insn-op-index insn))
    [("constantE") (format "~a ~a" (fixed-stream-insn-arg-int insn) input-name)]
    [("mergeE") (format "~a ~a" input-name input-name2)]
    [("collectE") (format "~a + ~a" (fixed-stream-insn-arg-int insn) input-name)]
    [("startsWith") (format "~a ~a" (fixed-stream-insn-arg-int insn) input-name)]
    [("mapE") (format "~a ~a" (guarded-access function-list-string (fixed-stream-insn-arg-index insn))
                      input-name)]
    [("liftB1") (format "~a ~a" (guarded-access function-list-string (fixed-stream-insn-arg-index insn))
                        input-name)]
    [("andB") (format "~a ~a" input-name input-name2)]
    [("ifB") (format "~a ~a ~a" input-name input-name2 input-name3)]
    ;; NB: doesn't print symbols correctly
    [("constantB") (format "~a" (guarded-access constantB-consts (fixed-stream-insn-arg-index insn)))]
    [("delayE") (format "~a ~a" (fixed-stream-insn-arg-int insn) input-name)]
    [("liftB2") (format "~a ~a ~a" (guarded-access function-2arg-list-string (fixed-stream-insn-arg-index insn))
                        input-name input-name2)]
    [("collectB") (format "~a ~a ~a" (guarded-access constantB-consts (fixed-stream-insn-arg-index insn))
                          (guarded-access function-2arg-list-string (fixed-stream-insn-arg-int insn))
                          input-name)]
    [("delayE1") (format "~a" input-name)]
    [("delayE2") (format "~a" input-name)]
    [("delayE3") (format "~a" input-name)]
    ))

(define (print-fixed-comm-insn insn input-name)
  (let* ([delay-string (if (not (equal? 0 (fixed-comm-insn-delay-int insn)))
                           (format "(delayE ~a " (fixed-comm-insn-delay-int insn))
                           "")]
         [constant-idx (fixed-comm-insn-constant-idx insn)]
         [constant-string (if (not (equal? -2 constant-idx))
                              (if (equal? -1 constant-idx)
                                  (format "(constantE ~a " (fixed-comm-insn-constant-int insn))
                                  (format "(constantE ~a " (guarded-access constantB-consts constant-idx)))
                              "")]
         [closing-parens (format "~a~a" (if (not (equal? -2 constant-idx)) ")" "")
                                 (if (not (equal? 0 (fixed-comm-insn-delay-int insn))) ")" ""))])
    (format "~a~a~a~a" delay-string constant-string input-name closing-parens)))

(define op-list (list "mergeE" ;; 0
                      "collectE" ;; 1
                      "startsWith" ;; 2
                      "mapE" ;; 3
                      "liftB1" ;; 4
                      "andB" ;; 5
                      "ifB" ;; 6
                      "constantB" ;; 7
                      "liftB2" ;; 8
                      "collectB" ;; 9
                      "identityE"
                    ;  "constantE" ;; 10
                    ;  "delayE" ;; 11
                      ))

(define (op-lookup idx)
  (list-ref op-list idx))
(define (comm-op-lookup idx)
  (case idx
    [(-1) "delayE"]
    [(-2) "constantE"]
    [else "not a comm op"]))
(define (full-op-lookup idx)
  (case idx
    [(-1) "delayE"]
    [(-2) "constantE"]
    [else (op-lookup idx)]))

(define (get-constant idx int)
  (if (equal? idx -1)
      int
      (list-ref constantB-consts idx)))

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

(define (string-from-holes bound-holes inputs input-count)
  (let* ([arg-list (for/list ([i (range input-count)])
                    (format "input~a" (add1 i)))]
        [input-stmt-list (for/list ([i (range input-count)])
                           (format "  (define r~a input~a)" (add1 i) (add1 i)))]
        [depth (length bound-holes)]
        [varlist (for/list ([i (range (add1 (+ input-count depth)))])
                                            (format "r~a" (add1 i)))]
        [synthed-stmt-list (for/list ([i (range depth)])
                             (apply (curry print-single-insn (list-ref bound-holes i) (list-ref varlist (+ input-count i)))
                                                (list-ref inputs i)))]
        [return-stmt (format "  r~a)" (+ input-count depth))])
    (string-append (format "(define (synthesized-function ~a)\n" (string-join arg-list))
                   (string-join input-stmt-list "\n")
                   "\n"
                   (string-join synthed-stmt-list "\n")
                   "\n"
                   return-stmt)))

;; better parameterize the number of input streams
(define (print-from-holes bound-holes inputs input-count)
  (displayln (string-from-holes bound-holes inputs input-count)))
