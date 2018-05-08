#lang rosette

(require "dense-fjmodels.rkt")
(require "operators.rkt")
(require "specifications.rkt")

(provide (all-defined-out))

(struct sketch (holes state-mask stateless-op-list stateful-op-list input-count) #:transparent)

(define (full-op-list sk)
  (append (sketch-stateless-op-list sk) (sketch-stateful-op-list sk)))

(define (operator-lookup sk insn-idx [binding #f])
  (let* ([insn (if binding
                   (evaluate (list-ref (sketch-holes sk) insn-idx) binding)
                   (list-ref (sketch-holes sk) insn-idx))]
         [op-id (stream-insn-op-index insn)])
    (if (vector-ref (sketch-state-mask sk) insn-idx)
        (list-ref (full-op-list sk) op-id)
        (list-ref (sketch-stateless-op-list sk) op-id))))

(define (string-from-sketch sk binding funcname)
  (let* ([input-count (sketch-input-count sk)]
         [arg-list (for/list ([i (range input-count)])
                     (format "input~a" (add1 i)))]
         [input-stmt-list (for/list ([i (range input-count)])
                            (format "  (define r~a input~a)" (add1 i) (add1 i)))]
         [depth (length (sketch-holes sk))]
         [varlist (for/list ([i (range (+ input-count depth))])
                    (format "r~a" (add1 i)))]
         [stmt-list (for/list ([i (range depth)])
                      (print-stream-insn (operator-lookup sk i binding)
                                         (list-ref (evaluate (sketch-holes sk) binding) i)
                                         (list-ref varlist (+ input-count i))
                                         (take varlist (+ input-count i))))]
         [return-stmt (format "  ~a)" (last varlist))])
    (string-append (format "(define (~a ~a)\n" funcname (string-join arg-list))
                   (string-join input-stmt-list "\n")
                   "\n"
                   (string-join stmt-list "\n")
                   "\n"
                   return-stmt)))

(define (print-sketch sk binding [funcname "synthesized-function"])
  (displayln (string-from-sketch sk binding funcname)))

(define (execute-sketch sk inputs [binding #f])
  (let ([sketch-program (if binding
                            (get-sketch-function (evaluate sk binding))
                            (get-sketch-function sk))])
    (apply sketch-program (get-inputs inputs))))

(define (get-sketch-function sk)
  (letrec ([f (λ (calculated-streams i)
                (cond [(equal? (length (sketch-holes sk)) i) calculated-streams]
                      [else (let ([next-stream (call-stream-insn (operator-lookup sk i)
                                                                 (list-ref (sketch-holes sk) i)
                                                                 calculated-streams)])
                              (f (append calculated-streams (list next-stream)) (add1 i)))]))])
    (λ inputs (last (f inputs 0)))))

(define (get-bound-sketch-function sk binding)
  (letrec ([f (λ (calculated-streams i)
                (cond [(equal? (length (sketch-holes sk)) i) calculated-streams]
                      [else (let ([next-stream (call-stream-insn (operator-lookup sk i binding)
                                                                 (list-ref (evaluate (sketch-holes sk) binding) i)
                                                                 calculated-streams)])
                              (f (append calculated-streams (list next-stream)) (add1 i)))]))])
    (λ inputs (last (f inputs 0)))))

#;(define (synth-from-ref-impl sk ref-impl . inputs)
  (let ([sketch-program (get-sketch-function sk)])
    (begin (define binding (time (synthesize #:forall (apply harvest inputs)
                                             #:guarantee (assert (apply (curry same ref-impl sketch-program) inputs)))))
           (if (unsat? binding)
               (displayln "Cannot synthesize program that matches reference implementation")
               (print-sketch sk binding)))))

(define (synth-from-ref-impl sk ref-impl . inputs)
  (begin
  (let ([evaled-sk (apply (get-sketch-function sk) inputs)]
        [evaled-ref (apply ref-impl inputs)])
    (begin (define binding (time (synthesize #:forall (apply harvest inputs)
                                             #:guarantee (assert (equal? evaled-sk evaled-ref)))))
           (clear-asserts!)
           (if (unsat? binding)
               (displayln "Cannot synthesize program that matches reference implementation")
               (print-sketch sk binding))))))

#;(define (specs-synthesis sk specs inputs)
  (let ([sketch-program1 (get-sketch-function sk)])
    (begin (clear-asserts!)
           (define binding (time (synthesize #:forall '()
                                            #:guarantee (spec-assertions specs sketch-program1))))
           (if (unsat? binding)
               (displayln "Specs are unsatisfiable")
               (begin (displayln "Specs are satisfiable")
                      (let* ([bound-sketch-program1 (get-bound-sketch-function sk binding)]
                             [shuffled-sketch (sketch (get-holes-list (length (sketch-holes sk)))
                                                      (sketch-state-mask sk)
                                                      (shuffle stateless-operator-list)
                                                      (shuffle stateful-operator-list)
                                                      (sketch-input-count sk))]
                             [sketch-program2 (get-sketch-function shuffled-sketch)])
                        (begin (print-sketch sk binding)
                               (clear-asserts!)
                               (define binding2
                                 (time (synthesize #:forall '()
                                                  #:guarantee (begin (spec-assertions specs sketch-program2)
                                                                     (assert (not (equal? (apply bound-sketch-program1 (get-inputs inputs))
                                                                                          (apply sketch-program2 (get-inputs inputs)))))))))
                               (if (unsat? binding2)
                                   (displayln "No two distinct programs that satisfy specs")
                                   (begin (print-sketch shuffled-sketch binding2)
                                          (displayln "Distinguishing inputs:")
                                          (for-each (λ (i) (begin (displayln (sym-input-name i))
                                                                  (displayln (evaluate (sym-input-input i) binding2)))) inputs)
                                          (displayln (format "Program1 output: ~a"
                                                             (execute-sketch sk (evaluate inputs binding2) binding)))
                                          (displayln (format "Program2 output: ~a"
                                                             (execute-sketch shuffled-sketch (evaluate inputs binding2) binding2))))))))))))

(define (specs-synthesis sk specs inputs)
  (let ([formulas (spec-formulas specs (get-sketch-function sk))])
    (begin (clear-asserts!)
           (define binding (time (synthesize #:forall '()
                                            #:guarantee (assert formulas))))
           (if (unsat? binding)
               (displayln "Specs are unsatisfiable")
               (begin (displayln "Specs are satisfiable")
                      (let* ([bound-sketch-program1 (get-bound-sketch-function sk binding)]
                             [shuffled-sketch (sketch (get-holes-list (length (sketch-holes sk)))
                                                      (sketch-state-mask sk)
                                                      (shuffle stateless-operator-list)
                                                      (shuffle stateful-operator-list)
                                                      (sketch-input-count sk))]
                             [sketch-program2 (get-sketch-function shuffled-sketch)]
                             [formulas2 (spec-formulas specs sketch-program2)])
                        (begin (print-sketch sk binding)
                               (clear-asserts!)
                               (define binding2
                                 (time (synthesize #:forall '()
                                                  #:guarantee (begin (assert formulas2)
                                                                     (assert (not (equal? (apply bound-sketch-program1 (get-inputs inputs))
                                                                                          (apply sketch-program2 (get-inputs inputs)))))))))
                               (if (unsat? binding2)
                                   (displayln "No two distinct programs that satisfy specs")
                                   (begin (print-sketch shuffled-sketch binding2)
                                          (displayln "Distinguishing inputs:")
                                          (for-each (λ (i) (begin (displayln (sym-input-name i))
                                                                  (displayln (evaluate (sym-input-input i) binding2)))) inputs)
                                          (displayln (format "Program1 output: ~a"
                                                             (execute-sketch sk (evaluate inputs binding2) binding)))
                                          (displayln (format "Program2 output: ~a"
                                                             (execute-sketch shuffled-sketch (evaluate inputs binding2) binding2))))))))))))