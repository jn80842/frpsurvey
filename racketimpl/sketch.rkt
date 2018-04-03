#lang rosette

(require "dense-fjmodels.rkt")
(require "straightline.rkt")
(require "specifications.rkt")

(provide (all-defined-out))

(struct sketch (holes state-mask retval-idx
                      stateless-op-list stateful-op-list input-count) #:transparent)

(define (full-op-list sk)
  (append (sketch-stateless-op-list sk) (sketch-stateful-op-list sk)))

(define (operator-lookup-sketch sk insn-idx)
  (let* ([insn (list-ref (sketch-holes sk) insn-idx)]
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
         [varlist (for/list ([i (range (add1 (+ input-count depth)))])
                    (format "r~a" (add1 i)))]
         [stmt-list (for/list ([i (range depth)])
                      (print-stream-insn (vector-ref (sketch-state-mask sk) i)
                                         (list-ref (evaluate (sketch-holes sk) binding) i)
                                         (list-ref varlist (+ input-count i))
                                         (take varlist (+ input-count i))))]
         [return-stmt (format "  ~a" (list-ref varlist (evaluate (sketch-retval-idx sk) binding)))])
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
                            (evaluate (get-symbolic-sketch sk) binding)
                            (get-symbolic-sketch sk))])
    (apply (get-symbolic-sketch sk) (get-inputs inputs))))

(define (get-symbolic-sketch sk)
  (letrec ([f (λ (calculated-streams i)
                (cond [(equal? (length (sketch-holes sk)) i) calculated-streams]
                      [else (let ([next-stream ((operator-call (operator-lookup-sketch sk i))
                                                (list-ref (sketch-holes sk) i)
                                                calculated-streams)])
                                      (f (append calculated-streams (list next-stream)) (add1 i)))]))])
    (λ inputs (list-ref (f inputs 0) (sketch-retval-idx sk)))))

(define (synth-from-ref-impl sk ref-impl . inputs)
  (let ([sketch-program (get-symbolic-sketch sk)])
    (begin (define binding (time (synthesize #:forall (apply harvest inputs)
                                             #:guarantee (assert (apply (curry same ref-impl sketch-program) inputs)))))
           (if (unsat? binding)
               (displayln "Cannot synthesize program that matches reference implementation")
               (print-sketch sk binding)))))

(define (specs-synthesis sk specs inputs)
  (let ([sketch-program1 (get-symbolic-sketch sk)])
    (begin (clear-asserts!)
           (define binding (time (synthesize #:forall '()
                                            #:guarantee (spec-assertions specs sketch-program1))))
           (if (unsat? binding)
               (displayln "Specs are unsatisfiable")
               (begin (displayln "Specs are satisfiable")
                      (let* ([shuffled-sketch (sketch (get-holes-list (length (sketch-holes sk)))
                                                      (sketch-state-mask sk)
                                                      (get-retval-idx)
                                                      (shuffle stateless-operator-list)
                                                      (shuffle stateful-operator-list)
                                                      (sketch-input-count sk))]
                             [sketch-program2 (get-symbolic-sketch shuffled-sketch)])
                        (begin (print-sketch sk binding)
                               (clear-asserts!)
                               (define binding2
                                 (time (synthesize #:forall '()
                                                  #:guarantee (begin (spec-assertions specs sketch-program1)
                                                                     (spec-assertions specs sketch-program2)
                                                                     (assert (not (equal? (apply sketch-program1 (get-inputs inputs))
                                                                                          (apply sketch-program2 (get-inputs inputs)))))))))
                               (if (unsat? binding2)
                                   (displayln "No two distinct programs that satisfy specs")
                                   (begin (print-sketch shuffled-sketch binding2)
                                          (for-each (λ (i) (begin (displayln (sym-input-name i))
                                                                  (displayln (evaluate (sym-input-input i) binding2)))) inputs)
                                          (displayln (format "Program1 output: ~a"
                                                             (apply (evaluate sketch-program1 binding)
                                                                    (get-inputs inputs))))
                                          (displayln (format "Program2 output: ~a"
                                                             (apply (evaluate sketch-program2 binding2)
                                                                    (get-inputs inputs)))))))))))))