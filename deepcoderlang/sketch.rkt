#lang rosette

(require "api.rkt")
(require "operators.rkt")
(require "specifications.rkt")

(provide (all-defined-out))

(struct sketch (holes retval-idx input-count) #:transparent)

(define (get-sketch-function sk)
  (letrec ([f (λ (calculated-streams i)
                (cond [(equal? (length (sketch-holes sk)) i) calculated-streams]
                      [else (let ([next-stream (call-dc-insn (list-ref (sketch-holes sk) i) calculated-streams)])
                              (f (append calculated-streams (list next-stream)) (add1 i)))]))])
    (λ inputs (list-ref (f inputs 0) (sketch-retval-idx sk)))))

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
                      (print-dc-insn (list-ref (evaluate (sketch-holes sk) binding) i)
                                     (list-ref varlist (+ input-count i))
                                     (take varlist (+ input-count i))))]
         [return-stmt (format "  ~a)" (list-ref varlist (evaluate (sketch-retval-idx sk) binding)))])
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
    (apply sketch-program inputs)))

(define (synth-from-ref-impl sk ref-impl . inputs)
  (let ([sketch-program (get-sketch-function sk)])
    (begin (define binding (time (synthesize #:forall (apply harvest inputs)
                                             #:guarantee (assert (apply (curry same ref-impl sketch-program) inputs)))))
           (if (unsat? binding)
               (displayln "Cannot synthesize program that matches reference implementation")
               (print-sketch sk binding)))))