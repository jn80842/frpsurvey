#lang rosette

(require "api.rkt")
(require "operators.rkt")
(require "typed-operators.rkt")
(require "sketch.rkt")
(require "specifications.rkt")
(require "random.rkt")

(current-bitwidth #f)
(define input-count 2)
(define insn-count 5)
(define retval-idx 4)
(define stream-length 5)
(define random-input-count 5)
(define magnitude 32)

(define (get-symbolic-inputs-by-signature int-count list-count)
  (list (for/list ([i (range int-count)])
          (get-sym-int))
        (for/list ([i (range list-count)])
          (sym-int-list stream-length))))

(define (get-concrete-inputs-by-signature int-count list-count)
  (list (for/list ([i (range int-count)])
          (random-number magnitude))
        (for/list ([i (range list-count)])
          (get-random-list stream-length magnitude))))

;; hacky alternation to handle different signatures for typed and untyped functions
(define (synth-from-ref-impl-random sk ref-impl inputs)
  (begin (clear-asserts!)
         (let ([evaled-sketch-program (apply (get-sketch-function sk) (append (first inputs) (second inputs)))]
               [evaled-ref-program (apply ref-impl inputs)])
           (begin
             (define binding (time (synthesize #:forall (apply harvest inputs)
                                               #:guarantee (assert (equal? evaled-sketch-program evaled-ref-program)))))
             (clear-asserts!)
             (if (unsat? binding)
                 (displayln "Cannot synthesize program that matches reference implementation")
                 (print-sketch sk binding))))))

(define (synth-from-io-pairs-random sk inputs outputs ref-impl sym-inputs)
  (begin (clear-asserts!)
         (let* ([sk-function (get-sketch-function sk)]
                [phi (andmap (λ (i o) (equal? (apply sk-function (append (first i) (second i))) o)) inputs outputs)])
           (define binding (time (synthesize #:forall '()
                                             #:guarantee (assert phi))))
           (clear-asserts!)
           (if (unsat? binding)
               (displayln "Cannot synthesize program that matches input/output pair")
               (begin (print-sketch sk binding)
                      (verify-sketch-random ref-impl sk binding sym-inputs))))))

(define (verify-sketch-random ref-impl sk binding inputs)
  (let ([sk-phi (apply (get-bound-sketch-function sk binding) (append (first inputs) (second inputs)))]
        [ref-phi (apply ref-impl inputs)])
    (begin
      (define m (verify (assert (equal? sk-phi ref-phi))))
           (if (unsat? m)
               (displayln "Synthesized function is equivalent to reference implementation")
               (displayln "Synthesized function is NOT equivalent to reference implementation")))))

(define (benchmark-program int-count list-count)
  (let ([program-insns (get-random-program insn-count int-count list-count)])
    (begin (print-from-random-program program-insns int-count list-count)
           (let* ([program-function (get-random-program-function program-insns)]
                  [program-sketch (sketch (get-holes-list insn-count) (get-sym-int) (+ int-count list-count))]
                  [sym-inputs (get-symbolic-inputs-by-signature int-count list-count)]
                  [random-inputs (for/list ([i (range random-input-count)])
                                   (get-concrete-inputs-by-signature int-count list-count))]
                  [random-outputs (for/list ([i (range (length random-inputs))])
                                    (apply program-function (list-ref random-inputs i)))])
             (begin
               (displayln "Synthesize function against concrete program")
               (synth-from-ref-impl-random program-sketch program-function sym-inputs)
               (displayln "Synthesize function from input-output pairs")
               (synth-from-io-pairs-random program-sketch random-inputs random-outputs program-function sym-inputs)
               )))))

(define list-inputs (random 1 input-count))
(define int-inputs (- input-count list-inputs))
(benchmark-program int-inputs list-inputs)