#lang rosette

(require racket/engine)

(require "api.rkt")
(require "sketch.rkt")
(require "specifications.rkt")
(require "random.rkt")

(current-bitwidth #f)
(define input-count 2)
(define insn-count 5)
(define retval-idx 6)
(define stream-length 5)
(define random-input-count 5)
(define magnitude 32)

(define (get-symbolic-inputs-by-signature int-count list-count)
  (list (for/list ([i (range int-count)])
          (get-sym-int))
        (for/list ([i (range list-count)])
          (sym-int-list stream-length))))

(define (get-concrete-inputs-by-signature int-count list-count input-length)
  (list (for/list ([i (range int-count)])
          (random-positive-number input-length))
        (for/list ([i (range list-count)])
          (get-random-list stream-length magnitude))))

;; inputs is a list of two lists (list of ints, list of lists of ints)
;; sketch program takes all inputs as separate arguments
;; generated program takes a list of ints and a list of list of ints as arguments
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
               (displayln "SAME - Synthesized function is equivalent to reference implementation")
               (displayln "DIFF - Synthesized function is NOT equivalent to reference implementation")))))

;; note: we use the typed operators to generate a random program (to make sure it's valid and well typed)
;; but synthesize a program sketch using untyped operator
;; (for no real good reason other than to avoid rewriting sketch code)
(define (benchmark-program int-count list-count)
  (let ([program-insns (get-random-program insn-count int-count list-count)])
    (begin (print-from-random-program program-insns int-count list-count)
           (let* ([program-function (get-random-program-function program-insns)]
                  [program-sketch (get-sketch insn-count (+ int-count list-count))]
                  [sym-inputs (get-symbolic-inputs-by-signature int-count list-count)]
                  [random-inputs (for/list ([i (range random-input-count)])
                                   (let ([i (get-concrete-inputs-by-signature int-count list-count stream-length)])
                                     (begin (displayln i)
                                            i)))]
                  ;; some inputs will cause programs to fail; if so this will throw an exception and quit
                  [random-outputs (for/list ([i (range (length random-inputs))])
                                    (apply program-function (list-ref random-inputs i)))])
             (begin
              ; (displayln "Synthesize function against concrete program")
              ; (synth-from-ref-impl-random program-sketch program-function sym-inputs)
               (displayln "Synthesize function from input-output pairs")
               (synth-from-io-pairs-random program-sketch random-inputs random-outputs program-function sym-inputs)
               )))))

(define list-inputs (random 1 input-count))
(define int-inputs (- input-count list-inputs))
(define e (engine
           (λ (_) (benchmark-program list-inputs int-inputs))))
(engine-run 3600000 e)