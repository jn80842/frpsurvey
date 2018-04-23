#lang rosette

(require "api.rkt")
(require "operators.rkt")
(require "typed-operators.rkt")
(require "sketch.rkt")
(require "specifications.rkt")
(require "random.rkt")

(current-bitwidth #f)
(define input-count 2)
(define insn-count 3)
(define retval-idx 4)
(define stream-length 3)
(define random-input-count 4)

(define list-input1 (sym-int-list stream-length))
(define list-input2 (sym-int-list stream-length))
(define int-input1 (get-sym-int))
(define int-input2 (get-sym-int))

(define (get-symbolic-inputs-by-signature int-count list-count)
  3)

(define (get-concrete-inputs-by-signature int-count list-count)
  3)

(define (sketch-program-wrapper f)
  (λ (i l) (apply f (apply append (list i l)))))

(define (benchmark-program int-count list-count)
  (let ([program-insns (get-random-program insn-count int-count list-count)])
    (begin (print-from-random-program program-insns int-count list-count)
           (let* ([program-function (get-random-program-function program-insns)]
                  [program-sketch (sketch (get-holes-list insn-count) (get-sym-int) (+ int-count list-count))]
                  [sym-inputs (get-symbolic-inputs-by-signature int-count list-count)]
                  [random-inputs (get-concrete-inputs-by-signature int-count list-count)]
                  [random-outputs (for/list ([i (range (length random-inputs))])
                                    (apply program-function (list-ref random-inputs i)))])
             (begin
               (displayln "Synthesize function against concrete program")
               (apply (curry synth-from-ref-impl (sketch-program-wrapper program-sketch) program-function) sym-inputs)
               (displayln "Synthesize function from input-output pairs")
               (apply (curry synth-from-io-pairs (sketch-program-wrapper program-sketch) random-inputs random-outputs program-function) sym-inputs))))))

           
    

