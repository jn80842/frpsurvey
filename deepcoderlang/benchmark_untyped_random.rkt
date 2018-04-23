#lang rosette

(require "api.rkt")
(require "operators.rkt")
(require "typed-operators.rkt")
(require "sketch.rkt")
(require "specifications.rkt")
(require "random.rkt")

(define (get-random-insn stream-count)
  (dc-insn (random (length operator-list))
           (random stream-count)
           (random stream-count)
           (random (length int-to-int-funcs))
           (random (length int-to-bool-funcs))
           (random (length int-to-int-to-int-funcs))))

(define (get-random-program-insn input-count insn-count)
  (for/list ([i (range insn-count)])
    (get-random-insn (+ input-count i))))

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

(define r-program (get-random-program-insn input-count insn-count))

(define (valid-program? prog . inputs)
  (with-handlers ([exn:fail? (λ (exn) #f)])
    (begin
      (apply prog inputs)
      #t)))

(define (get-program-signature program-insn)
  (let ([program-function (get-concrete-program-function program-insn retval-idx)])
    (cond [(valid-program? program-function '(1 2 3) '(1 2 3)) 'list-list]
          [(valid-program? program-function '(1 2 3) 3) 'list-int]
          [(valid-program? program-function 3 '(1 2 3)) 'int-list]
          [(valid-program? program-function 3 3) 'int-int]
          [else #f])))

(define (get-symbolic-inputs-by-signature sig)
  (cond [(equal? sig 'list-list) (list list-input1 list-input2)]
        [(equal? sig 'list-int) (list list-input1 int-input1)]
        [(equal? sig 'int-list) (list int-input1 list-input1)]
        [(equal? sig 'int-int) (list int-input1 int-input2)]
        [else 'fail]))

(define (get-concrete-inputs-by-signature sig)
  (cond [(equal? sig 'list-list) (for/list ([i (range random-input-count)])
                                   (list (get-random-list stream-length)
                                         (get-random-list stream-length)))]
        [(equal? sig 'list-int) (for/list ([i (range random-input-count)])
                                 (list (get-random-list stream-length) (random-number 32)))]
        [(equal? sig 'int-list) (for/list ([i (range random-input-count)])
                                  (list (random-number 32) (get-random-list stream-length)))]
        [(equal? sig 'int-int) (for/list ([i (range random-input-count)])
                                 (list (random-number 32) (random-number 32)))]
        [else 'fail]))

(define (benchmark-program)
  (let* ([program-insn (get-random-program-insn input-count insn-count)]
         [signature (get-program-signature program-insn)])
    (begin (print-from-concrete-program input-count program-insn retval-idx)
      (displayln signature)
      (if signature
        (let* ([program-function (get-concrete-program-function program-insn retval-idx)]
               [program-sketch (sketch (get-holes-list insn-count) (get-sym-int) input-count)]
               [sym-inputs (get-symbolic-inputs-by-signature signature)]
               [random-inputs (get-concrete-inputs-by-signature signature)]
               [random-outputs (for/list ([i (range (length random-inputs))])
                                 (apply program-function (list-ref random-inputs i)))])
          (begin 
                 (displayln "Synthesize function against concrete program")
                 (apply (curry synth-from-ref-impl program-sketch program-function) sym-inputs)
                 (displayln "Synthesize function from input/output pairs")
                 (apply (curry synth-from-io-pairs program-sketch random-inputs random-outputs program-function) sym-inputs)))
        (displayln "invalid program")))))

           
    

