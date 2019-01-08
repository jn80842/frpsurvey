#lang rosette

(require "operators.rkt")

(provide (all-defined-out))

(struct sketch (holes retval-idx input-count) #:transparent)

(define (get-symbolic-sketch insn-length input-count)
  (sketch (get-holes-list insn-length) (get-retval-idx) input-count))

(define (operator-lookup sk insn-idx [binding #f])
  (let* ([insn (if binding
                   (evaluate (list-ref (sketch-holes sk) insn-idx) binding)
                   (list-ref (sketch-holes sk) insn-idx))]
         [op-id (stream-insn-op-index insn)])
    (list-ref operator-list op-id)))

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

(define (get-sketch-function sk)
  (letrec ([f (λ (calculated-streams i)
                (cond [(equal? (length (sketch-holes sk)) i) calculated-streams]
                      [else (let ([next-stream (call-stream-insn (operator-lookup sk i)
                                                                 (list-ref (sketch-holes sk) i)
                                                                 calculated-streams)])
                              (f (append calculated-streams (list next-stream)) (add1 i)))]))])
    (λ inputs (list-ref (f inputs 0) (sketch-retval-idx sk)))))

(define (get-bound-sketch-function sk binding)
  (letrec ([f (λ (calculated-streams i)
                (cond [(equal? (length (sketch-holes sk)) i) calculated-streams]
                      [else (let ([next-stream (call-stream-insn (operator-lookup sk i binding)
                                                                 (list-ref (evaluate (sketch-holes sk) binding) i)
                                                                 calculated-streams)])
                              (f (append calculated-streams (list next-stream)) (add1 i)))]))])
    (λ inputs (list-ref (f inputs 0) (evaluate (sketch-retval-idx sk) binding)))))

;; can add this back in by adding operator list as a field on the sketch struct
#;(define (shuffle-sketch sk)
  (sketch (get-holes-list (length (sketch-holes sk)))
          (get-retval-idx)
          (shuffle stateless-operator-list)
          (shuffle stateful-operator-list)
          (sketch-input-count sk)))
