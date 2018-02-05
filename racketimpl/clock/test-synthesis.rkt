#lang rosette

(require "fjmodel.rkt")
(require "fjapi.rkt")
(require "instruction.rkt")

(define stream-length 3)

(define holes (list (get-insn-holes)))
(define-symbolic* retval-idx integer?)

(define int-stream (new-event-stream sym-integer stream-length))

(define (sketch-graph1-1 e)
  (define r1 e)
  (define r2 (call-insn (list-ref holes 0) (list r1)))
  (list-ref (list r1 r2) retval-idx))

;; constantE-imm

(define (constantE-imm-graph e)
  (define r1 e)
  (define r2 (constantE 1 e))
  r2)

(define b (synthesize #:forall (harvest int-stream)
                      #:guarantee (assert (equal? (map constantE-imm-graph int-stream)
                                                  (map sketch-graph1-1 int-stream)))))
(if (unsat? b)
    (displayln "!!!!! constantE-imm graph not synthesized !!!!!")
    (begin (displayln "* constantE-imm graph successfully synthesized")
           (print-from-holes (evaluate holes b)
                             (evaluate retval-idx b) 1)))

;; constantE

(define (constantE-graph e)
  (define r1 e)
  (define r2 (constantE 'test e))
  r2)

(define b-constant (synthesize #:forall (harvest int-stream)
                               #:guarantee (assert (equal? (map constantE-graph int-stream)
                                                          (map sketch-graph1-1 int-stream)))))
(if (unsat? b-constant)
    (displayln "!!!!! constantE graph not synthesized !!!!!")
    (begin (displayln "* constantE graph successfully synthesized")
           (print-from-holes (evaluate holes b-constant)
                             (evaluate retval-idx b-constant) 1)))
