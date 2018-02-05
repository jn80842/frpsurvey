#lang rosette

;;(error-print-width 100000000000)

(require "fjmodel.rkt")
(require "fjapi.rkt")
(require "instruction.rkt")
(require "../benchmarks/thermostat.rkt")

(define (straightline-thermostat-graph tempB clockB)
  (define r1 tempB)
  (define r2 clockB)
  (define r3 (liftB (λ (t) (<= t 2)) r1))
  (define r4 (liftB (λ (c) (or (>= c 4) (>= 2 c))) r2))
  (define r5 (andB r3 r4))
  (define r6 (constantB 'on r1))
  (define r7 (constantB 'off r2))
  (define r8 (ifB r5 r6 r7))
  r8)

(define holes (for/list ([i (range 6)])
                (get-insn-holes)))
(define-symbolic* retval-idx integer?)

(define (sketch-graph input1 input2)
  (define r1 input1)
  (define r2 input2)
  (define r3 (call-insn (list-ref holes 0) (list r1 r2)))
  (define r4 (call-insn (list-ref holes 1) (list r1 r2 r3)))
  (define r5 (call-insn (list-ref holes 2) (list r1 r2 r3 r4)))
  (define r6 (call-insn (list-ref holes 3) (list r1 r2 r3 r4 r5)))
  (define r7 (call-insn (list-ref holes 4) (list r1 r2 r3 r4 r5 r6)))
  (define r8 (call-insn (list-ref holes 5) (list r1 r2 r3 r4 r5 r6 r7)))
  (list-ref (list r1 r2 r3 r4 r5 r6 r7 r8) retval-idx))