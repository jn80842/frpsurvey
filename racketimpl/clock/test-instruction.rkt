#lang rosette

(require rackunit)
(require "fjmodel.rkt")
(require "fjapi.rkt")
(require "instruction.rkt")
(require rackunit)

(define constant-insn (insn #t 0 0 0 0 1))
(check-equal? (call-insn constant-insn (list '(a b c))) '(1 1 1))
