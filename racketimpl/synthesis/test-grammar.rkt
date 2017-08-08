#lang rosette/safe

(require rosette/lib/synthax)
(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")
(require "grammar.rkt")

(define (test test-name test-code expected)
  (if (equal? test-code expected)
      (printf "~a test successful!\n" test-name)
      (printf "~a test failed! got ~a, expected ~a\n" test-name test-code expected)))

(define (test-assert test-name test-code)
  (begin (printf "testing ~a..." test-name)
         (clear-asserts!)
         (printf "ok!~n")))

(define-symbolic* x integer?)
(define-symbolic* b boolean?)
(define u (if b 'a 'b))
(define-symbolic* v1 integer?)
(define-symbolic* v2 integer?)
(define-symbolic* v3 integer?)
(define v (vector v1 v2 v3))
(define-symbolic* t1 integer?)
(define-symbolic* t2 integer?)
(define-symbolic* t3 integer?)

(test-assert "same 1 arg" (same add1 (λ (n) (+ n 1)) 3))
(test-assert "same 2 args" (same + (λ (n m) (+ m n)) 2 3))

(test "harvest-term integer" (harvest-term x) x)
(test "harvest-term boolean" (harvest-term b) b)
(test "harvest-term union" (harvest-term u) b)
(test "harvest-term vector" (harvest-term v) (list v1 v2 v3))
(test "harvest-term not a term" (harvest-term 3) (void))

(test "harvest-events integers" (harvest-events (list (list t1 v1) (list t2 v2) (list t3 v3))) (list t1 t2 t3 v1 v2 v3))
(test "harvest-events unions" (harvest-events (list (list t1 u))) (list t1 b))

(test "harvest-behavior integers" (harvest-behavior (behavior x (list (list t1 v1) (list t2 v2) (list t3 v3)))) (list x t1 t2 t3 v1 v2 v3))

