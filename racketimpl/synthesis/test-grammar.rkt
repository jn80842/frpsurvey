#lang rosette/safe

(require rosette/lib/synthax)
(require rackunit)
(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")
(require "grammar.rkt")

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

(check-true (same add1 (λ (n) (+ n 1)) 3))
(check-true (same + (λ (n m) (+ m n)) 2 3))

(check-eq? (harvest-term x) x)
(check-eq? (harvest-term b) b)
(check-eq? (harvest-term u) b)
(check-equal? (harvest-term v) (list v1 v2 v3))
(check-eq? (harvest-term 3) (void))

(check-equal? (harvest-events (list (list t1 v1) (list t2 v2) (list t3 v3))) (list t1 t2 t3 v1 v2 v3))
(check-equal? (harvest-events (list (list t1 u))) (list t1 b))
(check-equal? (harvest-behavior (behavior x (list (list t1 v1) (list t2 v2) (list t3 v3)))) (list x t1 t2 t3 v1 v2 v3))
