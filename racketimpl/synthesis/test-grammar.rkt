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
