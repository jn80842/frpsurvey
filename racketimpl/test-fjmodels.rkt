#lang racket

(require rackunit)
(require "fjmodels.rkt")

(check-equal? (startBehaviorAtTimestamp 3 (behavior 0 (list (list 1 1) (list 2 2)
                                                                                 (list 3 3) (list 4 4))))
  (list (list 3 3) (list 4 4)))
(check-equal? (startBehaviorAtTimestamp 2 (behavior 0 (list '(1 1)
                                                                                                 '(5 5)
                                                                                                 '(6 6))))
      (list '(2 1) '(5 5) '(6 6)))
(check-equal? (startBehaviorAtTimestamp 2 (behavior 0 '())) (list (list 2 0)))

(check-equal? (behavior-check positive? (behavior 2 (list (list 1 2) (list 5 3) (list 10 10)))) #t)
(check-equal? (behavior-check (λ (n t) (and (integer? n) (symbol? t)))
                                                   (behavior 1 (list (list 1 2) (list 2 3)))
                                                   (behavior 'a (list (list 1 'b) (list 2 'c)))) #t)

(check-equal? (valid-time-vec-value? (vector 22 5 0)) #t)
(check-equal? (valid-time-vec-value? (vector 0 0 1)) #t)
(check-equal? (valid-time-vec-value? (vector 24 0 0)) #f)
(check-equal? (valid-time-vec-value? (vector 23 6 0)) #f)
(check-equal? (valid-time-vec-value? (vector 8 2 10)) #f)
(check-equal? (valid-time-vec-behavior?
                                                (behavior (vector 1 2 1) (list (list 1 (vector 23 3 2))))) #t)

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
