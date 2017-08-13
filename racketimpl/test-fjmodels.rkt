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


                                                                                                 