#lang racket

(require "dense-fjmodels.rkt")
(require rackunit)

(check-equal? (pad-behavior (behavior #t '()) 3) (behavior #t (list #t #t #t)))
