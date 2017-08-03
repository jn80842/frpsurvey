#lang racket

(require "fjmodels.rkt")

(define (test test-name test-code expected)
  (if (equal? test-code expected)
      (printf "~a test successful!\n" test-name)
      (printf "~a test failed! got ~a, expected ~a\n" test-name test-code expected)))

(test "startBehaviorAtTimestamp" (startBehaviorAtTimestamp 3 (behavior 0 (list (list 1 1) (list 2 2)
                                                                                 (list 3 3) (list 4 4))))
  (list (list 3 3) (list 4 4)))
(test "startBehaviorAtTimestamp missing timestamp" (startBehaviorAtTimestamp 2 (behavior 0 (list '(1 1)
                                                                                                 '(5 5)
                                                                                                 '(6 6))))
      (list '(2 1) '(5 5) '(6 6)))
(test "startBehaviorAtTimestamp no changes" (startBehaviorAtTimestamp 2 (behavior 0 '())) (list (list 2 0)))

(test "behavior check" (behavior-check positive? (behavior 2 (list (list 1 2) (list 5 3) (list 10 10)))) #t)

(test "valid-time-vec-value valid value" (valid-time-vec-value? (vector 22 5 0)) #t)
(test "valid-time-vec-value valid value2" (valid-time-vec-value? (vector 0 0 1)) #t)
(test "valid-time-vec-value invalid value" (valid-time-vec-value? (vector 24 0 0)) #f)
(test "valid-time-vec-value invalid value" (valid-time-vec-value? (vector 23 6 0)) #f)
(test "valid-time-vec-value invalid value" (valid-time-vec-value? (vector 8 2 10)) #f)
(test "valid-time-vec-behavior valid behavior" (valid-time-vec-behavior?
                                                (behavior (vector 1 2 1) (list (list 1 (vector 23 3 2))))) #t)


                                                                                                 