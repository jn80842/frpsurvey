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
                                                                                                 