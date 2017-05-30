#lang rosette/safe

(require "rosettefjapi.rkt")

(define (test test-name test-code expected)
  (if (equal? test-code expected)
      (printf "~a test successful!\n" test-name)
      (printf "~a test failed! expected ~a, got ~a\n" test-name test-code expected)))

(define standard-evt-stream (λ () (list (list 1 'one) (list 2 'two) (list 3 'three))))
(define empty-evt-stream (λ () '()))

;; oneE tests
(test "oneE" ((oneE (λ () (list (list 1 'one) (list 2 'two) (list 3 'three)))))
      (list (list 1 'one)))
(test "oneE on empty stream" ((oneE (λ ()'()))) (void))

;; zeroE tests
(test "zeroE" ((zeroE standard-evt-stream))
      (list (list 1 (void)) (list 2 (void)) (list 3 (void))))
(test "zeroE on empty stream" ((zeroE empty-evt-stream)) '())

;; mapE tests




;; startsWith tests
(test "startsWith" ((startsWith standard-evt-stream 'zero))
      (list (list 0 'zero) (list 1 'one) (list 2 'two) (list 3 'three)))
(test "startsWith on empty stream" ((startsWith empty-evt-stream 'zero)) (list (list 0 'zero)))