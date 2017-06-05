#lang rosette/safe

(require "rosettefjapi.rkt")

(define (test test-name test-code expected)
  (if (equal? test-code expected)
      (printf "~a test successful!\n" test-name)
      (printf "~a test failed! expected ~a, got ~a\n" test-name test-code expected)))

(define standard-evt-stream (λ () (list (list 1 11) (list 2 '12) (list 3 13))))
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
(test "mapE" ((mapE (λ (e) (list (first e) (add1 (second e)))) standard-evt-stream)) (list (list 1 12) (list 2 13) (list 3 14)))
(test "mapE on empty stream" ((mapE (λ (e) (list (first e) (add1 (second e)))) empty-evt-stream)) '())

;; collectE tests
(test "collectE" ((collectE standard-evt-stream 0 (λ (old new) (+ old new)))) (list (list 1 11) (list 2 23) (list 3 36)))
(test "collectE on empty stream)" ((collectE empty-evt-stream 0 (λ (old new) (+ old new)))) '())

;; startsWith tests
(test "startsWith" ((startsWith standard-evt-stream 10))
      (list (list 0 10) (list 1 11) (list 2 12) (list 3 13)))
(test "startsWith on empty stream" ((startsWith empty-evt-stream 'zero)) (list (list 0 'zero)))