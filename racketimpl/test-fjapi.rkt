#lang rosette/safe

(require "rosettefjapi.rkt")

(define (test test-name test-code expected)
  (if (equal? test-code expected)
      (printf "~a test successful!\n" test-name)
      (printf "~a test failed! got ~a, expected ~a\n" test-name test-code expected)))

(define standard-evt-stream (λ () (list (list 1 11) (list 2 '12) (list 3 13))))
(define empty-evt-stream (λ () '()))
(define stream-with-no-evts (λ () (list (list 1 'no-evt) (list 2 "hello") (list 5 "world") (list 6 'no-evt))))

;; oneE tests
(test "oneE" ((oneE (λ () (list (list 1 'one) (list 2 'two) (list 3 'three)))))
      (list (list 1 'one)))
(test "oneE on empty stream" ((oneE (λ ()'()))) (void))

;; zeroE tests
(test "zeroE" ((zeroE standard-evt-stream))
      (list (list 1 (void)) (list 2 (void)) (list 3 (void))))
(test "zeroE on empty stream" ((zeroE empty-evt-stream)) '())

;; mapE tests
(test "mapE" ((mapE (λ (e) (list (first e) (add1 (second e)))) standard-evt-stream))
      (list (list 1 12) (list 2 13) (list 3 14)))
(test "mapE on empty stream" ((mapE (λ (e) (list (first e) (add1 (second e)))) empty-evt-stream)) '())
;;(test "mapE on stream with no-evts" ((mapE (λ (e) (list (first e) (

;; mergeE tests
(test "mergeE" ((mergeE (λ() (list (list 1 'one) (list 3 'three))) (λ () (list (list 2 'two)))))
      (list (list 1 'one) (list 2 'two) (list 3 'three)))
(test "mergeE two empty streams" ((mergeE (λ () '()) empty-evt-stream)) '())
(test "mergeE one stream, one empty stream" ((mergeE standard-evt-stream empty-evt-stream)) (standard-evt-stream))
(test "mergeE on stream with no events" ((mergeE stream-with-no-evts (λ () (list (list 3 'three)))))
      (list (list 1 'no-evt) (list 2 "hello") (list 3 'three) (list 5 "world") (list 6 'no-evt)))

;; constantE tests
(test "constantE" ((constantE standard-evt-stream "hello")) (list (list 1 "hello") (list 2 "hello") (list 3 "hello")))
(test "constantE on empty stream" ((constantE empty-evt-stream "hello")) '())
(test "constantE on stream with no-evts" ((constantE stream-with-no-evts 100)) (list (list 1 'no-evt) (list 2 100) (list 5 100) (list 6 'no-evt)))

;; collectE tests
(test "collectE" ((collectE standard-evt-stream 0 (λ (old new) (+ old new)))) (list (list 1 11) (list 2 23) (list 3 36)))
(test "collectE on empty stream)" ((collectE empty-evt-stream 0 (λ (old new) (+ old new)))) '())
(test "collectE on stream with no-evts" ((collectE (λ () (list (list 1 11) (list 2 'no-evt) (list 3 10))) 0 (λ (old new) (+ old new))))
      (list (list 1 11) (list 2 11) (list 3 21)))

;; startsWith tests
(test "startsWith" ((startsWith standard-evt-stream 10))
      (list (list 0 10) (list 1 11) (list 2 12) (list 3 13)))
(test "startsWith on empty stream" ((startsWith empty-evt-stream 'zero)) (list (list 0 'zero)))