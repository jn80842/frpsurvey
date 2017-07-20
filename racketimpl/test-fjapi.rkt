#lang rosette/safe

(require "rosettefjapi.rkt")
(require "fjmodels.rkt")

(define (test test-name test-code expected)
  (if (equal? test-code expected)
      (printf "~a test successful!\n" test-name)
      (printf "~a test failed! got ~a, expected ~a\n" test-name test-code expected)))

(define standard-evt-stream (list (list 1 11) (list 2 '12) (list 3 13)))
(define empty-evt-stream '())
(define stream-with-no-evts (list (list 1 'no-evt) (list 2 "hello") (list 5 "world") (list 6 'no-evt)))

;; oneE tests
(test "oneE" (oneE (list (list 1 'one) (list 2 'two) (list 3 'three)))
      (list (list 1 'one)))
(test "oneE on empty stream" (oneE '()) (void))

;; zeroE tests
(test "zeroE" (zeroE) '())

;; mapE tests
(test "mapE" (mapE (λ (e) (list (first e) (add1 (second e)))) standard-evt-stream)
      (list (list 1 12) (list 2 13) (list 3 14)))
(test "mapE on empty stream" (mapE (λ (e) (list (first e) (add1 (second e)))) empty-evt-stream) '())
;;(test "mapE on stream with no-evts" ((mapE (λ (e) (list (first e) (

;; mergeE tests
(test "mergeE" (mergeE (list (list 1 'one) (list 3 'three)) (list (list 2 'two)))
      (list (list 1 'one) (list 2 'two) (list 3 'three)))
(test "mergeE two empty streams" (mergeE '() empty-evt-stream) '())
(test "mergeE one stream, one empty stream" (mergeE standard-evt-stream empty-evt-stream) standard-evt-stream)
(test "mergeE on stream with no events" (mergeE stream-with-no-evts (list (list 3 'three)))
      (list (list 1 'no-evt) (list 2 "hello") (list 3 'three) (list 5 "world") (list 6 'no-evt)))

;; switchE tests
(test "switchE on empty event stream" (switchE '()) '())
(test "switchE on single event stream" (switchE (list (list 1 (list (list 1 'a) (list 2 'b)))))
      (list (list 1 'a) (list 2 'b)))
(test "switchE on stream of 2 event streams" (switchE (list
                          (list 1 (list (list 2 'a) (list 5 'b) (list 10 'c)))
                          (list 6 (list (list 7 'd) (list 8 'e)))))
      (list (list 2 'a) (list 5 'b) (list 7 'd) (list 8 'e)))
(test "switchE on stream of 4 event streams" (switchE (list
                                                       (list 1 (list (list 2 'a) (list 5 'b) (list 10 'c)))
                                                       (list 6 (list (list 6 'd) (list 7 'e) (list 20 'f)))
                                                       (list 8 (list (list 9 'g)))
                                                       (list 10 (list (list 11 'h) (list 12 'i)))))
      (list '(2 a) '(5 b) '(6 d) '(7 e) '(9 g) '(11 h) '(12 i)))
(test "switchE on stream including empty streams" (switchE (list
                                                           (list 1 (list (list 2 'a) (list 10 'b)))
                                                           (list 5 '())
                                                           (list 10 (list (list 11 'c) (list 15 'd)))))
      (list '(2 a) '(11 c) '(15 d)))

;; filterE tests
(test "filterE" (filterE (list (list 1 #t) (list 2 #f) (list 3 #t) (list 4 #f)) not)
      (list (list 2 #f) (list 4 #f)))

;; constantE tests
(test "constantE" (constantE standard-evt-stream "hello") (list (list 1 "hello") (list 2 "hello") (list 3 "hello")))
(test "constantE on empty stream" (constantE empty-evt-stream "hello") '())
(test "constantE on stream with no-evts" (constantE stream-with-no-evts 100) (list (list 1 'no-evt) (list 2 100) (list 5 100) (list 6 'no-evt)))

;; collectE tests
(test "collectE" (collectE standard-evt-stream 0 (λ (old new) (+ old new))) (list (list 1 11) (list 2 23) (list 3 36)))
(test "collectE on empty stream)" (collectE empty-evt-stream 0 (λ (old new) (+ old new))) '())
(test "collectE on stream with no-evts" (collectE (list (list 1 11) (list 2 'no-evt) (list 3 10)) 0 (λ (old new) (+ old new)))
      (list (list 1 11) (list 2 11) (list 3 21)))

;; snapshotE tests
(test "snapshotE" (snapshotE standard-evt-stream (behavior 1 '())) (list (list 1 1) (list 2 1) (list 3 1)))
(test "snapshotE with behavior changes" (snapshotE  (list (list 1 #t) (list 4 #t) (list 9 #t))
                                                    (behavior 0 (list (list 1 1) (list 2 2) (list 8 8) (list 10 10))))
      (list (list 1 1) (list 4 2) (list 9 8)))
(test "snapshotE with no-events" (snapshotE stream-with-no-evts (behavior 1 '())) (list (list 2 1) (list 5 1)))

;; startsWith tests
(test "startsWith" (startsWith standard-evt-stream 10) (behavior 10 (list (list 1 11) (list 2 '12) (list 3 13))))
     ; (list (list 0 10) (list 1 11) (list 2 12) (list 3 13)))
(test "startsWith on empty stream" (startsWith empty-evt-stream 'zero) (behavior 'zero '())) ;;(list (list 0 'zero)))

;; changes tests
(test "changes" (changes (behavior 0 (list (list 1 1) (list 2 2) (list 3 3)))) (list (list 1 1) (list 2 2) (list 3 3)))
(test "changes with no events" (changes (behavior 0 '())) '())

;; behavior
(test "behavior-init" (behavior-init (behavior 'a '())) 'a)
(test "behavior-changes" (behavior-changes (behavior 'a '())) '())

;; project-values
(test "project-values" (project-values (behavior 'a (list (list 1 'a) (list 3 'b) (list 6 'c))) (list 1 2 4 7))
      (list (list 1 'a) (list 2 'a) (list 4 'b) (list 7 'c)))

;; valueNow
(test "valueNow matching timestamp" (valueNow (behavior 3 (list (list 1 'a))) 1) 'a)
(test "valueNow non-matching timestamp" (valueNow (behavior 3 (list (list 1 'a) (list 5 'b))) 3) 'a)
(test "valueNow: behavior has no changes" (valueNow (behavior 3 '()) 10) 3)
(test "valueNow: timestamp before any change" (valueNow (behavior 3 (list (list 10 5))) 6) 3)

;; constantB
(test "constantB" (constantB 'a) (behavior 'a '()))

;; andB
(test "andB" (andB (behavior #t (list (list 1 #t) (list 5 #f) (list 7 #t)))
                   (behavior #t (list (list 1 #t) (list 3 #t) (list 6 #t) (list 7 #f))))
      (behavior #t (list (list 1 #t) (list 3 #t) (list 5 #f) (list 6 #f) (list 7 #f))))

;; notB
(test "notB" (notB (behavior #t (list (list 1 #f) (list 2 #t) (list 3 #f))))
                   (behavior #f (list (list 1 #t) (list 2 #f) (list 3 #t))))

;; liftB
(test "liftB" (liftB (λ (v) (+ v 3)) (behavior 0 (list (list 1 2) (list 2 3) (list 5 4))))
      (behavior 3 (list (list 1 5) (list 2 6) (list 5 7))))
(test "liftB with var args" (liftB + (behavior 0 (list (list 1 1) (list 2 2) (list 3 3)))
                                   (behavior 10 (list (list 1 1) (list 2 2) (list 3 3))))
      (behavior 10 (list (list 1 2) (list 2 4) (list 3 6))))

;; condB
(test "condB" (condB (list (list (behavior #t (list (list 1 #f))) (behavior 3 (list (list 1 3)))) (list (behavior #f (list (list 1 #t))) (behavior 5 (list (list 1 5))))))
      (behavior 3 (list (list 1 5))))
(test "condB returning #f" (condB (list
                                   (list (behavior #t (list (list 1 #f))) (behavior #f (list (list 1 'a))))
                                   (list (behavior #t (list (list 1 #t))) (behavior 'wrong (list (list 1 'b))))))
      (behavior #f (list (list 1 'b))))

;; ifB
(test "ifB matching timestamps" (ifB (behavior #t (list (list 1 #t) (list 3 #f) (list 5 #f) (list 7 #t)))
                                     (behavior 't (list (list 1 't) (list 3 't) (list 5 't) (list 7 't)))
                                     (behavior 'f (list (list 1 'f) (list 3 'f) (list 5 'f) (list 7 'f))))
      (behavior 't (list (list 1 't) (list 3 'f) (list 5 'f) (list 7 't))))

(test "ifB non matching timestamps" (ifB (behavior #t (list (list 1 #f) (list 5 #t) (list 9 #f)))
                                         (behavior 't (list (list 1 '1) (list 3 '3) (list 5 '5) (list 11 '11)))
                                         (behavior 'f (list (list 1 'no) (list 2 '2) (list 5 'no) (list 6 '6) (list 8 '8))))
      (behavior 't (list (list 1 'no) (list 2 '2) (list 3 '2) (list 5 '5) (list 6 '5) (list 8 '5) (list 9 '8) (list 11 '8))))