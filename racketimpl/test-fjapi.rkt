#lang rosette/safe
(require rackunit)
(require "rosettefjapi.rkt")
(require "fjmodels.rkt")

(define standard-evt-stream (list (list 1 11) (list 2 '12) (list 3 13)))
(define empty-evt-stream '())
(define stream-with-no-evts (list (list 1 'no-evt) (list 2 "hello") (list 5 "world") (list 6 'no-evt)))

;; oneE tests
(check-equal? (oneE (list (list 1 'one) (list 2 'two) (list 3 'three)))
      (list (list 1 'one)))
(check-equal? (oneE '()) (void))

;; zeroE tests
(check-equal? (zeroE) '())

;; mapE tests
(check-equal? (mapE (λ (e) (list (first e) (add1 (second e)))) standard-evt-stream)
      (list (list 1 12) (list 2 13) (list 3 14)))
(check-equal? (mapE (λ (e) (list (first e) (add1 (second e)))) empty-evt-stream) '())

;; mergeE tests
(check-equal? (mergeE (list (list 1 'one) (list 3 'three)) (list (list 2 'two)))
      (list (list 1 'one) (list 2 'two) (list 3 'three)))
(check-equal? (mergeE '() empty-evt-stream) '())
(check-equal? (mergeE standard-evt-stream empty-evt-stream) standard-evt-stream)
(check-equal? (mergeE stream-with-no-evts (list (list 3 'three)))
      (list (list 1 'no-evt) (list 2 "hello") (list 3 'three) (list 5 "world") (list 6 'no-evt)))

;; switchE tests
(check-equal? (switchE '()) '())
(check-equal? (switchE (list (list 1 (list (list 1 'a) (list 2 'b)))))
      (list (list 1 'a) (list 2 'b)))
(check-equal? (switchE (list
                          (list 1 (list (list 2 'a) (list 5 'b) (list 10 'c)))
                          (list 6 (list (list 7 'd) (list 8 'e)))))
      (list (list 2 'a) (list 5 'b) (list 7 'd) (list 8 'e)))
(check-equal? (switchE (list
                                                       (list 1 (list (list 2 'a) (list 5 'b) (list 10 'c)))
                                                       (list 6 (list (list 6 'd) (list 7 'e) (list 20 'f)))
                                                       (list 8 (list (list 9 'g)))
                                                       (list 10 (list (list 11 'h) (list 12 'i)))))
      (list '(2 a) '(5 b) '(6 d) '(7 e) '(9 g) '(11 h) '(12 i)))
(check-equal? (switchE (list
                                                           (list 1 (list (list 2 'a) (list 10 'b)))
                                                           (list 5 '())
                                                           (list 10 (list (list 11 'c) (list 15 'd)))))
      (list '(2 a) '(11 c) '(15 d)))

;; filterE tests
(check-equal? (filterE not (list (list 1 #t) (list 2 #f) (list 3 #t) (list 4 #f)))
      (list (list 2 #f) (list 4 #f)))

;; ifE tests
(check-equal? (ifE (list (list 1 #t) (list 3 #f) (list 7 #t))
                 (list (list 1 'a) (list 2 'b) (list 3 'c) (list 10 'd))
                 (list (list 2 'x) (list 3 'y) (list 11 'z)))
      (list (list 1 'a) (list 3 'y)))

;; constantE tests
(check-equal? (constantE "hello" standard-evt-stream) (list (list 1 "hello") (list 2 "hello") (list 3 "hello")))
(check-equal? (constantE "hello" empty-evt-stream) '())
(check-equal? (constantE 100 stream-with-no-evts) (list (list 1 'no-evt) (list 2 100) (list 5 100) (list 6 'no-evt)))

;; collectE tests
(check-equal? (collectE 0 (λ (old new) (+ old new)) standard-evt-stream) (list (list 1 11) (list 2 23) (list 3 36)))
(check-equal? (collectE 0 (λ (old new) (+ old new)) empty-evt-stream) '())
(check-equal? (collectE 0 (λ (old new) (+ old new)) (list (list 1 11) (list 2 'no-evt) (list 3 10)))
      (list (list 1 11) (list 2 11) (list 3 21)))

;; notE tests
(check-equal? (notE (list (list 1 #f) (list 3 #t) (list 10 #f)))
      (list (list 1 #t) (list 3 #f) (list 10 #t)))

;; snapshotE tests
(check-equal? (snapshotE standard-evt-stream (behavior 1 '())) (list (list 1 1) (list 2 1) (list 3 1)))
(check-equal? (snapshotE  (list (list 1 #t) (list 4 #t) (list 9 #t))
                                                    (behavior 0 (list (list 1 1) (list 2 2) (list 8 8) (list 10 10))))
      (list (list 1 1) (list 4 2) (list 9 8)))
(check-equal? (snapshotE stream-with-no-evts (behavior 1 '())) (list (list 2 1) (list 5 1)))

;; delayE tests
(check-equal? (delayE 3 standard-evt-stream) '((4 11) (5 12) (6 13)))

;; calmE tests
(check-equal? (calmE 3 (list (list 1 'a) (list 5 'b) (list 6 'c) (list 7 'd)))
      '((4 a) (10 d)))
(check-equal? (calmE 3 '()) '())
(check-equal? (calmE 3 (list (list 3 'a))) '((6 a)))
(check-equal? (calmE 3 (list (list 1 'a) (list 2 'b) (list 3 'c)
                                                (list 4 'd) (list 5 'e) (list 6 'f)))
      '((9 f)))

;; blindE tests
(check-equal? (blindE 3 (list (list 1 'a) (list 2 'b) (list 3 'c)
                             (list 7 'd) (list 12 'e) (list 13 'f)))
      (list (list 1 'a) (list 7 'd) (list 12 'e)))
(check-equal? (blindE 3 '()) '())
(check-equal? (blindE 3 (list (list 1 'a))) (list (list 1 'a)))
(check-equal? (blindE 3 (list (list 1 'a) (list 2 'b) (list 3 'c)
                                                (list 4 'd) (list 5 'e) (list 6 'f)))
      '((1 a) (4 d)))

;; startsWith tests
(check-equal? (startsWith 10 standard-evt-stream) (behavior 10 (list (list 1 11) (list 2 '12) (list 3 13))))
     ; (list (list 0 10) (list 1 11) (list 2 12) (list 3 13)))
(check-equal? (startsWith 'zero empty-evt-stream) (behavior 'zero '())) ;;(list (list 0 'zero)))

;; changes tests
(check-equal? (changes (behavior 0 (list (list 1 1) (list 2 2) (list 3 3)))) (list (list 1 1) (list 2 2) (list 3 3)))
(check-equal? (changes (behavior 0 '())) '())

;; behavior
(check-equal? (behavior-init (behavior 'a '())) 'a)
(check-equal? (behavior-changes (behavior 'a '())) '())

;; project-values
(check-equal? (project-values (behavior 'a (list (list 1 'a) (list 3 'b) (list 6 'c))) (list 1 2 4 7))
      (list (list 1 'a) (list 2 'a) (list 4 'b) (list 7 'c)))

;; valueNow
(check-equal? (valueNow (behavior 3 (list (list 1 'a))) 1) 'a)
(check-equal? (valueNow (behavior 3 (list (list 1 'a) (list 5 'b))) 3) 'a)
(check-equal? (valueNow (behavior 3 '()) 10) 3)
(check-equal? (valueNow (behavior 3 (list (list 10 5))) 6) 3)

;; constantB
(check-equal? (constantB 'a) (behavior 'a '()))

;; delayB
(check-equal? (delayB 3 (behavior 'a (list (list 1 'b) (list 2 'c))))
              (behavior 'a (list (list 4 'b) (list 5 'c))))

;; andB
(check-equal? (andB (behavior #t (list (list 1 #t) (list 5 #f) (list 7 #t)))
                   (behavior #t (list (list 1 #t) (list 3 #t) (list 6 #t) (list 7 #f))))
      (behavior #t (list (list 1 #t) (list 3 #t) (list 5 #f) (list 6 #f) (list 7 #f))))

;; orB
#;(check-equal? (orB (behavior #f (list (list 1 #t) (list 5 #f) (list 10 #t) (list 11 #f)))
                   (behavior #t (list (list 2 #f) (list 12 #t))))
              (behavior #t (list (list 1 #t) (list 2 #t) (list 5 #f) (list 10 #t) (list 11 #f) (list 12 #t))))

;; notB
(check-equal? (notB (behavior #t (list (list 1 #f) (list 2 #t) (list 3 #f))))
                   (behavior #f (list (list 1 #t) (list 2 #f) (list 3 #t))))

;; liftB
(check-equal? (liftB (λ (v) (+ v 3)) (behavior 0 (list (list 1 2) (list 2 3) (list 5 4))))
      (behavior 3 (list (list 1 5) (list 2 6) (list 5 7))))
(check-equal? (liftB2 + (behavior 0 (list (list 1 1) (list 2 2) (list 3 3)))
                                   (behavior 10 (list (list 1 1) (list 2 2) (list 3 3))))
      (behavior 10 (list (list 1 2) (list 2 4) (list 3 6))))
(check-equal? (liftB (λ (t) (<= t 2)) (behavior 1 (list (list 1 0) (list 3 0))))
              (behavior #t (list (list 1 #t) (list 3 #t))))
(check-equal? (liftB (λ (t) (<= t 2)) (behavior 1 '()))
              (behavior #t '()))

;; condB
(check-equal? (condB (list (list (behavior #t (list (list 1 #f))) (behavior 3 (list (list 1 3)))) (list (behavior #f (list (list 1 #t))) (behavior 5 (list (list 1 5))))))
      (behavior 3 (list (list 1 5))))
(check-equal? (condB (list
                                   (list (behavior #t (list (list 1 #f))) (behavior #f (list (list 1 'a))))
                                   (list (behavior #t (list (list 1 #t))) (behavior 'wrong (list (list 1 'b))))))
      (behavior #f (list (list 1 'b))))

;; ifB
(check-equal? (ifB (behavior #t (list (list 1 #t) (list 3 #f) (list 5 #f) (list 7 #t)))
                                     (behavior 't (list (list 1 't) (list 3 't) (list 5 't) (list 7 't)))
                                     (behavior 'f (list (list 1 'f) (list 3 'f) (list 5 'f) (list 7 'f))))
      (behavior 't (list (list 1 't) (list 3 'f) (list 5 'f) (list 7 't))))

(check-equal? (ifB (behavior #t (list (list 1 #f) (list 5 #t) (list 9 #f)))
                                         (behavior 't (list (list 1 '1) (list 3 '3) (list 5 '5) (list 11 '11)))
                                         (behavior 'f (list (list 1 'no) (list 2 '2) (list 5 'no) (list 6 '6) (list 8 '8))))
      (behavior 't (list (list 1 'no) (list 2 '2) (list 3 '2) (list 5 '5) (list 6 '5) (list 8 '5) (list 9 '8) (list 11 '8))))