#lang rosette
(require rackunit)
(require "densefjapi.rkt")
(require "dense-fjmodels.rkt")

(define standard-evt-stream (list (list 1 11) (list 2 '12) (list 3 13)))
(define empty-evt-stream '())
(define stream-with-no-evts (list (list 1 'no-evt) (list 2 "hello") (list 5 "world") (list 6 'no-evt)))

;; oneE tests
;(check-equal? (oneE (list 'one 'two 'three)) (list 'one))
;(check-equal? (oneE '()) '())

;; zeroE tests
(check-equal? (zeroE) '())

;; mapE tests
(check-equal? (mapE add1 '(11 12 13)) '(12 13 14))
(check-equal? (mapE add1 '()) '())
(check-equal? (mapE add1 '(11 no-evt 13)) '(12 no-evt 14))

;; mergeE tests
(check-equal? (mergeE '(one no-evt three) '(no-evt two no-evt)) '(one two three))
(check-equal? (mergeE '() '()) '())
;(check-equal? (mergeE '(one no-evt three) '()) '(one no-evt three))

;; switchE tests
;(check-equal? (switchE '()) '())
;(check-equal? (switchE (list (list 'a 'b))) (list 'a 'b))
;(check-equal? (switchE (list (list 'no-evt 'a 'no-evt 'b 'no-evt 'no-evt 'no-evt 'no-evt 'c)
;                             'no-evt
;                             'no-evt
;                             (list 'd 'e)))
;              (list 'no-evt 'a 'no-evt 'b 'd 'e))

;; filterE tests
(check-equal? (filterE not (list #t #f #t 'no-evt)) (list 'no-evt #f 'no-evt 'no-evt))

;; ifE tests
#;(check-equal? (ifE (list #t 'no-evt #f 'no-evt 'no-evt 'no-evt 'no-evt #t)
                   (list 'a 'b 'c 'd 'e 'f 'g)
                   (list 'x 'y' z 'aa 'bb 'cc 'dd))
              (list 'a 'no-evt 'z 'no-evt 'no-evt 'no-evt 'no-evt 'g))

;; constantE tests
(check-equal? (constantE "hello" (list 1 'no-evt 3)) (list "hello" 'no-evt "hello"))
(check-equal? (constantE "hello" '()) '())

;; collectE tests
(check-equal? (collectE 1 + (list 11 12 13)) (list 12 24 37))
(check-equal? (collectE 0 + '()) '())
(check-equal? (collectE 0 + (list 11 'no-evt 13)) (list 11 'no-evt 24))

;; andE tests
(check-equal? (andE (list #f #t 'no-evt #f #t)
                    (list #t #t #t #t #f))
              (list #f #t 'no-evt #f #f))

;; notE tests
(check-equal? (notE (list #f #t #f)) (list #t #f #t))
(check-equal? (notE (list #f 'no-evt #t)) (list #t 'no-evt #f))

;; filterRepeatsE
(check-equal? (filterRepeatsE '()) '())
(check-equal? (filterRepeatsE '(1)) '(1))
(check-equal? (filterRepeatsE '(1 1)) '(1 no-evt))
(check-equal? (filterRepeatsE '(1 1 no-evt 2 3 2 no-evt 2)) '(1 no-evt no-evt 2 3 2 no-evt no-evt))

;; snapshotE tests
(check-equal? (snapshotE (list #t 'no-evt #f) (behavior 1 (list 2 3 4))) (list 2 'no-evt 4))

;; delayE tests
(check-equal? (delayE 3 (list 1 'no-evt 'no-evt 2 3)) (list 'no-evt 'no-evt 'no-evt 1 'no-evt))
(check-equal? (delayE 0 (list 'no-evt 1 'no-evt 2 3)) (list 'no-evt 1 'no-evt 2 3))

;; calmE tests
;(check-equal? (calmE 3 '(a no-evt no-evt no-evt b c d)) '(no-evt no-evt no-evt a no-evt no-evt no-evt d))
;(check-equal? (calmE 3 '()) '())
;(check-equal? (calmE 3 '(a)) '(no-evt no-evt no-evt a))
;(check-equal? (calmE 3 '(a b c d e f)) '(no-evt no-evt no-evt c no-evt no-evt no-evt f))

;; blindE tests
;(check-equal? (blindE 2 (list 'a 'b 'c 'no-evt 'no-evt 'no-evt 'd 'no-evt 'no-evt 'no-evt 'no-evt 'd 'e))
;              (list 'a 'no-evt 'no-evt 'no-evt 'no-evt 'no-evt 'd 'no-evt 'no-evt 'no-evt 'no-evt 'd 'no-evt))
;(check-equal? (blindE 3 '()) '())
;(check-equal? (blindE 3 '(a)) '(a))
;(check-equal? (blindE 3 '(a b c d e f)) '(a no-evt no-evt no-evt e no-evt))

;; timerE tests
(check-equal? (timerE 3 '(a b c d e f g)) (list 'no-evt 'no-evt #t 'no-evt 'no-evt #t 'no-evt))
(check-equal? (timerE 3 '()) '())
(check-equal? (timerE 0 '(a b c d)) '(no-evt no-evt no-evt no-evt))

;; startsWith tests
(check-equal? (startsWith 10 '(11 12 13)) (behavior 10 '(11 12 13)))
(check-equal? (startsWith 'zero '()) (behavior 'zero '()))
(check-equal? (startsWith 'a '(no-evt no-evt b no-evt c d)) (behavior 'a '(a a b b c d)))

;; changes tests
(check-equal? (changes (behavior 0 (list (list 1 1) (list 2 2) (list 3 3)))) (list (list 1 1) (list 2 2) (list 3 3)))
(check-equal? (changes (behavior 0 '())) '())

;; behavior
(check-equal? (behavior-init (behavior 'a '())) 'a)
(check-equal? (behavior-changes (behavior 'a '())) '())

;; project-values
;(check-equal? (project-values (behavior 'a (list (list 1 'a) (list 3 'b) (list 6 'c))) (list 1 2 4 7))
;      (list (list 1 'a) (list 2 'a) (list 4 'b) (list 7 'c)))

;; valueNow
;(check-equal? (valueNow (behavior 3 (list (list 1 'a))) 1) 'a)
;(check-equal? (valueNow (behavior 3 (list (list 1 'a) (list 5 'b))) 3) 'a)
;(check-equal? (valueNow (behavior 3 '()) 10) 3)
;(check-equal? (valueNow (behavior 3 (list (list 10 5))) 6) 3)

;; constantB
(check-equal? (constantB 'a (behavior #t '())) (behavior 'a '()))
(check-equal? (constantB 'a (behavior #t (list #t #f #t))) (behavior 'a '(a a a)))

;; switchB
(check-equal? (switchB (behavior (behavior 'a '(a a a)) (list (behavior 1 (list 1 1 1))
                                                              (behavior 1 (list 1 1 1))
                                                              (behavior 'b '(b b b)))))
              (behavior 'a (list 1 1 'b)))

;; delayB
#;(check-equal? (delayB 3 (behavior 'a (list (list 1 'b) (list 2 'c))))
              (behavior 'a (list (list 4 'b) (list 5 'c))))

;; andB
(check-equal? (andB (behavior #t (list #t #f #t)) (behavior #t (list #t #t #f)))
              (behavior #t (list #t #f #f)))
;; need to fix this issue
#;(check-equal? (andB (behavior #t '()) (behavior #t (list #t #f #t #f)))
              (behavior #t (list #t #f #t #f)))

;; orB
(check-equal? (orB (behavior #f (list #t #f #t #f)) (behavior #t (list #f #f #f #t)))
              (behavior #t (list #t #f #t #t)))
#;(check-equal? (orB (behavior #f (list #t)) (behavior #f (list #f #t)))
              (behavior #f (list #t #t)))

;; notB
(check-equal? (notB (behavior #t (list #f #t #f))) (behavior #f (list #t #f #t)))

;; liftB
(check-equal? (liftB1 (λ (v) (+ v 3)) (behavior 0 (list 2 3 4)))
              (behavior 3 (list 5 6 7)))
(check-equal? (liftB2 + (behavior 0 (list 1 2 3)) (behavior 10 (list 1 2 3)))
              (behavior 10 (list 2 4 6)))
(check-equal? (liftB1 (λ (t) (<= t 2)) (behavior 1 (list 0 5)))
              (behavior #t (list #t #f)))
(check-equal? (liftB1 (λ (t) (<= t 2)) (behavior 1 '())) (behavior #t '()))

;; condB
(check-equal? (condB (list (list (behavior #t (list #f)) (behavior 3 (list 3)))
                           (list (behavior #f (list #t)) (behavior 5 (list 5)))))
      (behavior 3 (list 5)))
(check-equal? (condB (list (list (behavior #t (list #f)) (behavior #f (list 'a)))
                           (list (behavior #t (list #t)) (behavior 'wrong (list 'b)))))
      (behavior #f (list 'b)))
(check-equal? (condB (list (list (behavior #t (list #f)) (behavior 'a (list 'b)))
                           (list (behavior #f (list #t #t)) (behavior 'no (list 'okay 'okay2)))))
              (behavior 'a (list 'okay 'okay2)))

;; ifB
(check-equal? (ifB (behavior #t (list #t #f #f #t))
                   (behavior 't (list 't 't 't 't))
                   (behavior 'f (list 'f 'f 'f 'f)))
              (behavior 't (list 't 'f 'f 't)))

#;(check-equal? (ifB (behavior #t (list (list 1 #f) (list 5 #t) (list 9 #f)))
                                         (behavior 't (list (list 1 '1) (list 3 '3) (list 5 '5) (list 11 '11)))
                                         (behavior 'f (list (list 1 'no) (list 2 '2) (list 5 'no) (list 6 '6) (list 8 '8))))
      (behavior 't (list (list 1 'no) (list 2 '2) (list 3 '2) (list 5 '5) (list 6 '5) (list 8 '5) (list 9 '8) (list 11 '8))))

;; collectB
(check-equal? (collectB 0 + (behavior 1 '(2 3 4)))
              (behavior 1 '(3 6 10)))