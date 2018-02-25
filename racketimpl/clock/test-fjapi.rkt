#lang racket

(require rackunit)
(require "fjmodel.rkt")
(require "fjapi.rkt")

;; oneE tests

;; zeroE tests
(check-equal? (zeroE 'a) 'no-evt)

;; mapE tests
(check-equal? (mapE add1 '(1)) 2)
(check-equal? (mapE add1 '(2 1)) 2)

;; mapE2 tests
(check-equal? (mapE2 + '(1 2 3) '(3 2 1)) 4)
(check-equal? (mapE2 + '(1) '(no-evt)) 'no-evt)

;; mergeE tests
(check-equal? (mergeE '(a) '(no-evt)) 'a)
(check-equal? (mergeE '(a b) '(c no-evt)) 'b)
(check-equal? (mergeE '(a) '(b)) 'a)
(check-equal? (mergeE '(a no-evt) '(c d)) 'd)

;; filterE tests
(check-equal? (filterE odd? '(1)) 1)
(check-equal? (filterE even? '(0 1 2)) 2)
(check-equal? (filterE odd? '(2)) 'no-evt)

;; ifE
(check-equal? (ifE '(#t) '(a) '(b)) 'a)
(check-equal? (ifE '(#f) '(a) '(b)) 'b)
(check-equal? (ifE '(#t) '(no-evt) '(b)) 'no-evt)

;; constantE
(check-equal? (constantE 'a '(1)) 'a)

;; collectE
(check-equal? (collectE 1 + (list 1 2)) 4)
(check-equal? (collectE 1 + (list 1 2 'no-evt 3)) 7)

;; andE
(check-equal? (andE '(#t) '(#t)) #t)
(check-equal? (andE '(#t) '(#f)) #f)
(check-equal? (andE '(#f) '(#t)) #f)
(check-equal? (andE '(#t) '(no-evt)) 'no-evt)

;; orE
(check-equal? (orE '(#t) '(#t)) #t)
(check-equal? (orE '(#t) '(#f)) #t)
(check-equal? (orE '(#f) '(#t)) #t)
(check-equal? (orE '(#f) '(#f)) #f)
(check-equal? (orE '(#t) '(no-evt)) 'no-evt)

;; notE
(check-equal? (notE '(#t)) #f)
(check-equal? (notE '(#f)) #t)

;; filterRepeatsE
(check-equal? (filterRepeatsE '(no-evt no-evt no-evt)) 'no-evt)
(check-equal? (filterRepeatsE '(1)) 1)
(check-equal? (filterRepeatsE '(1 1)) 'no-evt)
(check-equal? (filterRepeatsE '(1 2 3)) 3)
(check-equal? (filterRepeatsE '(no-evt 1)) 1)
(check-equal? (filterRepeatsE '(1 no-evt 1)) 'no-evt)
(check-equal? (filterRepeatsE '(1 1 no-evt 1)) 'no-evt)

;; snapshotE

;; onceE
(check-equal? (onceE (list 1)) 1)
(check-equal? (onceE '(no-evt)) 'no-evt)
(check-equal? (onceE '(no-evt 1)) 1)
(check-equal? (onceE '(no-evt 1 2)) 'no-evt)

;; delayE
(check-equal? (delayE 1 '(a)) 'no-evt)
(check-equal? (delayE 1 '(a no-evt)) 'a)
(check-equal? (delayE 2 '(a no-evt no-evt)) 'a)

;; calmE

;; blindE

;; timerE
(check-equal? (timerE 3 '(no-evt)) 'no-evt)
(check-equal? (timerE 3 '(no-evt no-evt)) 'no-evt)
(check-equal? (timerE 3 '(no-evt no-evt no-evt)) #t)
(check-equal? (timerE 3 '(no-evt no-evt 1)) #t)

;; startsWith
(check-equal? (startsWith 1 '()) (behavior 1 '()))
(check-equal? (startsWith 1 '(no-evt no-evt)) (behavior 1 '(1 1)))
(check-equal? (startsWith 1 '(2 no-evt)) (behavior 1 '(2 2)))
(check-equal? (startsWith 1 '(2 no-evt 3)) (behavior 1 '(2 2 3)))

;; changes
;(check-equal? (changes 'a) 'a)
;(check-equal? (changes 'no-evt) 'no-evt)

;; constantB
(check-equal? (constantB 'a (behavior #t '())) (behavior 'a '()))
(check-equal? (constantB 'a (behavior #t (list #t #f #t))) (behavior 'a '(a a a)))

;; delayB

;; switchB

;; andB
(check-equal? (andB (behavior #t (list #t #f #t)) (behavior #t (list #t #t #f)))
              (behavior #t (list #t #f #f)))

;; orB
(check-equal? (orB (behavior #f (list #t #f #t #f)) (behavior #t (list #f #f #f #t)))
              (behavior #t (list #t #f #t #t)))

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

;; ifB
(check-equal? (ifB (behavior #t (list #t #f #f #t))
                   (behavior 't (list 't 't 't 't))
                   (behavior 'f (list 'f 'f 'f 'f)))
              (behavior 't (list 't 'f 'f 't)))

;; timerB

;; blindB

;; calmB