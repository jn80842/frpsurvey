#lang racket

(require rackunit)
(require "fjmodel.rkt")
(require "fjapi.rkt")

;; oneE tests

;; zeroE tests
(check-equal? (zeroE 'a) 'no-evt)

;; mapE tests
(check-equal? (mapE add1 1) 2)

;; mergeE tests
(check-equal? (mergeE 'a 'no-evt) 'a)
(check-equal? (mergeE 'no-evt 'b) 'b)
(check-equal? (mergeE 'a 'b) 'a)

;; filterE tests
(check-equal? (filterE odd? 1) 1)
(check-equal? (filterE odd? 2) 'no-evt)

;; ifE
(check-equal? (ifE #t 'a 'b) 'a)
(check-equal? (ifE #f 'a 'b) 'b)
(check-equal? (ifE 'no-evt 'a 'b) 'no-evt)
(check-equal? (ifE #t 'no-evt 'b) 'no-evt)

;; constantE
(check-equal? (constantE 'a 1) 'a)
(check-equal? (constantE 'a 'no-evt) 'no-evt)

;; collectE
(check-equal? (collectE 1 + (list 1 2)) 4)
(check-equal? (collectE 1 + (list 1 2 'no-evt)) 'no-evt)
(check-equal? (collectE 1 + (list 1 2 'no-evt 3)) 7)

;; andE
(check-equal? (andE #t #t) #t)
(check-equal? (andE #t #f) #f)
(check-equal? (andE #f #t) #f)
(check-equal? (andE 'no-evt #t) 'no-evt)
(check-equal? (andE #t 'no-evt) 'no-evt)

;; orE
(check-equal? (orE #t #t) #t)
(check-equal? (orE #t #f) #t)
(check-equal? (orE #f #t) #t)
(check-equal? (orE #f #f) #f)
(check-equal? (orE 'no-evt #t) 'no-evt)
(check-equal? (orE #t 'no-evt) 'no-evt)

;; notE
(check-equal? (notE #t) #f)
(check-equal? (notE #f) #t)
(check-equal? (notE 'no-evt) 'no-evt)

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
(check-equal? (startsWith 1 '()) 1)
(check-equal? (startsWith 1 '(no-evt no-evt)) 1)
(check-equal? (startsWith 1 '(2 no-evt)) 2)
(check-equal? (startsWith 1 '(2 no-evt 3)) 3)

;; changes
(check-equal? (changes 'a) 'a)
(check-equal? (changes 'no-evt) 'no-evt)

;; constantB
(check-equal? (constantB 'a 1) 'a)

;; delayB

;; switchB

;; andB
(check-equal? (andB #t #t) #t)
(check-equal? (andB #t #f) #f)
(check-equal? (andB #f #t) #f)

;; orB
(check-equal? (orB #t #t) #t)
(check-equal? (orB #t #f) #t)
(check-equal? (orB #f #t) #t)
(check-equal? (orB #f #f) #f)

;; notB
(check-equal? (notB #t) #f)
(check-equal? (notB #f) #t)

;; liftB
(check-equal? (liftB add1 1) 2)
(check-equal? (liftB + 1 2) 3)

;; ifB
(check-equal? (ifB #t 'a 'b) 'a)
(check-equal? (ifB #f 'a 'b) 'b)

;; timerB

;; blindB

;; calmB