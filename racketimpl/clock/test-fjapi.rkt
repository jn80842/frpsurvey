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

;; snapshotE

;; delayE

;; calmE

;; blindE

;; timerE

;; startsWith

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

;; ifB
(check-equal? (ifB #t 'a 'b) 'a)
(check-equal? (ifB #f 'a 'b) 'b)

;; timerB

;; blindB

;; calmB