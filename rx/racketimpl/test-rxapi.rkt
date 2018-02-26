#lang rosette

(require "rxapi.rkt")
(require rackunit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   Observable constructors
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-equal? (combineLatestO (list '() '(1) '() '(1))
                              (list '() '() '(2) '(2))) (list '() '() '(1 2) '(1 2)))
(check-equal? (combineLatestO (list '() '(1) '() '(2))
                              (list '() '() '(3) '(4))
                              (list '() '() '() '(10))) (list '() '() '() '(2 4 10)))

(check-equal? (fromEventO (list '() '() '(a) '()))
              (list '() '() '(a) '()))
(check-equal? (fromEventO (list '() '(1) '(2) '()) add1)
              (list '() '(2) '(3) '()))

(check-equal? (intervalO 3 10) (list '() '() '(1) '() '() '(2) '() '() '(3) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   Observable methods
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-equal? (distinctUntilChangedO (list '() '() '(1) '(1) '(2) '(1)))
              (list '() '() '(1) '() '(2) '(1)))

(check-equal? (mapO add1 (list '(1) '(2))) (list '(2) '(3)))
(check-equal? (mapO-with-datetime - (list '() '(1) '() '(0)))
              (list '() '(1) '() '(4)))

(check-equal? (scanO-no-seed + (list '(1) '(2) '(3))) (list '(1) '(3) '(6)))
(check-equal? (scanO-seed + 10 (list '(1) '(2) '(3))) (list '(11) '(13) '(16)))

(check-equal? (withLatestFromO (list '() '(3) '() '(9))
                               (list '(1) '() '(2) '())) (list '() '(3 1) '() '(9 2)))
(check-equal? (withLatestFromO (list '(3) '() '(1) '(0))
                               (list '() '(1) '() '())) (list '() '() '(1 1) '(0 1)))