#lang rosette

(require "rxapi.rkt")
(require rackunit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   Observable constructors
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-equal? (intervalO 3 10) (list '() '() '(1) '() '() '(2) '() '() '(3) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   Observable methods
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-equal? (mapO + (list '(1) '(2)) (list '(3) '(4))) (list '(4) '(6)))

(check-equal? (scanO-no-seed + (list '(1) '(2) '(3))) (list '(1) '(3) '(6)))
(check-equal? (scanO-seed + 10 (list '(1) '(2) '(3))) (list '(11) '(13) '(16)))