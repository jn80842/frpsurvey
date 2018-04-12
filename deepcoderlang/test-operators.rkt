#lang racket

(require "operators.rkt")
(require rackunit)

;; head
(check-equal? (call-dc-insn (dc-insn 0 0 0 0) (list '(1 2 3 4 5))) 1)
(check-equal? (call-dc-insn (dc-insn 0 0 0 0) (list '())) 'null)
;; last
(check-equal? (call-dc-insn (dc-insn 1 0 0 0) (list '(1 2 3 4 5))) 5)
(check-equal? (call-dc-insn (dc-insn 1 0 0 0) (list '())) 'null)
;; take
(check-equal? (call-dc-insn (dc-insn 2 0 0 1) (list '(1 2 3 4 5) 2)) '(1 2))
(check-equal? (call-dc-insn (dc-insn 2 0 0 1) (list '(1 2 3 4 5) 10)) '(1 2 3 4 5))
;; drop
(check-equal? (call-dc-insn (dc-insn 3 0 0 1) (list '(1 2 3 4 5) 2)) '(3 4 5))
(check-equal? (call-dc-insn (dc-insn 3 0 0 1) (list '(1 2 3 4 5) 10)) '())
;; access
(check-equal? (call-dc-insn (dc-insn 4 0 0 1) (list '(1 2 3 4 5) 2)) 3)
(check-equal? (call-dc-insn (dc-insn 4 0 0 1) (list '(1 2 3 4 5) 10)) 'null)
;; minimum
(check-equal? (call-dc-insn (dc-insn 5 0 0 0) (list '(1 2 3 4 5))) 1)
(check-equal? (call-dc-insn (dc-insn 5 0 0 0) (list '())) 'null)
;; maximum
(check-equal? (call-dc-insn (dc-insn 6 0 0 0) (list '(1 2 3 4 5))) 5)
(check-equal? (call-dc-insn (dc-insn 6 0 0 0) (list '())) 'null)
;; reverse
(check-equal? (call-dc-insn (dc-insn 7 0 0 0) (list '(1 2 3 4 5))) '(5 4 3 2 1))
(check-equal? (call-dc-insn (dc-insn 7 0 0 0) (list '())) '())
;; sort
(check-equal? (call-dc-insn (dc-insn 8 0 0 0) (list '(3 2 5 4 1))) '(1 2 3 4 5))
(check-equal? (call-dc-insn (dc-insn 8 0 0 0) (list '())) '())
;; sum
(check-equal? (call-dc-insn (dc-insn 9 0 0 0) (list '(1 2 3 4 5))) 15)
(check-equal? (call-dc-insn (dc-insn 9 0 0 0) (list '())) 0)
;; map (λ (+ i 1)
(check-equal? (call-dc-insn (dc-insn 10 0 0 0) (list '(1 2 3 4 5))) '(2 3 4 5 6))
(check-equal? (call-dc-insn (dc-insn 10 0 0 0) (list '())) '())
;; filter odd?
(check-equal? (call-dc-insn (dc-insn 11 0 1 0) (list '(1 2 3 4 5))) '(1 3 5))
;; count odd?
(check-equal? (call-dc-insn (dc-insn 12 0 1 0) (list '(1 2 3 4 5))) 3)
;; zipwith +
(check-equal? (call-dc-insn (dc-insn 13 0 0 1) (list '(1 2 3 4 5) '(5 4 3 2 1))) '(6 6 6 6 6))
(check-equal? (call-dc-insn (dc-insn 13 0 0 1) (list '(1 2 3 4 5) '(5 4 3))) '(6 6 6))
;; scanl1 +
(check-equal? (call-dc-insn (dc-insn 14 0 0 0) (list '(1 2 3 4 5))) '(1 3 6 10 15))