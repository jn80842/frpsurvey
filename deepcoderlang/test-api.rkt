#lang racket

(require "api.rkt")
(require rackunit)

(check-equal? (head-dc '(1 2 3 4 5)) 1)
(check-equal? (head-dc '()) 'null)

(check-equal? (last-dc '(1 2 3 4 5)) 5)
(check-equal? (last-dc '()) 'null)

(check-equal? (take-dc 2 '(1 2 3 4 5)) '(1 2))
(check-equal? (take-dc 10 '(1 2 3 4 5)) '(1 2 3 4 5))

(check-equal? (drop-dc 2 '(1 2 3 4 5)) '(3 4 5))
(check-equal? (drop-dc 10 '(1 2 3 4 5)) '())

(check-equal? (access-dc 2 '(1 2 3 4 5)) 3)
(check-equal? (access-dc 10 '(1 2 3 4 5)) 'null)

(check-equal? (minimum-dc '(1 2 3 4 5)) 1)
(check-equal? (minimum-dc '()) 'null)

(check-equal? (maximum-dc '(1 2 3 4 5)) 5)
(check-equal? (maximum-dc '()) 'null)

(check-equal? (reverse-dc '(1 2 3 4 5)) '(5 4 3 2 1))
(check-equal? (reverse-dc '()) '())

(check-equal? (sort-dc '(3 2 5 4 1)) '(1 2 3 4 5))
(check-equal? (sort-dc '()) '())

(check-equal? (sum-dc '(1 2 3 4 5)) '(15))
(check-equal? (sum-dc '()) '(0))

(check-equal? (map-dc add1 '(1 2 3 4 5)) '(2 3 4 5 6))
(check-equal? (map-dc add1 '()) '())

(check-equal? (filter-dc odd? '(1 2 3 4 5)) '(1 3 5))

(check-equal? (count-dc odd? '(1 2 3 4 5)) 3)

(check-equal? (zipwith-dc + '(1 2 3 4 5) '(5 4 3 2 1)) '(6 6 6 6 6))
(check-equal? (zipwith-dc + '(1 2 3 4 5) '(5 4 3)) '(6 6 6))

(check-equal? (scanl1-dc + '(1 2 3 4 5)) '(1 3 6 10 15))