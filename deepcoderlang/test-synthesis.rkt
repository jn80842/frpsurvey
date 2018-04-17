#lang rosette

(require rackunit)
(require "api.rkt")
(require "operators.rkt")
(require "sketch.rkt")
(require "specifications.rkt")

; (error-print-width 100000000000)
(current-bitwidth #f)

(define ilist (sym-int-list 3))
(define ilist2 (sym-int-list 3))
(define sym-int (get-sym-int))

(define sketch1-1 (sketch (get-holes-list 1) (get-retval-idx) 1))
(define sketch1-2 (sketch (get-holes-list 1) (get-retval-idx) 2))

(define (head-graph input)
  (define r1 input)
  (define r2 (head-dc input))
  r2)

(synth-from-ref-impl sketch1-1 head-graph ilist)

(define (last-graph input)
  (define r1 input)
  (define r2 (last-dc r1))
  r2)

(synth-from-ref-impl sketch1-1 last-graph ilist)

(define (take-graph input int-input)
  (define r1 input)
  (define r2 int-input)
  (define r3 (take-dc r2 r1))
  r3)

(synth-from-ref-impl sketch1-2 take-graph ilist sym-int)

(define (drop-graph input int-input)
  (define r1 input)
  (define r2 int-input)
  (define r3 (drop-dc r2 r1))
  r3)

(synth-from-ref-impl sketch1-2 drop-graph ilist sym-int)

(define (access-graph input int-input)
  (define r1 input)
  (define r2 int-input)
  (define r3 (access-dc r2 r1))
  r3)

(synth-from-ref-impl sketch1-2 access-graph ilist sym-int)

(define (minimum-graph input)
  (define r1 input)
  (define r2 (minimum-dc r1))
  r2)

(synth-from-ref-impl sketch1-1 minimum-graph ilist)

(define (maximum-graph input)
  (define r1 input)
  (define r2 (maximum-dc r1))
  r2)

(synth-from-ref-impl sketch1-1 maximum-graph ilist)

(define (reverse-graph input)
  (define r1 input)
  (define r2 (reverse-dc r1))
  r2)

(synth-from-ref-impl sketch1-1 reverse-graph ilist)

(define (sort-graph input)
  (define r1 input)
  (define r2 (sort-dc r1))
  r2)

(synth-from-ref-impl sketch1-1 sort-graph ilist)

(define (sum-graph input)
  (define r1 input)
  (define r2 (sum-dc input))
  r2)

(synth-from-ref-impl sketch1-1 sum-graph ilist)

(define (map-graph input)
  (define r1 input)
  (define r2 (map-dc add1 r1))
  r2)

(synth-from-ref-impl sketch1-1 map-graph ilist)

(define (filter-graph input)
  (define r1 input)
  (define r2 (filter-dc odd? r1))
  r2)

(synth-from-ref-impl sketch1-1 filter-graph ilist)

(define (count-graph input)
  (define r1 input)
  (define r2 (count-dc odd? r1))
  r2)

(synth-from-ref-impl sketch1-1 count-graph ilist)

(define (zipwith-graph input1 input2)
  (define r1 input1)
  (define r2 input2)
  (define r3 (zipwith-dc + r1 r2))
  r3)

(synth-from-ref-impl sketch1-2 zipwith-graph ilist ilist2)

(define (scanl1-graph input)
  (define r1 input)
  (define r2 (scanl1-dc + r1))
  r2)

(synth-from-ref-impl sketch1-1 scanl1-graph ilist)