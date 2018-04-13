#lang rosette

(require "api.rkt")
(require "operators.rkt")
(require "sketch.rkt")
(require "specifications.rkt")

(current-bitwidth 9)

(define sketch2-1 (sketch (get-holes-list 2) (get-retval-idx) 1))
(define sketch2-2 (sketch (get-holes-list 2) (get-retval-idx) 2))
(define sketch3-2 (sketch (get-holes-list 3) (get-retval-idx) 2))
(define sketch4-1 (sketch (get-holes-list 4) (get-retval-idx) 1))
(define sketch4-2 (sketch (get-holes-list 4) (get-retval-idx) 2))
(define sketch5-2 (sketch (get-holes-list 5) (get-retval-idx) 2))

(define (program0-graph int int-list)
  (define r1 int)
  (define r2 int-list)
  (define r3 (sort-dc r2))
  (define r4 (take-dc r1 r3))
  (define r5 (sum-dc r4))
  r5)

(define program0-inputs (for/list ([i (range 5)])
                           (list (random 6) (get-random-list 5))))
(define program0-outputs (map (λ (i) (apply program0-graph i)) program0-inputs))

;(synth-from-ref-impl sketch3-2 program0-graph (get-sym-int) (sym-int-list 3)) ;; 48s
(synth-from-io-pairs sketch3-2 program0-inputs program0-outputs) ;; 9s

(define (program1-graph int-list1 int-list2)
  (define r1 int-list1)
  (define r2 int-list2)
  (define r3 (map-dc (λ (i) (* i 3)) r1))
  (define r4 (zipwith-dc + r2 r3))
  (define r5 (maximum-dc r4))
  r5)

(define program1-inputs (for/list ([i (range 5)]) (list (get-random-list 5) (get-random-list 5))))
(define program1-outputs (map (λ (i) (apply program1-graph i)) program1-inputs))

;(synth-from-ref-impl sketch3-2 program1-graph (sym-int-list 3) (sym-int-list 3)) ;; 35s
;(synth-from-io-pairs sketch3-2 program1-inputs program1-outputs) ;; 38s

(define (program2-graph int-list1 int-list2)
  (define r1 int-list1)
  (define r2 int-list2)
  (define r3 (zipwith-dc - r2 r1))
  (define r4 (count-dc positive? r3))
  r4)

(define program2-inputs (for/list ([i (range 5)]) (list (get-random-list 5) (get-random-list 5))))
(define program2-outputs (map (λ (i) (apply program2-graph i)) program2-inputs))

;(synth-from-ref-impl sketch2-2 program2-graph (sym-int-list 3) (sym-int-list 3)) ;; 7s
;(synth-from-io-pairs sketch2-2 program2-inputs program2-outputs) ;; 9s

(define (program3-graph int-list)
  (define r1 int-list)
  (define r2 (scanl1-dc min r1))
  (define r3 (zipwith-dc - r1 r2))
  (define r4 (filter-dc positive? r3))
  (define r5 (sum-dc r4))
  r5)

(define program3-inputs (for/list ([i (range 5)]) (list (get-random-list 5))))
(define program3-outputs (map (λ (i) (apply program3-graph i)) program3-inputs))

;(synth-from-ref-impl sketch4-1 program3-graph (sym-int-list 3)) ;; >300s
;(synth-from-io-pairs sketch4-1 program3-inputs program3-outputs) ;; 63s

(define (program4-graph int-list1 int-list2)
  (define r1 int-list1)
  (define r2 int-list2)
  (define r3 (sort-dc r1))
  (define r4 (sort-dc r2))
  (define r5 (reverse-dc r4))
  (define r6 (zipwith-dc * r4 r5))
  (define r7 (sum-dc r6))
  r7)

(define program4-inputs (for/list ([i (range 5)]) (list (get-random-list 5) (get-random-list 5))))
(define program4-outputs (map (λ (i) (apply program4-graph i)) program4-inputs))

;(synth-from-ref-impl sketch5-2 program4-graph (sym-int-list 3) (sym-int-list 3))
(synth-from-io-pairs sketch5-2 program4-inputs program4-outputs) ;; >300s

(define (program5-graph int-list)
  (define r1 int-list)
  (define r2 (reverse-dc r1))
  (define r3 (zipwith-dc min r1 r2))
  r3)

(define program5-inputs (for/list ([i (range 5)]) (list (get-random-list 5))))
(define program5-outputs (map (λ (i) (apply program5-graph i)) program5-inputs))

;(synth-from-ref-impl sketch2-1 program5-graph (sym-int-list 3)) ;; 2s
;(synth-from-io-pairs sketch2-1 program5-inputs program5-outputs) ;; 3s

(define (program6-graph int-list1 int-list2)
  (define r1 int-list1)
  (define r2 int-list2)
  (define r3 (map-dc sub1 r1))
  (define r4 (map-dc sub1 r2))
  (define r5 (zipwith-dc + r3 r4))
  (define r6 (minimum-dc r5))
  r6)

(define program6-inputs (for/list ([i (range 5)]) (list (get-random-list 5) (get-random-list 5))))
(define program6-outputs (map (λ (i) (apply program6-graph i)) program6-inputs))

;(synth-from-ref-impl sketch4-2 program6-graph (sym-int-list 3) (sym-int-list 3)) ;; >300s
;(synth-from-io-pairs sketch4-2 program6-inputs program6-outputs) ;; 292s

(define (program7-graph int-list1 int-list2)
  (define r1 int-list1)
  (define r2 int-list2)
  (define r3 (scanl1-dc + r2))
  (define r4 (zipwith-dc * r1 r3))
  (define r5 (sum-dc r4))
  r5)

(define program7-inputs (for/list ([i (range 5)]) (list (get-random-list 5) (get-random-list 5))))
(define program7-outputs (map (λ (i) (apply program7-graph i)) program7-inputs))

;(synth-from-ref-impl sketch3-2 program7-graph (sym-int-list 3) (sym-int-list 3))
;(synth-from-io-pairs sketch3-2 program7-inputs program7-outputs) ;; 32s

(define (program8-graph int-list)
  (define r1 int-list)
  (define r2 (reverse-dc r1))
  (define r3 (zipwith-dc - r2 r1))
  (define r4 (filter-dc positive? r3))
  (define r5 (sum-dc r4))
  r5)

(define program8-inputs (for/list ([i (range 5)]) (list (get-random-list 5))))
(define program8-outputs (map (λ (i) (apply program8-graph i)) program8-inputs))

;(synth-from-ref-impl sketch4-1 program8-graph (sym-int-list 3)) ;; >300s
;(synth-from-io-pairs sketch4-1 program8-inputs program8-outputs) ;; 276s