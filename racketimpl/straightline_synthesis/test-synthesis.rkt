#lang rosette

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../operators.rkt")
(require "../sketch.rkt")
;(require "../straightline.rkt")

(current-bitwidth 6)

(define stream-length 5)

(define holes (list (get-insn-holes)))

(define int-stream (new-event-stream get-sym-int stream-length))
(define int-stream2 (new-event-stream get-sym-int stream-length))
(define bool-stream (new-event-stream get-sym-bool stream-length))
(define off-stream (new-event-stream (λ () 'off) stream-length))

(define int-behavior (new-behavior get-sym-int stream-length))
(define int-behavior2 (new-behavior get-sym-int stream-length))
(define int-behavior3 (new-behavior get-sym-int stream-length))
(define bool-behavior (new-behavior get-sym-bool stream-length))
(define bool-behavior2 (new-behavior get-sym-bool stream-length))

(define sketch1-f-1 (sketch holes (make-vector 1 #f) stateless-operator-list stateful-operator-list 1))
(define sketch1-t-1 (sketch holes (make-vector 1 #t) stateless-operator-list stateful-operator-list 1))
(define sketch1-f-2 (sketch holes (make-vector 1 #f) stateless-operator-list stateful-operator-list 2))
(define sketch1-f-3 (sketch holes (make-vector 1 #f) stateless-operator-list stateful-operator-list 3))
;; constantE-imm

(define (constantE-imm-graph e)
  (define r1 e)
  (define r2 (constantE 1 e))
  r2)

(displayln "constantE-imm")
(synth-from-ref-impl sketch1-f-1 constantE-imm-graph int-stream)

;; constantE

(define (constantE-graph e)
  (define r1 e)
  (define r2 (constantE 'test e))
  r2)

(displayln "constantE")
(synth-from-ref-impl sketch1-f-1 constantE-graph int-stream)

;; mergeE

(define (mergeE-graph e1 e2)
  (define r1 e1)
  (define r2 e2)
  (define r3 (mergeE r1 r2))
  r3)

(for ([i (range stream-length)])
  (assert (or (eq? 'no-evt (list-ref int-stream i))
              (eq? 'no-evt (list-ref int-stream2 i)))))

(displayln "mergeE")
(synth-from-ref-impl sketch1-f-2 mergeE-graph int-stream int-stream2)

;; collectE-imm

(define (collectE-imm-graph e)
  (define r1 e)
  (define r2 (collectE 0 + e))
  r2)

(displayln "collectE")
(synth-from-ref-impl sketch1-t-1 collectE-imm-graph int-stream)

;; snapshotE

(define (snapshotE-graph e b)
  (define r1 e)
  (define r2 b)
  (define r3 (snapshotE r1 r2))
  r3)

(displayln "snapshotE")
(synth-from-ref-impl sketch1-f-2 snapshotE-graph int-stream int-behavior)

;; startsWith

(define (startsWith-graph e)
  (define r1 e)
  (define r2 (startsWith #f e))
  r2)

(displayln "startsWith")
(synth-from-ref-impl sketch1-t-1 startsWith-graph int-stream)

;; startsWith-imm

(define (startsWith-imm-graph e)
  (define r1 e)
  (define r2 (startsWith 1 e))
  r2)

(displayln "startsWith-imm")
(synth-from-ref-impl sketch1-t-1 startsWith-imm-graph int-stream)

;; mapE

(define (mapE-graph e)
  (define r1 e)
  (define r2 (mapE (λ (c x) (+ x c)) 5 e))
  r2)

(displayln "mapE (λ (x) (+ x 5))")
(synth-from-ref-impl sketch1-f-1 mapE-graph int-stream)

;; ifE

(define (ifE-graph bool-e int-e int2-e)
  (define r1 bool-e)
  (define r2 int-e)
  (define r3 int2-e)
  (define r4 (ifE r1 r2 r3))
  r4)

(displayln "ifE")
(synth-from-ref-impl sketch1-f-3 ifE-graph bool-stream int-stream int-stream2)

;; filterE

(define (filterE-graph e)
  (define r1 e)
  (define r2 (filterE (λ (c i) (identity i)) 0 r1))
  r2)

(displayln "filterE")
(synth-from-ref-impl sketch1-f-1 filterE-graph int-stream)

;; filterE const

(define (filterE-const-graph e)
  (define r1 e)
  (define r2 (filterE (λ (c e) (= e c)) 3 r1))
  r2)

(displayln "filterE const")
(synth-from-ref-impl sketch1-f-1 filterE-const-graph int-stream)

;; filterRepeatsE

(define (filterRepeatsE-graph e)
  (define r1 e)
  (define r2 (filterRepeatsE r1))
  r2)

(displayln "filterRepeatsE")
(synth-from-ref-impl sketch1-t-1 filterRepeatsE-graph int-stream)

;; delayE

(define (delayE-graph e)
  (define r1 e)
  (define r2 (delayE 3 r1))
  r2)

(displayln "delayE")
(synth-from-ref-impl sketch1-t-1 delayE-graph int-stream)

;; timerE

(define (timerE-graph e)
  (define r1 e)
  (define r2 (timerE 3 r1))
  r2)

(displayln "timerE")
(synth-from-ref-impl sketch1-t-1 timerE-graph int-stream)

;; changes

(define (changes-graph b)
  (define r1 b)
  (define r2 (changes r1))
  r2)

(displayln "changes")
(synth-from-ref-impl sketch1-t-1 changes-graph int-behavior)

;; liftB1

(define (liftB-graph b)
  (define r1 b)
  (define r2 (liftB1 (λ (t) (<= t 2)) b))
  r2)

(displayln "liftB")
(synth-from-ref-impl sketch1-f-1 liftB-graph int-behavior)

;; liftB 2 consts

(define (liftB-twoconst-graph b)
  (define r1 b)
  (define r2 (liftB1 (λ (i) (and (>= i 2) (<= i 6))) r1))
  r2)

(displayln "liftB 2 consts")
(synth-from-ref-impl sketch1-f-1 liftB-twoconst-graph int-behavior)

;; andB

(define (andB-graph b1 b2)
  (define r1 b1)
  (define r2 b2)
  (define r3 (andB b1 b2))
  r3)

(displayln "andB")
(synth-from-ref-impl sketch1-f-2 andB-graph bool-behavior bool-behavior2)

;; notB

(define (notB-graph b)
  (define r1 b)
  (define r2 (notB r1))
  r2)

(displayln "notB")
(synth-from-ref-impl sketch1-f-1 notB-graph bool-behavior)

;; ifB

(define (ifB-graph b1 b2 b3)
  (define r1 b1)
  (define r2 b2)
  (define r3 b3)
  (define r4 (ifB r1 r2 r3))
  r4)

(displayln "ifB")
(synth-from-ref-impl sketch1-f-3 ifB-graph bool-behavior int-behavior int-behavior2)

;; constantB

(define (constantB-graph b1)
  (define r1 b1)
  (define r2 (constantB 'test b1))
  r2)

(displayln "constantB")
(synth-from-ref-impl sketch1-f-1 constantB-graph int-behavior)

;; constantB-imm

(define (constantB-imm-graph b1)
  (define r1 b1)
  (define r2 (constantB 1 b1))
  r2)

(displayln "constantB-imm")
(synth-from-ref-impl sketch1-f-1 constantB-imm-graph int-behavior)

;; collectB

(define (collectB-graph b1)
  (define r1 b1)
  (define r2 (collectB 'on (λ (x y) (if x y x)) b1))
  r2)

;(displayln "collectB")
;(synth-from-ref-impl sketch1-t-1 collectB-graph int-behavior)

;; collectB-imm

(define (collectB-imm-graph b1)
  (define r1 b1)
  (define r2 (collectB 0 + b1))
  r2)

(displayln "collectB")
(synth-from-ref-impl sketch1-t-1 collectB-imm-graph int-behavior)
