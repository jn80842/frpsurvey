#lang rosette

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../straightline.rkt")

(current-bitwidth 6)

(define stream-length 3)

(define holes (list (get-insn-holes)))
(define-symbolic* retval-idx integer?)

(define int-stream (new-event-stream sym-integer stream-length))
(define int-stream2 (new-event-stream sym-integer stream-length))
(define off-stream (new-event-stream (λ () 'off) stream-length))

(define int-behavior (new-behavior sym-integer stream-length))
(define int-behavior2 (new-behavior sym-integer stream-length))
(define int-behavior3 (new-behavior sym-integer stream-length))
(define bool-behavior (new-behavior sym-boolean stream-length))
(define bool-behavior2 (new-behavior sym-boolean stream-length))

(define (sketch-graph1-1 e)
  (define r1 e)
  (define r2 (call-stream-insn (list-ref holes 0) (list r1)))
  (list-ref (list r1 r2) retval-idx))

(define (sketch-graph1-2 e1 e2)
  (define r1 e1)
  (define r2 e2)
  (define r3 (call-stream-insn (list-ref holes 0) (list r1 r2)))
  (list-ref (list r1 r2 r3) retval-idx))

(define (sketch-graph1-3 e1 e2 e3)
  (define r1 e1)
  (define r2 e2)
  (define r3 e3)
  (define r4 (call-stream-insn (list-ref holes 0) (list r1 r2 r3)))
  (list-ref (list r1 r2 r3 r4) retval-idx))

(define (sketch-graph1-5 e1 e2 e3 e4 e5)
  (define r1 e1)
  (define r2 e2)
  (define r3 e3)
  (define r4 e4)
  (define r5 e5)
  (define r6 (call-stream-insn (list-ref holes 0) (list r1 r2 r3 r4 r5)))
  (list-ref (list r1 r2 r3 r4 r5 r6) retval-idx))

;; constantE-imm

(define (constantE-imm-graph e)
  (define r1 e)
  (define r2 (constantE 1 e))
  r2)

(define b (synthesize #:forall (harvest int-stream)
            #:guarantee (assert (same constantE-imm-graph sketch-graph1-1 int-stream))))
(if (unsat? b)
    (displayln "!!!!! constantE-imm graph not synthesized !!!!!")
    (begin (displayln "* constantE-imm graph successfully synthesized")
           (print-from-holes (evaluate holes b)
                             (evaluate retval-idx b) 1)))

;; constantE

(define (constantE-graph e)
  (define r1 e)
  (define r2 (constantE 'test e))
  r2)

(define b-constant (synthesize #:forall (harvest int-stream)
                               #:guarantee (assert (same constantE-graph sketch-graph1-1
                                                                          int-stream))))
(if (unsat? b-constant)
    (displayln "!!!!! constantE graph not synthesized !!!!!")
    (begin (displayln "* constantE graph successfully synthesized")
           (print-from-holes (evaluate holes b-constant)
                             (evaluate retval-idx b-constant) 1)))

;; mergeE

(define (mergeE-graph e1 e2)
  (define r1 e1)
  (define r2 e2)
  (define r3 (mergeE r1 r2))
  r3)

(for ([i (range stream-length)])
  (assert (or (eq? 'no-evt (list-ref int-stream i))
              (eq? 'no-evt (list-ref int-stream2 i)))))

(define b-merge (synthesize #:forall (harvest int-stream int-stream2)
                            #:guarantee (assert (same mergeE-graph sketch-graph1-2
                                                      int-stream int-stream2))))
(if (unsat? b-merge)
    (displayln "!!!!! mergeE graph not synthesized !!!!!")
    (begin (displayln "* mergeE graph successfully synthesized")
           (print-from-holes (evaluate holes b-merge)
                             (evaluate retval-idx b-merge) 2)))

(clear-asserts!)

;; collectE-imm

(define (collectE-imm-graph e)
  (define r1 e)
  (define r2 (collectE 0 + e))
  r2)

(define b-collect-imm (synthesize #:forall (harvest int-stream)
                              #:guarantee (assert (same collectE-imm-graph
                                                        sketch-graph1-1
                                                        int-stream))))
(if (unsat? b-collect-imm)
    (displayln "!!!!! collectE-imm graph not synthesized !!!!!")
    (begin (displayln "* collectE-imm graph successfully synthesized")
           (print-from-holes (evaluate holes b-collect-imm)
                             (evaluate retval-idx b-collect-imm) 1)))

;; collectE

(define (collectE-graph e)
  (define r1 e)
  (define r2 (collectE 'off (λ (x y) (if x y x)) r1))
  r2)

(define b-collect (synthesize #:forall (harvest off-stream)
                              #:guarantee (assert (same collectE-graph
                                                        sketch-graph1-1
                                                        off-stream))))
(if (unsat? b-collect)
    (displayln "!!!!! collectE graph not synthesized !!!!!")
    (begin (displayln "* collectE graph successfully synthesized")
           (print-from-holes (evaluate holes b-collect)
                             (evaluate retval-idx b-collect) 1)))

;; snapshotE

(define (snapshotE-graph e b)
  (define r1 e)
  (define r2 b)
  (define r3 (snapshotE r1 r2))
  r3)

(define b-snapshot (synthesize #:forall (harvest int-stream int-behavior)
                               #:guarantee (assert (same snapshotE-graph
                                                         sketch-graph1-2
                                                         int-stream int-behavior))))
(if (unsat? b-snapshot)
    (displayln "!!!!! snapshotE graph not synthesized !!!!!")
    (begin (displayln "* snapshotE graph successfully synthesized")
           (print-from-holes (evaluate holes b-snapshot)
                             (evaluate retval-idx b-snapshot) 2)))

;; startsWith

(define (startsWith-graph e)
  (define r1 e)
  (define r2 (startsWith #f e))
  r2)

(define b-startsWith (synthesize #:forall (harvest int-stream)
                                 #:guarantee (assert (same startsWith-graph
                                                           sketch-graph1-1
                                                           int-stream))))
(if (unsat? b-startsWith)
    (displayln "!!!!! startsWith graph not synthesized !!!!!")
    (begin (displayln "* startsWith graph successfully synthesized")
           (print-from-holes (evaluate holes b-startsWith)
                             (evaluate retval-idx b-startsWith) 1)))

;; startsWith-imm

(define (startsWith-imm-graph e)
  (define r1 e)
  (define r2 (startsWith 1 e))
  r2)

(define b-startsWith-imm (synthesize #:forall (harvest int-stream)
                                     #:guarantee (assert (same startsWith-imm-graph
                                                               sketch-graph1-1
                                                               int-stream))))
(if (unsat? b-startsWith-imm)
    (displayln "!!!!! startsWith-imm graph not synthesized !!!!!")
    (begin (displayln "* startsWith-imm graph successfully synthesized")
           (print-from-holes (evaluate holes b-startsWith-imm)
                             (evaluate retval-idx b-startsWith-imm) 1)))

;; mapE

(define (mapE-graph e)
  (define r1 e)
  (define r2 (mapE (λ (x) (+ x 5)) e))
  r2)

(define b-map (synthesize #:forall (harvest int-stream)
                          #:guarantee (assert (same mapE-graph
                                                    sketch-graph1-1
                                                    int-stream))))
(if (unsat? b-map)
    (displayln "!!!!! mapE graph not synthesized !!!!!")
    (begin (displayln "* mapE graph successfully synthesized")
           (print-from-holes (evaluate holes b-map)
                             (evaluate retval-idx b-map) 1)))

;; mapE2

(define (mapE2-graph e1 e2)
  (define r1 e1)
  (define r2 e2)
  (define r3 (mapE2 (λ (x y) (if x y 'no-evt)) r1 r2))
  r3)

(define b-map2 (synthesize #:forall (harvest int-stream int-stream2)
                           #:guarantee (assert (same mapE2-graph
                                                     sketch-graph1-2
                                                     int-stream int-stream2))))
(if (unsat? b-map2)
    (displayln "!!!!! mapE2 graph not synthesized !!!!!")
    (begin (displayln "* mapE2 graph successfully synthesized")
           (print-from-holes (evaluate holes b-map2)
                             (evaluate retval-idx b-map2) 2)))

;; liftB1

(define (liftB1-graph b)
  (define r1 b)
  (define r2 (liftB1 (λ (t) (<= t 2)) b))
  r2)

(define b-liftB1 (synthesize #:forall (harvest int-behavior)
                             #:guarantee (assert (same liftB1-graph
                                                       sketch-graph1-1
                                                       int-behavior))))
(if (unsat? b-liftB1)
    (displayln "!!!!! liftB1 graph not synthesized !!!!!")
    (begin (displayln "* liftB1 graph successfully synthesized")
           (print-from-holes (evaluate holes b-liftB1)
                             (evaluate retval-idx b-liftB1) 1)))

;; liftB2

(define (liftB2-graph b1 b2)
  (define r1 b1)
  (define r2 b2)
  (define r3 (liftB2 (λ (elt1 elt2) (+ elt1 elt2)) r1 r2))
  r3)

(define b-liftB2 (synthesize #:forall (append (harvest int-behavior) (harvest int-behavior2))
                             #:guarantee (assert (same liftB2-graph
                                                       sketch-graph1-2
                                                       int-behavior int-behavior2))))
(if (unsat? b-liftB2)
    (displayln "!!!!! liftB2 graph not synthesized !!!!!")
    (begin (displayln "* liftB2 graph successfully synthesized")
           (print-from-holes (evaluate holes b-liftB2)
                             (evaluate retval-idx b-liftB2) 2)))

;; andB

(define (andB-graph b1 b2)
  (define r1 b1)
  (define r2 b2)
  (define r3 (andB b1 b2))
  r3)

(define b-andB (synthesize #:forall (harvest bool-behavior bool-behavior2)
                           #:guarantee (assert (same andB-graph
                                                     sketch-graph1-2
                                                     bool-behavior bool-behavior2))))
(if (unsat? b-andB)
    (displayln "!!!!! andB graph not synthesized !!!!!")
    (begin (displayln "* andB graph successfully synthesized")
           (print-from-holes (evaluate holes b-andB)
                             (evaluate retval-idx b-andB) 2)))

;; ifB

(define (ifB-graph b1 b2 b3)
  (define r1 b1)
  (define r2 b2)
  (define r3 b3)
  (define r4 (ifB r1 r2 r3))
  r4)

(define b-ifB (synthesize #:forall (harvest bool-behavior int-behavior int-behavior2)
                          #:guarantee (assert (same ifB-graph
                                                    sketch-graph1-3
                                                    bool-behavior int-behavior int-behavior2))))

(if (unsat? b-ifB)
    (displayln "!!!!! ifB graph not synthesized !!!!!")
    (begin (displayln "* ifB graph successfully synthesized")
           (print-from-holes (evaluate holes b-ifB)
                             (evaluate retval-idx b-ifB) 3)))

;; constantB

(define (constantB-graph b1)
  (define r1 b1)
  (define r2 (constantB 'test b1))
  r2)

(define b-constantB (synthesize #:forall (harvest int-behavior)
                                #:guarantee (assert (same constantB-graph
                                                          sketch-graph1-1
                                                          int-behavior))))

(if (unsat? b-constantB)
    (displayln "!!!!! constantB graph not synthesized !!!!!")
    (begin (displayln "* constantB graph successfully synthesized")
           (print-from-holes (evaluate holes b-constantB)
                             (evaluate retval-idx b-constantB) 1)))

;; constantB-imm

(define (constantB-imm-graph b1)
  (define r1 b1)
  (define r2 (constantB 1 b1))
  r2)

(define b-constantB-imm (synthesize #:forall (harvest int-behavior)
                                    #:guarantee (assert (same constantB-imm-graph
                                                              sketch-graph1-1
                                                              int-behavior))))
(if (unsat? b-constantB-imm)
    (displayln "!!!!! constantB-imm graph not synthesized !!!!!")
    (begin (displayln "* constantB-imm graph successfully synthesized")
           (print-from-holes (evaluate holes b-constantB-imm)
                             (evaluate retval-idx b-constantB-imm) 1)))

;; collectB

(define (collectB-graph b1)
  (define r1 b1)
  (define r2 (collectB 'on (λ (x y) (if x y x)) b1))
  r2)

(define b-collectB (synthesize #:forall (harvest int-behavior)
                               #:guarantee (assert (same collectB-graph
                                                         sketch-graph1-1
                                                         int-behavior))))
(if (unsat? b-collectB)
    (displayln "!!!!! collectB graph not synthesized !!!!!")
    (begin (displayln "* collectB graph successfully synthesized")
           (print-from-holes (evaluate holes b-collectB)
                             (evaluate retval-idx b-collectB) 1)))

;; collectB-imm

(define (collectB-imm-graph b1)
  (define r1 b1)
  (define r2 (collectB 0 + b1))
  r2)

(define b-collectB-imm (synthesize #:forall (harvest int-behavior)
                                   #:guarantee (assert (same collectB-imm-graph
                                                             sketch-graph1-1
                                                             int-behavior))))
(if (unsat? b-collectB-imm)
    (displayln "!!!!! collectB-imm graph not synthesized !!!!!")
    (begin (displayln "* collectB-imm graph successfully synthesized")
           (print-from-holes (evaluate holes b-collectB-imm)
                             (evaluate retval-idx b-collectB-imm) 1)))