#lang rosette

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../straightline.rkt")

(current-bitwidth 6)

(define stream-length 5)

(define holes (list (get-insn-holes)))
(define-symbolic* retval-idx integer?)

(define int-stream (new-event-stream get-sym-int stream-length))
(define int-stream2 (new-event-stream get-sym-int stream-length))
(define off-stream (new-event-stream (λ () 'off) stream-length))

(define int-behavior (new-behavior get-sym-int stream-length))
(define int-behavior2 (new-behavior get-sym-int stream-length))
(define int-behavior3 (new-behavior get-sym-int stream-length))
(define bool-behavior (new-behavior get-sym-bool stream-length))
(define bool-behavior2 (new-behavior get-sym-bool stream-length))

;; constantE-imm

(define (constantE-imm-graph e)
  (define r1 e)
  (define r2 (constantE 1 e))
  r2)

(define b (synthesize #:forall (harvest int-stream)
            #:guarantee (assert (same constantE-imm-graph (recursive-sketch holes retval-idx (make-vector 1 #t))
                                      int-stream))))
(define b-stateless (synthesize #:forall (harvest int-stream)
            #:guarantee (assert (same constantE-imm-graph (recursive-sketch holes retval-idx (make-vector 1 #f)) int-stream))))
(if (or (unsat? b) (unsat? b-stateless))
    (displayln "!!!!! constantE-imm graph not synthesized !!!!!")
    (begin (displayln "* constantE-imm graph successfully synthesized")
           (print-from-holes (evaluate holes b) (make-vector 1 #t)
                             (evaluate retval-idx b) 1)
           (displayln "* constantE-imm graph (stateless) successfully synthesized")
           (print-from-holes (evaluate holes b-stateless) (make-vector 1 #f)
                             (evaluate retval-idx b-stateless) 1)))

;; constantE

(define (constantE-graph e)
  (define r1 e)
  (define r2 (constantE 'test e))
  r2)

(define b-constant (synthesize #:forall (harvest int-stream)
                               #:guarantee (assert (same constantE-graph
                                                         (recursive-sketch holes retval-idx (make-vector 1 #t))
                                                         int-stream))))
(define b-constant-stateless (synthesize #:forall (harvest int-stream)
                                         #:guarantee (assert (same constantE-graph
                                                                   (recursive-sketch holes retval-idx (make-vector 1 #f))
                                                                   int-stream))))
(if (or (unsat? b-constant) (unsat? b-constant-stateless))
    (displayln "!!!!! constantE graph not synthesized !!!!!")
    (begin (displayln "* constantE graph successfully synthesized")
           (print-from-holes (evaluate holes b-constant) (make-vector 1 #t)
                             (evaluate retval-idx b-constant) 1)
           (displayln "* constantE graph (stateless) successfully synthesized")
           (print-from-holes (evaluate holes b-constant-stateless) (make-vector 1 #f)
                             (evaluate retval-idx b-constant-stateless) 1)))

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
                            #:guarantee (assert (same mergeE-graph (recursive-sketch holes retval-idx (make-vector 1 #t))
                                                      int-stream int-stream2))))
(define b-merge-stateless (synthesize #:forall (harvest int-stream int-stream2)
                            #:guarantee (assert (same mergeE-graph (recursive-sketch holes retval-idx (make-vector 1 #f))
                                                      int-stream int-stream2))))
(if (or (unsat? b-merge) (unsat? b-merge-stateless))
    (displayln "!!!!! mergeE graph not synthesized !!!!!")
    (begin (displayln "* mergeE graph successfully synthesized")
           (print-from-holes (evaluate holes b-merge) (make-vector 1 #t)
                             (evaluate retval-idx b-merge) 2)
           (displayln "* mergeE graph (stateless) successfully synthesized")
           (print-from-holes (evaluate holes b-merge-stateless) (make-vector 1 #f)
                             (evaluate retval-idx b-merge-stateless) 2)))

(clear-asserts!)

;; collectE-imm

(define (collectE-imm-graph e)
  (define r1 e)
  (define r2 (collectE 0 + e))
  r2)

(define b-collect-imm (synthesize #:forall (harvest int-stream)
                              #:guarantee (assert (same collectE-imm-graph
                                                        (recursive-sketch holes retval-idx (make-vector 1 #t))
                                                        int-stream))))
(if (unsat? b-collect-imm)
    (displayln "!!!!! collectE-imm graph not synthesized !!!!!")
    (begin (displayln "* collectE-imm graph successfully synthesized")
           (print-from-holes (evaluate holes b-collect-imm) (make-vector 1 #t)
                             (evaluate retval-idx b-collect-imm) 1)))

;; collectE

(define (collectE-graph e)
  (define r1 e)
  (define r2 (collectE 'off (λ (x y) (if x y x)) r1))
  r2)

(define b-collect (synthesize #:forall (harvest off-stream)
                              #:guarantee (assert (same collectE-graph
                                                        (recursive-sketch holes retval-idx (make-vector 1 #t))
                                                        off-stream))))
(if (unsat? b-collect)
    (displayln "!!!!! collectE graph not synthesized !!!!!")
    (begin (displayln "* collectE graph successfully synthesized")
           (print-from-holes (evaluate holes b-collect) (make-vector 1 #t)
                             (evaluate retval-idx b-collect) 1)))

;; snapshotE

(define (snapshotE-graph e b)
  (define r1 e)
  (define r2 b)
  (define r3 (snapshotE r1 r2))
  r3)

(define b-snapshot (synthesize #:forall (harvest int-stream int-behavior)
                               #:guarantee (assert (same snapshotE-graph
                                                         (recursive-sketch holes retval-idx (make-vector 1 #t))
                                                         int-stream int-behavior))))
(define b-snapshot-stateless (synthesize #:forall (harvest int-stream int-behavior)
                               #:guarantee (assert (same snapshotE-graph
                                                         (recursive-sketch holes retval-idx (make-vector 1 #f))
                                                         int-stream int-behavior))))
(if (or (unsat? b-snapshot) (unsat? b-snapshot-stateless))
    (displayln "!!!!! snapshotE graph not synthesized !!!!!")
    (begin (displayln "* snapshotE graph successfully synthesized")
           (print-from-holes (evaluate holes b-snapshot) (make-vector 1 #t)
                             (evaluate retval-idx b-snapshot) 2)
           (displayln "* snapshotE graph (stateless) successfully synthesized")
           (print-from-holes (evaluate holes b-snapshot-stateless) (make-vector 1 #f)
                             (evaluate retval-idx b-snapshot-stateless) 2)))

;; startsWith

(define (startsWith-graph e)
  (define r1 e)
  (define r2 (startsWith #f e))
  r2)

(define b-startsWith (synthesize #:forall (harvest int-stream)
                                 #:guarantee (assert (same startsWith-graph
                                                           (recursive-sketch holes retval-idx (make-vector 1 #t))
                                                           int-stream))))
(if (unsat? b-startsWith)
    (displayln "!!!!! startsWith graph not synthesized !!!!!")
    (begin (displayln "* startsWith graph successfully synthesized")
           (print-from-holes (evaluate holes b-startsWith) (make-vector 1 #t)
                             (evaluate retval-idx b-startsWith) 1)))

;; startsWith-imm

(define (startsWith-imm-graph e)
  (define r1 e)
  (define r2 (startsWith 1 e))
  r2)

(define b-startsWith-imm (synthesize #:forall (harvest int-stream)
                                     #:guarantee (assert (same startsWith-imm-graph
                                                               (recursive-sketch holes retval-idx (make-vector 1 #t))
                                                               int-stream))))
(if (unsat? b-startsWith-imm)
    (displayln "!!!!! startsWith-imm graph not synthesized !!!!!")
    (begin (displayln "* startsWith-imm graph successfully synthesized")
           (print-from-holes (evaluate holes b-startsWith-imm) (make-vector 1 #t)
                             (evaluate retval-idx b-startsWith-imm) 1)))

;; mapE

(define (mapE-graph e)
  (define r1 e)
  (define r2 (mapE (λ (x) (+ x 5)) e))
  r2)

(define b-map (synthesize #:forall (harvest int-stream)
                          #:guarantee (assert (same mapE-graph
                                                    (recursive-sketch holes retval-idx (make-vector 1 #t))
                                                    int-stream))))
(define b-map-stateless (synthesize #:forall (harvest int-stream)
                                    #:guarantee (assert (same mapE-graph
                                                              (recursive-sketch holes retval-idx (make-vector 1 #f))
                                                              int-stream))))
(if (or (unsat? b-map) (unsat? b-map-stateless))
    (displayln "!!!!! mapE graph not synthesized !!!!!")
    (begin (displayln "* mapE graph successfully synthesized")
           (print-from-holes (evaluate holes b-map) (make-vector 1 #t)
                             (evaluate retval-idx b-map) 1)
           (displayln "* mapE graph (stateless) successfully synthesized")
           (print-from-holes (evaluate holes b-map-stateless) (make-vector 1 #f)
                             (evaluate retval-idx b-map-stateless) 1)))

;; mapE2

(define (mapE2-graph e1 e2)
  (define r1 e1)
  (define r2 e2)
  (define r3 (mapE2 (λ (x y) (if x y 'no-evt)) r1 r2))
  r3)

(define b-map2 (synthesize #:forall (harvest int-stream int-stream2)
                           #:guarantee (assert (same mapE2-graph
                                                     (recursive-sketch holes retval-idx (make-vector 1 #t))
                                                     int-stream int-stream2))))
(define b-map2-stateless (synthesize #:forall (harvest int-stream int-stream2)
                                     #:guarantee (assert (same mapE2-graph
                                                               (recursive-sketch holes retval-idx (make-vector 1 #f))
                                                               int-stream int-stream2))))
(if (or (unsat? b-map2) (unsat? b-map2-stateless))
    (displayln "!!!!! mapE2 graph not synthesized !!!!!")
    (begin (displayln "* mapE2 graph successfully synthesized")
           (print-from-holes (evaluate holes b-map2) (make-vector 1 #t)
                             (evaluate retval-idx b-map2) 2)
           (displayln "* mapE2 graph (stateless) successfully synthesized")
           (print-from-holes (evaluate holes b-map2-stateless) (make-vector 1 #f)
                                       (evaluate retval-idx b-map2-stateless) 2)))

;; liftB1

(define (liftB1-graph b)
  (define r1 b)
  (define r2 (liftB1 (λ (t) (<= t 2)) b))
  r2)

(define b-liftB1 (synthesize #:forall (harvest int-behavior)
                             #:guarantee (assert (same liftB1-graph
                                                       (recursive-sketch holes retval-idx (make-vector 1 #t))
                                                       int-behavior))))
(define b-liftB1-stateless (synthesize #:forall (harvest int-behavior)
                                       #:guarantee (assert (same liftB1-graph
                                                                 (recursive-sketch holes retval-idx (make-vector 1 #f))
                                                                 int-behavior))))
(if (or (unsat? b-liftB1) (unsat? b-liftB1-stateless))
    (displayln "!!!!! liftB1 graph not synthesized !!!!!")
    (begin (displayln "* liftB1 graph successfully synthesized")
           (print-from-holes (evaluate holes b-liftB1) (make-vector 1 #t)
                             (evaluate retval-idx b-liftB1) 1)
           (displayln "* liftB1 graph (stateless) successfully synthesized")
           (print-from-holes (evaluate holes b-liftB1-stateless) (make-vector 1 #f)
                             (evaluate retval-idx b-liftB1-stateless) 2)))

;; liftB2

(define (liftB2-graph b1 b2)
  (define r1 b1)
  (define r2 b2)
  (define r3 (liftB2 (λ (elt1 elt2) (+ elt1 elt2)) r1 r2))
  r3)

(define b-liftB2 (synthesize #:forall (append (harvest int-behavior) (harvest int-behavior2))
                             #:guarantee (assert (same liftB2-graph
                                                       (recursive-sketch holes retval-idx (make-vector 1 #t))
                                                       int-behavior int-behavior2))))
(define b-liftB2-stateless (synthesize #:forall (append (harvest int-behavior) (harvest int-behavior2))
                                       #:guarantee (assert (same liftB2-graph
                                                                 (recursive-sketch holes retval-idx (make-vector 1 #f))
                                                                 int-behavior int-behavior2))))
(if (or (unsat? b-liftB2) (unsat? b-liftB2-stateless))
    (displayln "!!!!! liftB2 graph not synthesized !!!!!")
    (begin (displayln "* liftB2 graph successfully synthesized")
           (print-from-holes (evaluate holes b-liftB2) (make-vector 1 #t)
                             (evaluate retval-idx b-liftB2) 2)
           (displayln "* liftB2 graph (stateless) successfully synthesized")
           (print-from-holes (evaluate holes b-liftB2-stateless) (make-vector 1 #f)
                             (evaluate retval-idx b-liftB2-stateless) 2)))

;; andB

(define (andB-graph b1 b2)
  (define r1 b1)
  (define r2 b2)
  (define r3 (andB b1 b2))
  r3)

(define b-andB (synthesize #:forall (harvest bool-behavior bool-behavior2)
                           #:guarantee (assert (same andB-graph
                                                     (recursive-sketch holes retval-idx (make-vector 1 #t))
                                                     bool-behavior bool-behavior2))))
(define b-andB-stateless (synthesize #:forall (harvest bool-behavior bool-behavior2)
                                     #:guarantee (assert (same andB-graph
                                                               (recursive-sketch holes retval-idx (make-vector 1 #f))
                                                               bool-behavior bool-behavior2))))
(if (or (unsat? b-andB) (unsat? b-andB-stateless))
    (displayln "!!!!! andB graph not synthesized !!!!!")
    (begin (displayln "* andB graph successfully synthesized")
           (print-from-holes (evaluate holes b-andB) (make-vector 1 #t)
                             (evaluate retval-idx b-andB) 2)
           (displayln "* andB graph (stateless) successfully synthesized")
           (print-from-holes (evaluate holes b-andB-stateless) (make-vector 1 #f)
                             (evaluate retval-idx b-andB-stateless) 2)))

;; ifB

(define (ifB-graph b1 b2 b3)
  (define r1 b1)
  (define r2 b2)
  (define r3 b3)
  (define r4 (ifB r1 r2 r3))
  r4)

(define b-ifB (synthesize #:forall (harvest bool-behavior int-behavior int-behavior2)
                          #:guarantee (assert (same ifB-graph
                                                    (recursive-sketch holes retval-idx (make-vector 1 #t))
                                                    bool-behavior int-behavior int-behavior2))))
(define b-ifB-stateless (synthesize #:forall (harvest bool-behavior int-behavior int-behavior2)
                                    #:guarantee (assert (same ifB-graph
                                                              (recursive-sketch holes retval-idx (make-vector 1 #f))
                                                              bool-behavior int-behavior int-behavior2))))

(if (or (unsat? b-ifB) (unsat? b-ifB-stateless))
    (displayln "!!!!! ifB graph not synthesized !!!!!")
    (begin (displayln "* ifB graph successfully synthesized")
           (print-from-holes (evaluate holes b-ifB) (make-vector 1 #t)
                             (evaluate retval-idx b-ifB) 3)
           (displayln "* ifB graph (stateless) successfully synthesized")
           (print-from-holes (evaluate holes b-ifB-stateless) (make-vector 1 #f)
                             (evaluate retval-idx b-ifB-stateless) 3)))

;; constantB

(define (constantB-graph b1)
  (define r1 b1)
  (define r2 (constantB 'test b1))
  r2)

(define b-constantB (synthesize #:forall (harvest int-behavior)
                                #:guarantee (assert (same constantB-graph
                                                          (recursive-sketch holes retval-idx (make-vector 1 #t))
                                                          int-behavior))))
(define b-constantB-stateless (synthesize #:forall (harvest int-behavior)
                                          #:guarantee (assert (same constantB-graph
                                                                    (recursive-sketch holes retval-idx (make-vector 1 #f))
                                                                    int-behavior))))

(if (or (unsat? b-constantB) (unsat? b-constantB-stateless))
    (displayln "!!!!! constantB graph not synthesized !!!!!")
    (begin (displayln "* constantB graph successfully synthesized")
           (print-from-holes (evaluate holes b-constantB) (make-vector 1 #t)
                             (evaluate retval-idx b-constantB) 1)
           (displayln "* constantB graph (stateless) successfully synthesized")
           (print-from-holes (evaluate holes b-constantB-stateless) (make-vector 1 #f)
                             (evaluate retval-idx b-constantB-stateless) 1)))

;; constantB-imm

(define (constantB-imm-graph b1)
  (define r1 b1)
  (define r2 (constantB 1 b1))
  r2)

(define b-constantB-imm (synthesize #:forall (harvest int-behavior)
                                    #:guarantee (assert (same constantB-imm-graph
                                                              (recursive-sketch holes retval-idx (make-vector 1 #t))
                                                              int-behavior))))
(define b-constantB-imm-stateless (synthesize #:forall (harvest int-behavior)
                                              #:guarantee (assert (same constantB-imm-graph
                                                                        (recursive-sketch holes retval-idx (make-vector 1 #f))
                                                                        int-behavior))))

(if (or (unsat? b-constantB-imm) (unsat? b-constantB-imm-stateless))
    (displayln "!!!!! constantB-imm graph not synthesized !!!!!")
    (begin (displayln "* constantB-imm graph successfully synthesized")
           (print-from-holes (evaluate holes b-constantB-imm) (make-vector 1 #t)
                             (evaluate retval-idx b-constantB-imm) 1)
           (displayln "* constantB-imm graph (stateless) successfully synthesized")
           (print-from-holes (evaluate holes b-constantB-imm-stateless) (make-vector 1 #f)
                             (evaluate retval-idx b-constantB-imm-stateless) 1)))

;; collectB

(define (collectB-graph b1)
  (define r1 b1)
  (define r2 (collectB 'on (λ (x y) (if x y x)) b1))
  r2)

(define b-collectB (synthesize #:forall (harvest int-behavior)
                               #:guarantee (assert (same collectB-graph
                                                         (recursive-sketch holes retval-idx (make-vector 1 #t))
                                                         int-behavior))))
(if (unsat? b-collectB)
    (displayln "!!!!! collectB graph not synthesized !!!!!")
    (begin (displayln "* collectB graph successfully synthesized")
           (print-from-holes (evaluate holes b-collectB) (make-vector 1 #t)
                             (evaluate retval-idx b-collectB) 1)))

;; collectB-imm

(define (collectB-imm-graph b1)
  (define r1 b1)
  (define r2 (collectB 0 + b1))
  r2)

(define b-collectB-imm (synthesize #:forall (harvest int-behavior)
                                   #:guarantee (assert (same collectB-imm-graph
                                                             (recursive-sketch holes retval-idx (make-vector 1 #t))
                                                             int-behavior))))
(if (unsat? b-collectB-imm)
    (displayln "!!!!! collectB-imm graph not synthesized !!!!!")
    (begin (displayln "* collectB-imm graph successfully synthesized")
           (print-from-holes (evaluate holes b-collectB-imm) (make-vector 1 #t)
                             (evaluate retval-idx b-collectB-imm) 1)))