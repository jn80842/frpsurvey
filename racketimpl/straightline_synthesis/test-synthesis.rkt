#lang rosette

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../straightline.rkt")

(define stream-length 3)

(define holes (list (get-insn-holes)))
(define-symbolic* retval-idx integer?)

(define int-stream (new-event-stream sym-integer stream-length))
(define int-stream2 (new-event-stream sym-integer stream-length))

(define int-behavior (new-behavior sym-integer stream-length))
(define int-behavior2 (new-behavior sym-integer stream-length))
(define bool-behavior (new-behavior sym-boolean stream-length))
(define bool-behavior2 (new-behavior sym-boolean stream-length))

(define (sketch-graph1-1 e)
  (define r1 e)
  (define r2 (single-insn (list-ref holes 0) (list r1)))
  (list-ref (list r1 r2) retval-idx))

(define (sketch-graph1-2 e1 e2)
  (define r1 e1)
  (define r2 e2)
  (define r3 (single-insn (list-ref holes 0) (list r1 r2)))
  (list-ref (list r1 r2 r3) retval-idx))

(define (sketch-graph1-3 e1 e2 e3)
  (define r1 e1)
  (define r2 e2)
  (define r3 e3)
  (define r4 (single-insn (list-ref holes 0) (list r1 r2 r3)))
  (list-ref (list r1 r2 r3 r4) retval-idx))

;; constantE

(define (constantE-graph e)
  (define r1 e)
  (define r2 (constantE 1 e))
  r2)

(define b (synthesize #:forall (harvest int-stream)
            #:guarantee (assert (same constantE-graph sketch-graph1-1 int-stream))))
(if (unsat? b)
    (displayln "!!!!! constantE graph not synthesized !!!!!")
    (begin (displayln "* constantE graph successfully synthesized")
           (print-from-holes holes retval-idx b 1 1)))

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
           (print-from-holes holes retval-idx b-merge 1 2)))

(clear-asserts!)

;; collectE

(define (collectE-graph e)
  (define r1 e)
  (define r2 (collectE 0 + e))
  r2)

(define b-collect (synthesize #:forall (harvest int-stream)
                              #:guarantee (assert (same collectE-graph
                                                        sketch-graph1-1
                                                        int-stream))))
(if (unsat? b-collect)
    (displayln "!!!!! collectE graph not synthesized !!!!!")
    (begin (displayln "* collectE graph successfully synthesized")
           (print-from-holes holes retval-idx b-collect 1 1)))

;; startsWith

(define (startsWith-graph e)
  (define r1 e)
  (define r2 (startsWith 1 e))
  r2)

(define b-startsWith (synthesize #:forall (harvest int-stream)
                                 #:guarantee (assert (same startsWith-graph
                                                           sketch-graph1-1
                                                           int-stream))))
(if (unsat? b-startsWith)
    (displayln "!!!!! startsWith graph not synthesized !!!!!")
    (begin (displayln "* startsWith graph successfully synthesized")
           (print-from-holes holes retval-idx b-startsWith 1 1)))

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
           (print-from-holes holes retval-idx b-map 1 1)))

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
           (print-from-holes holes retval-idx b-liftB1 1 1)))

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
           (print-from-holes holes retval-idx b-liftB2 1 2)))

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
           (print-from-holes holes retval-idx b-andB 1 2)))

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
           (print-from-holes holes retval-idx b-ifB 1 3)))

;; constantB

(define (constantB-graph b1)
  (define r1 b1)
  (define r2 (constantB 'on b1))
  r2)

(define b-constantB (synthesize #:forall (harvest int-behavior)
                                #:guarantee (assert (same constantB-graph
                                                          sketch-graph1-1
                                                          int-behavior))))

(if (unsat? b-constantB)
    (displayln "!!!!! constantB graph not synthesized !!!!!")
    (begin (displayln "* constantB graph successfully synthesized")
           (print-from-holes holes retval-idx b-constantB 1 1)))