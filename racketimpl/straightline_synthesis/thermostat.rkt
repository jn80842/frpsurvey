#lang rosette
(require rosette/lib/synthax)

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../straightline.rkt")
(require "../benchmarks/thermostat.rkt")

(define (straightline-thermostat-graph tempB clockB)
  (define r1 tempB)
  (define r2 clockB)
  (define r3 (liftB1 (λ (t) (<= t 2)) r1))
  (define r4 (liftB1 (λ (c) (or (>= c 4) (>= 2 c))) r2))
  (define r5 (andB r3 r4))
  (define r6 (constantB 'on '()))
  (define r7 (constantB 'off '()))
  (define r8 (ifB r5 r6 r7))
  r5)

(define (holes-style-graph tempB clockB)
  (define r1 tempB)
  (define r2 clockB)
  (define r3-holes (stream-insn 5 0 1 0 0))
  (define r3 (single-insn r3-holes (list r1 r2)))
  (define r4-holes (stream-insn 5 1 2 0 0))
  (define r4 (single-insn r4-holes (list r1 r2 r3)))
  (define r5-holes (stream-insn 6 2 3 0 0))
  (single-insn r5-holes (list r1 r2 r3 r4)))

(define sol (verify (assert (same straightline-thermostat-graph
                      holes-style-graph
                      s-tempB s-clockB))))

;; pass input streams here as args
(define (holes-based-synthesis depth input-count)
  (define holes-structure (for/list ([i (range depth)])
                            (get-insn-holes)))
  (define-symbolic* retval-idx integer?)
  ;; need to generate this 
  (define (sketch-graph input1 input2)
    (define r1 input1)
    (define r2 input2)
   ; (define r3 (liftB1 (λ (t) (<= t 2)) r1))
   ; (define r4 (liftB1 (λ (c) (or (>= c 4) (>= 2 c))) r2))
    (define r3 (single-insn (list-ref holes-structure 0) (list r1 r2)))
    (define r4 (single-insn (list-ref holes-structure 1) (list r1 r2 r3)))
    (define r5 (single-insn (list-ref holes-structure 2) (list r1 r2 r3 r4)))
   ; (define r6 (single-insn (list-ref holes-structure 3) (list r1 r2 r3 r4 r5)))
   ; (define r7 (single-insn (list-ref holes-structure 4) (list r1 r2 r3 r4 r5 r6)))
   ; (define r8 (single-insn (list-ref holes-structure 5) (list r1 r2 r3 r4 r5 r6 r7)))
    (list-ref (list r1 r2 r3 r4 r5) retval-idx))
  (define binding (synthesize #:forall (append (harvest s-tempB) (harvest s-clockB))
                              #:guarantee (assert (same straightline-thermostat-graph
                                                        sketch-graph
                                                        s-tempB s-clockB))))
  (if (unsat? binding)
      "unsat"
      (print-from-holes holes-structure retval-idx binding depth input-count)))


#;(define (fully-expanded-sketch-graph tempB clockB)
  (define r1 tempB)
  (define r2 clockB)
  (define r3 (choose (constantB (choose 'on 'off))
                     ((choose (curry andB (list-ref (list r1 r2) (choose 0 1)))
                              (curry orB (list-ref (list r1 r2) (choose 0 1)))
                              notB
                              (curry liftB1 (choose (λ (t) (<= t temp-floor))
                                                    (λ (c) (or (>= c 4) (>= 2 c)))))
                      ;; liftB2
                              (curry ifB (list-ref (list r1 r2) (choose 0 1)) (list-ref (list r1 r2) (choose 0 1))))
                      (list-ref (list r1 r2) (choose 0 1)))))
  (define r4 (choose (constantB (choose 'on 'off))
                     ((choose (curry andB (list-ref (list r1 r2 r3) (choose 0 1 2)))
                              (curry orB (list-ref (list r1 r2 r3) (choose 0 1 2)))
                              notB
                              (curry liftB1 (choose (λ (t) (<= t temp-floor))
                                                    (λ (c) (or (>= c 4) (>= 2 c)))))
                      ;; liftB2
                              (curry ifB (list-ref (list r1 r2 r3) (choose 0 1 2)) (list-ref (list r1 r2 r3) (choose 0 1 2))))
                      (list-ref (list r1 r2 r3) (choose 0 1 2)))))
  (define r5 (choose (constantB (choose 'on 'off))
                     ((choose (curry andB (list-ref (list r1 r2 r3 r4) (choose 0 1 2 3)))
                              (curry orB (list-ref (list r1 r2 r3 r4) (choose 0 1 2 3)))
                              notB
                              (curry liftB1 (choose (λ (t) (<= t temp-floor))
                                                    (λ (c) (or (>= c 4) (>= 2 c)))))
                      ;; liftB2
                              (curry ifB (list-ref (list r1 r2 r3 r4) (choose 0 1 2 3)) (list-ref (list r1 r2 r3 r4) (choose 0 1 2 3))))
                      (list-ref (list r1 r2 r3 r4) (choose 0 1 2 3)))))
  (define r6 (choose (constantB (choose 'on 'off))
                     ((choose (curry andB (list-ref (list r1 r2 r3 r4 r5) (choose 0 1 2 3 4)))
                      (curry orB (list-ref (list r1 r2 r3 r4 r5) (choose 0 1 2 3 4)))
                      notB
                      (curry liftB1 (choose (λ (t) (<= t temp-floor))
                                           (λ (c) (or (>= c 4) (>= 2 c)))))
                      ;; liftB2
                      (curry ifB (list-ref (list r1 r2 r3 r4 r5) (choose 0 1 2 3 4)) (list-ref (list r1 r2 r3 r4 r5) (choose 0 1 2 3 4))))
              (list-ref (list r1 r2 r3 r4 r5) (choose 0 1 2 3 4)))))
  (define r7 (choose (constantB (choose 'on 'off))
                     ((choose (curry andB (list-ref (list r1 r2 r3 r4 r5 r6) (choose 0 1 2 3 4 5)))
                              (curry orB (list-ref (list r1 r2 r3 r4 r5 r6) (choose 0 1 2 3 4 5)))
                              notB
                              (curry liftB1 (choose (λ (t) (<= t temp-floor))
                                                    (λ (c) (or (>= c 4) (>= 2 c)))))
                      ;; liftB2
                              (curry ifB (list-ref (list r1 r2 r3 r4 r5 r6) (choose 0 1 2 3 4 5)) (list-ref (list r1 r2 r3 r4 r5 r6) (choose 0 1 2 3 4 5))))
                     (list-ref (list r1 r2 r3 r4 r5 r6) (choose 0 1 2 3 4 5)))))
  (define r8 (choose (constantB (choose 'on 'off))
                     (ifB r5 r6 r7)
                     ((choose (curry andB (list-ref (list r1 r2 r3 r4 r5 r6 r7) (choose 0 1 2 3 4 5 6)))
                              (curry orB (list-ref (list r1 r2 r3 r4 r5 r6 r7) (choose 0 1 2 3 4 5 6)))
                              notB
                              (curry liftB1 (choose (λ (t) (<= t temp-floor))
                                                    (λ (c) (or (>= c 4) (>= 2 c)))))
                      ;; liftB2
                              (curry ifB (list-ref (list r1 r2 r3 r4 r5 r6 r7) (choose 0 1 2 3 4 5 6)) (list-ref (list r1 r2 r3 r4 r5 r6 r7) (choose 0 1 2 3 4 5 6))))
                     (list-ref (list r1 r2 r3 r4 r5 r6 r7) (choose 0 1 2 3 4 5 6)))))
  r5)

#;(define binding
  (time (synthesize #:forall (append (harvest s-tempB) (harvest s-clockB))
                    #:guarantee (assert (same straightline-thermostat-graph fully-expanded-sketch-graph s-tempB s-clockB)))))

;;(function-printer binding)