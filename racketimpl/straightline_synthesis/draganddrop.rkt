#lang rosette

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../straightline.rkt")
(require "../benchmarks/draganddrop.rkt")

(current-bitwidth #f)

(define (straightline-graph mouse-up mouse-down mouse-pos)
  (define r1 mouse-up)
  (define r2 mouse-down)
  (define r3 mouse-pos)
  (define r4 (constantE #f r1))
  (define r5 (constantE #t r2))
  (define r6 (mergeE r4 r5))
  (define r7 (startsWith #f r6))
  (define r8 (snapshotE r3 r7))
  (define r9 (constantE 'no-evt r1))
  (define r10 (ifE r8 r3 r9))
  r10)

(define (straightline2-graph mouse-up mouse-down mouse-pos)
  (define r1 mouse-up)
  (define r2 mouse-down)
  (define r3 mouse-pos)
  (define r4 (constantE #f r2))
  (define r5 (mergeE r1 r4))
  (define r6 (startsWith #t r5))
  (define r7 (snapshotE r3 r6))
  (define r8 (ifE r7 r4 r3))
  r8)

(displayln "drag and drop benchmark")

#;(define v-binding (verify (assert (same drag-and-drop-graph
                                        straightline-graph
                                        s-mouse-up s-mouse-down s-mouse-pos))))

#;(if (unsat? v-binding)
    (displayln "verified example implementation and straightline program are equivalent")
    (displayln "can't verify that straightline program matches example implementation"))

(define state-mask (list->vector (list #f #f #t #f #f)))
(define ddsketch (sketchfields 5 3 state-mask))

(synth-ref-impl ddsketch straightline-graph s-mouse-up s-mouse-down s-mouse-pos)

(define simple-spec (io-specs (list '(no-evt no-evt click)
                                   '(click no-evt no-evt)
                                   (list (coords 1 1) (coords 2 2) (coords 3 3)))
                             (list (coords 1 1) (coords 2 2) 'no-evt)))
(define no-clicks-spec (io-specs (list '(no-evt no-evt no-evt)
                                  '(no-evt no-evt no-evt)
                                  (list (coords 1 1) (coords 2 2) (coords 3 3)))
                            '(no-evt no-evt no-evt)))



(io-specs-satisfiable? ddsketch (list simple-spec no-clicks-spec))
(io-specs-unique-program? ddsketch (list simple-spec no-clicks-spec))
