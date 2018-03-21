#lang rosette

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../straightline.rkt")
(require "../benchmarks/incdecbutton.rkt")

(current-bitwidth #f)

(define (straightline-graph inc dec)
  (define r1 inc)
  (define r2 dec)
  (define r3 (constantE 1 r1))
  (define r4 (constantE -1 r2))
  (define r5 (mergeE r3 r4))
  (define r6 (collectE 0 + r5))
  (define r7 (startsWith 0 r6))
  r7)

(displayln "inc/dec button benchmark")

(define v-binding (verify (assert (same inc-dec-button-graph
                                        straightline-graph
                                        s-inc s-dec))))
(if (unsat? v-binding)
    (displayln "verified example implementation and straightline program are equivalent")
    (displayln "can't verify that straightline program matches example implementation"))

(define state-mask (list->vector (list #f #f #f #t #t)))
(define idbsketch-fields (sketchfields 5 2 state-mask))

(synth-ref-impl idbsketch-fields straightline-graph s-inc s-dec)

(define length-six-specs (io-specs (list '(no-evt click no-evt click no-evt no-evt no-evt)
                                         '(no-evt no-evt no-evt no-evt click click click))
                                   (behavior 0 '(0 1 1 2 1 0 -1))))

(displayln "Length six specs:")
(io-specs-satisfiable? idbsketch-fields (list length-six-specs))
(io-specs-unique-program? idbsketch-fields (list length-six-specs))

(define single-inc-click (io-specs (list '(click) '(no-evt))
                                   (behavior 0 '(-1))))
(define single-dec-click (io-specs (list '(no-evt) '(click))
                                   (behavior 0 '(1))))
(define two-dec-clicks (io-specs (list '(no-evt no-evt) '(click click))
                                 (behavior 0 '(-1 -2))))

(displayln "Two small specs:")
(io-specs-satisfiable? idbsketch-fields (list single-inc-click single-dec-click))
(io-specs-unique-program? idbsketch-fields (list single-inc-click single-dec-click))

(displayln "One length one, one length two specs:")
(io-specs-satisfiable? idbsketch-fields (list single-inc-click two-dec-clicks))
(io-specs-unique-program? idbsketch-fields (list single-inc-click two-dec-clicks))
