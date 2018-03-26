#lang rosette

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../straightline.rkt")
(require "../specifications.rkt")
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
(define ddsketchfields (sketchfields 5 3 state-mask))

;(synth-ref-impl ddsketch straightline-graph s-mouse-up s-mouse-down s-mouse-pos)

(define simple-spec (io-specs (list '(no-evt no-evt click)
                                   '(click no-evt no-evt)
                                   (list (coords 1 1) (coords 2 2) (coords 3 3)))
                             (list (coords 1 1) (coords 2 2) 'no-evt)))
(define no-clicks-spec (io-specs (list '(no-evt no-evt no-evt)
                                  '(no-evt no-evt no-evt)
                                  (list (coords 1 1) (coords 2 2) (coords 3 3)))
                            '(no-evt no-evt no-evt)))

(define sym-inputs-list (list (sym-input "mouse-up" s-mouse-up)
                              (sym-input "mouse-down" s-mouse-down)
                              (sym-input "mouse-pos" s-mouse-pos)))

(define (simultaneous-assertions mouse-up mouse-down mouse-pos)
  (andmap (λ (u d) (nand (not (empty-event? u)) (not (empty-event? d)))) mouse-up mouse-down))

(define simultaneous-invariant (input-invariant sym-inputs-list simultaneous-assertions))

(define coords-output-invariant (output-invariant sym-inputs-list (λ (o) (andmap (λ (e) (or (empty-event? e) (coords? e))) o))))

(define (alternate-up-down-checker mouse-up mouse-down mouse-pos)
  (letrec ([f (λ (pair lst) (cond [(empty? lst) #t]
                                  [(and (equal? (first pair) 'click)
                                        (equal? (first (first lst)) 'click)) #f]
                                  [(and (equal? (second pair) 'click)
                                        (equal? (second (first lst)) 'click)) #f]
                                  [(and (empty-event? (first (first lst)))
                                        (empty-event? (second (first lst)))) (f pair (rest lst))]
                                  [else (f (first lst) (rest lst))]))])
    (let ([pairs (map list mouse-up mouse-down)])
      (f (first pairs) (rest pairs)))))

(define alternate-up-down-invariant (input-invariant sym-inputs-list alternate-up-down-checker))

(define from-synth-spec1 (io-specs (list '(click no-evt no-evt click no-evt click)
                                         '(no-evt click no-evt no-evt click no-evt)
                                         (list 'no-evt 'no-evt 'no-evt 'no-evt 'no-evt (coords 0 0)))
                                   '(no-evt no-evt no-evt no-evt no-evt no-evt)))
(define from-synth-spec2 (io-specs (list '(no-evt no-evt click no-evt no-evt no-evt)
                                         '(no-evt click no-evt no-evt no-evt no-evt)
                                         (list (coords 2 -4) (coords 0 0) (coords 0 0) (coords -2 -3) 'no-evt 'no-evt))
                                   (list 'no-evt (coords 0 0) 'no-evt 'no-evt 'no-evt 'no-evt)))
(define from-synth-spec3 (io-specs (list '(click no-evt no-evt no-evt click no-evt)
                                         '(no-evt click no-evt no-evt no-evt no-evt)
                                         (list 'no-evt (coords 0 0) 'no-evt (coords 0 0) (coords 0 0) 'no-evt))
                                   (list 'no-evt (coords 0 0) 'no-evt (coords 0 0) 'no-evt 'no-evt)))

(specs-synthesis ddsketchfields (list simultaneous-invariant alternate-up-down-invariant
                                      coords-output-invariant
                                      simple-spec no-clicks-spec from-synth-spec1 from-synth-spec2 from-synth-spec3) sym-inputs-list)