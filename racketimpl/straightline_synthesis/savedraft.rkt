#lang rosette

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../straightline.rkt")
(require "../specifications.rkt")
(require "../benchmarks/savedraft.rkt")

(current-bitwidth #f)

(define (straightline-graph textChangedE saveButtonE)
  (define r1 textChangedE)
  (define r2 saveButtonE)
  (define r3 (timerE 5 r1))
  (define r4 (constantE #f r1))
  (define r5 (mergeE r3 r4))
  (define r6 (mergeE r2 r5))
  (define r7 (filterRepeatsE r6))
  (define r8 (filterE (λ (e) e) r7))
  r8)

(displayln "save draft benchmark")

(define v-binding (time (verify (assert (same saveCommand-graph
                                        straightline-graph
                                        s-textChangedE
                                        s-saveButtonE)))))
(if (unsat? v-binding)
    (displayln "verified example implementation and straightline program are equivalent")
    (displayln "can't verify that straightline program matches example implementation"))

(define state-mask (list->vector (list #f #t #f #f #t #f)))

(define sym-inputs-list (list (sym-input "textChangedE" s-textChangedE)
                              (sym-input "saveButtonE" s-saveButtonE)))

(define savedraftfields (sketchfields 6 2 state-mask))

;(synth-ref-impl savedraftfields straightline-graph s-textChangedE s-saveButtonE)

(define execution-spec (io-specs (list '(no-evt no-evt no-evt save no-evt no-evt no-evt no-evt no-evt no-evt)
                                       '(no-evt change change no-evt no-evt change change no-evt no-evt no-evt))
                                 '(no-evt change no-evt no-evt #t change no-evt no-evt no-evt #t)))

(define output-type (output-invariant sym-inputs-list (λ (output) (andmap (λ (e) (or (empty-event? e) (equal? e 'save))) output))))

(define (save-button-assertion-function textChangedE saveButtonE outputE)
  (andmap (λ (s o) (implies (not (empty-event? s)) (equal? o 'save))) saveButtonE outputE))

(define save-button-io-invariant (input-output-invariant sym-inputs-list save-button-assertion-function))

(specs-synthesis savedraftfields (list execution-spec output-type save-button-io-invariant) sym-inputs-list)