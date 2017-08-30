#lang rosette/safe

(require rosette/lib/synthax)
(require rackunit)
(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")
(require "grammar.rkt")

(define (synthesize-graph spec-graph input)
  (define (synth-graph input)
    (flapjax-grmr input 1))
  (synthesize #:forall (harvest input)
              #:guarantee (same spec-graph synth-graph input)))

(define (synthesize-graph2 spec-graph input1 input2)
  (define (synth-graph input1 input2)
    (flapjax-grmr input1 input2 1))
  (synthesize #:forall (append (harvest input1) (harvest input2))
              #:guarantee (same spec-graph synth-graph input1 input2)))

(define (synthesize-graph3 spec-graph input1 input2 input3)
  (define (synth-graph input1 input2 input3)
    (flapjax-grmr input1 input2 input3 1))
  (synthesize #:forall (append (harvest input1) (harvest input2) (harvest input3))
              #:guarantee (same spec-graph synth-graph input1 input2 input3)))

(define s-integer-evts (new-event-stream sym-integer 3))
(define s-integer-evts2 (new-event-stream sym-integer 3))
(define s-integer-behavior (new-behavior sym-integer 3))
(define s-integer-behavior2 (new-behavior sym-integer 3))
(define s-boolean-evts (new-event-stream sym-boolean 3))
(define s-boolean-behavior (new-behavior sym-boolean 3))
(define s-boolean-behavior2 (new-behavior sym-boolean 3))
(define s-stream-of-streams (new-event-stream (λ () (new-event-stream sym-integer 2)) 3))
(define s-light-behavior (new-behavior (sym-union-constructor 'on 'off) 3))
(define s-mode-behavior (new-behavior (sym-union-constructor 'night 'home) 3))

(define (zeroE-graph input)
  (zeroE))

(check-true (not (unsat? (synthesize-graph zeroE-graph s-integer-evts))))

(define (constantB-graph input)
  (constantB 1))

(check-true (not (unsat? (synthesize-graph constantB-graph s-integer-behavior))))

(define (oneE-graph input)
  (oneE input))

(check-true (not (unsat? (synthesize-graph oneE-graph s-integer-behavior))))

(define (switchE-graph input)
  (switchE input))

(check-true (not (unsat? (synthesize-graph switchE-graph s-stream-of-streams))))

(define (notE-graph input)
  (notE input))

(check-true (not (unsat? (synthesize-graph notE-graph s-boolean-evts))))

(define (changes-graph input)
  (changes input))

(check-true (not (unsat? (synthesize-graph changes-graph s-integer-behavior))))

(define (startsWith-graph input)
  (startsWith 1 input))

(check-true (not (unsat? (synthesize-graph startsWith-graph s-integer-evts))))

(define (constantE-graph input)
  (constantE 0 input))

(check-true (not (unsat? (synthesize-graph constantE-graph s-boolean-evts))))

(define (delayE-graph input)
  (delayE 3 input))

(check-true (not (unsat? (synthesize-graph delayE-graph s-integer-evts))))

(define (blindE-graph input)
  (blindE 3 input))

(check-true (not (unsat? (synthesize-graph blindE-graph s-integer-evts))))

(define (calmE-graph input)
  (calmE 3 input))

(check-true (not (unsat? (synthesize-graph calmE-graph s-integer-evts))))

(define (mapE-graph input)
  (mapE (λ (e) (list (get-timestamp e) (+ (get-value e) 3))) input))

(check-true (not (unsat? (synthesize-graph mapE-graph s-integer-evts))))

(define (filterE-graph input)
  (filterE (λ (t) (<= t 3)) input))

(check-true (not (unsat? (synthesize-graph filterE-graph s-integer-evts))))

(define (liftB-graph input)
  (liftB (λ (e) (if e 'on 'off)) input))

(check-true (not (unsat? (synthesize-graph liftB-graph s-boolean-behavior))))

(define (collectE-graph input)
  (collectE 0 + input))

(check-true (not (unsat? (synthesize-graph collectE-graph s-integer-evts))))


(define (andB-graph input1 input2)
  (andB input1 input2))

(check-true (not (unsat? (synthesize-graph2 andB-graph s-boolean-behavior s-boolean-behavior2))))

(define (mergeE-graph input1 input2)
  (mergeE input1 input2))

(check-true (not (unsat? (synthesize-graph2 mergeE-graph s-integer-evts s-integer-evts2))))

(define (snapshotE-graph input1 input2)
  (snapshotE input1 input2))

(check-true (not (unsat? (synthesize-graph2 snapshotE-graph s-boolean-evts s-integer-behavior))))

(define (liftB2-graph input1 input2)
  (liftB (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none)) input1 input2))

(check-true (not (unsat? (synthesize-graph2 liftB2-graph s-light-behavior s-mode-behavior))))

(define (ifE-graph input1 input2 input3)
  (ifE input1 input2 input3))

(check-true (not (unsat? (synthesize-graph3 ifE-graph s-boolean-evts s-integer-evts s-integer-evts2))))

(define (ifB-graph input1 input2 input3)
  (ifB input1 input2 input3))

(check-true (not (unsat? (synthesize-graph3 ifB-graph s-boolean-behavior s-integer-behavior s-integer-behavior2))))