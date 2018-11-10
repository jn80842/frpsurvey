#lang rosette

(require "../model.rkt")
(require "../fjapi.rkt")
(require "../operators.rkt")
(require "../sketch.rkt")

(current-bitwidth #f)

;; inputs: clicks on + button and clicks on - button
;; output: counter

;; reference solution in straightline/register code style
(define (straightline-graph inc dec)
  (define r1 inc)
  (define r2 dec)
  (define r3 (constantE 1 r1))
  (define r4 (constantE -1 r2))
  (define r5 (mergeE r3 r4))
  (define r6 (collectE 0 + r5))
  (define r7 (startsWith 0 r6))
  r7)

;; symbolic inputs
(define stream-length 4)
(define sym-increment (new-event-stream (λ () 'click) stream-length))
(define sym-decrement (new-event-stream (λ () 'click) stream-length))

;; symbolic sketch
(define sk (get-symbolic-sketch 5 (list->vector '(#f #f #f #t #t)) 2))

;; synthesize against reference solution

(println "Synthesize function against reference implementation")

(define evaled-sk ((get-sketch-function sk) sym-increment sym-decrement))

(define b (time (synthesize #:forall (symbolics (list sym-increment sym-decrement))
                            #:guarantee (assert (equal? evaled-sk (straightline-graph sym-increment sym-decrement))))))

(if (unsat? b)
    (println "no solution for synthesis against reference implementation")
    (print-sketch sk b))

(clear-asserts!)

;; synthesize using input/output example

(println "Synthesize function against single input/output example")

(define ex1-increment '(no-evt click no-evt no-evt))
(define ex1-decrement '(click no-evt no-evt click))
(define ex1-counter (behavior 0 '(-1 0 0 -1)))

(define evaled-sk2 ((get-sketch-function sk) ex1-increment ex1-decrement))

(define b2 (time (synthesize #:forall '()
                             #:guarantee (assert (equal? evaled-sk2 ex1-counter)))))

(if (unsat? b2)
    (println "no solution for synthesis against single input/output example")
    (print-sketch sk b2))

(clear-asserts!)

;; synthesize using invariant over inputs and over output

(println "Synthesize function against invariants on inputs and output")

(define evaled-sk3 ((get-sketch-function sk) sym-increment sym-decrement))

;; assert that increment click and decrement click cannot occur at the same time
(assert (andmap (λ (i d) (or (empty-event? i) (empty-event? d))) sym-increment sym-decrement))

(define b3 (time (synthesize #:forall (symbolics (list sym-increment sym-decrement))
                             #:guarantee (assert (and (<= (list-ref (behavior-changes evaled-sk3) (sub1 stream-length)) 4)
                                                      (>= (list-ref (behavior-changes evaled-sk3) (sub1 stream-length)) -4))))))

(if (unsat? b3)
    (println "no solution for synthesis against invariants")
    (print-sketch sk b3))
