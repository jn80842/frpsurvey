#lang rosette

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../operators.rkt")
(require "../sketch.rkt")
(require "../specifications.rkt")
(require "../properties.rkt")

(current-bitwidth #f)

(define stream-length 10)
(define (sym-coords)
  (define-symbolic* x integer?)
  (define-symbolic* y integer?)
  (coords x y))

(define (harvest-coords-stream clist)
  (apply append (map harvest-coords clist)))

(define s-mouse-up (new-event-stream (λ () 'click) stream-length))
(define s-mouse-down (new-event-stream (λ () 'click) stream-length))
(define s-mouse-pos (new-event-stream sym-coords stream-length))

(define (straightline-graph mouse-up mouse-down mouse-pos)
  (define r1 mouse-up)
  (define r2 mouse-down)
  (define r3 mouse-pos)
  (define r4 (constantE #f r1))
  (define r5 (constantE #t r2))
  (define r6 (mergeE r4 r5))
  (define r7 (startsWith #f r6))
  (define r8 (snapshotE r3 r7))
  (define r9 (ifE r8 r3 r8))
  (define r10 (filterE (λ (x) x) r9))
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

(define dd-sketch (sketch (get-holes-list 5) state-mask
                          stateless-operator-list stateful-operator-list 3))

;(synth-from-ref-impl dd-sketch straightline-graph s-mouse-up s-mouse-down s-mouse-pos)

#;(define (verify-sketch ref-impl sk binding inputs)
  (begin (clear-asserts!)
         (let ([sk-phi (apply (get-bound-sketch-function sk binding) inputs)]
               [ref-phi (apply ref-impl inputs)])
           (begin (define m (verify (assert (equal? sk-phi ref-phi))))
                  (clear-asserts!)
                  (if (unsat? m)
                      (displayln "Synthesized function is equivalent to reference implementation")
                      (displayln "Synthesized function is NOT equivalent to reference implementation"))))))

(define (verify-sketch ref-impl sk binding inputs)
  (begin (clear-asserts!)
         (begin (define m (verify (assert (equal? (apply (get-bound-sketch-function sk binding) inputs)
                                                  (apply ref-impl inputs)))))
                  (clear-asserts!)
                  (if (unsat? m)
                      (displayln "Synthesized function is equivalent to reference implementation")
                      (displayln "Synthesized function is NOT equivalent to reference implementation")))))

(define (synth-for-benchmarks sk ref-impl inputs long-inputs)
  (begin
  (let ([evaled-sk (apply (get-sketch-function sk) inputs)]
        [evaled-ref (apply ref-impl inputs)])
    (begin (define binding (time (synthesize #:forall (apply harvest inputs)
                                             #:guarantee (assert (equal? evaled-sk evaled-ref)))))
           (clear-asserts!)
           (if (unsat? binding)
               (displayln "Cannot synthesize program that matches reference implementation")
               (begin (print-sketch sk binding)
                      (for ([i (range (length (sketch-holes sk)))])
                        (displayln (list-ref (evaluate (sketch-holes sk) binding) i)))
                      (verify-sketch ref-impl sk binding long-inputs)
                      ))))))

(define long-mouse-up (new-event-stream (λ () 'click) 10))
(define long-mouse-down (new-event-stream (λ () 'click) 10))
(define long-mouse-pos (new-event-stream sym-coords 10))

(define smol-mouse-up (new-event-stream (λ () 'click) 1))
(define smol-mouse-down (new-event-stream (λ () 'click) 1))
(define smol-mouse-pos (new-event-stream sym-coords 1))

(define (get-sketch-by-size i)
  (sketch (get-holes-list i) (list->vector (list #f #f #t #f #f #f #f #f #f #f))
                           stateless-operator-list stateful-operator-list 3))

#;(for ([n (range 10)])
  (begin
    (displayln (format "######### input length ~a #######" (add1 n)))
    (define stream-length (add1 n))
    (define s-mouse-up (new-event-stream (λ () 'click) stream-length))
    (define s-mouse-down (new-event-stream (λ () 'click) stream-length))
    (define s-mouse-pos (new-event-stream sym-coords stream-length))
    (for ([i (range 10)])
      (begin
        (displayln (format "insn count ~a" (add1 i)))
        (define sk (sketch (get-holes-list (add1 i)) (list->vector (list #f #f #t #f #f #f #f #f #f #f))
                           stateless-operator-list stateful-operator-list 3))
        (synth-for-benchmarks sk straightline-graph (list s-mouse-up s-mouse-down s-mouse-pos)
                              (list long-mouse-up long-mouse-down long-mouse-pos))))))
    