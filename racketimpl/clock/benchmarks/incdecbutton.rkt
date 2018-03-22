#lang rosette

(require "../fjmodel.rkt")
(require "../fjapi.rkt")
(require "../instruction.rkt")

(error-print-width 100000000000)
(current-bitwidth #f)

;;         startsWith
;;          /       \
;;         0       collectE
;;                  /  |  \
;;                λ    0  mergeE
;;                       /      \
;;                 constantE constantE
;;                  /     \    /     \
;;                 inc     1  dec    -1

(define stream-length 2)

(define concrete-inc '(no-evt click no-evt no-evt))
(define concrete-dec '(click no-evt no-evt click))
(define s-inc (new-event-stream (λ () 'click) stream-length))
(define s-dec (new-event-stream (λ () 'click) stream-length))
(define int-stream (new-event-stream get-sym-int 3))

#;(define (inc-dec-graph inc dec)
  (let* ([constant1 (map (curry constantE 1) inc)]
         [constant-1 (map (curry constantE -1) dec)]
         [merge (map mergeE constant1 constant-1)]
         [collect (for/list ([i (range (length merge))])
                    (collectE 0 + (take merge (add1 i))))])
    (startsWith 0 collect)))


#;(define (call-graph graph inc dec)
  (for/list ([i (range 1 (add1 (length inc)))])
    (graph (take inc i) (take dec i))))

(define (straightline-graph inc dec)
  (define r1 inc)
  (define r2 dec)
  (define r3 (constantE 1 r1))
  (define r4 (constantE -1 r2))
  (define r5 (mergeE r3 r4))
  (define r6 (collectE 0 + r5))
  (define r7 (startsWith 0 r6))
  r7)

(define holes (for/list ([i (range 5)])
                          (get-insn-holes)))
(define-symbolic* retval-idx integer?)

(define (sketch-graph input1 input2)
  (define r1 input1)
  (define r2 input2)
  (define r3 (call-insn (list-ref holes 0) (list r1 r2)))
  (define r4 (call-insn (list-ref holes 1) (list r1 r2 r3)))
  (define r5 (call-insn (list-ref holes 2) (list r1 r2 r3 r4)))
  (define r6 (call-insn (list-ref holes 3) (list r1 r2 r3 r4 r5)))
  (define r7 (call-insn (list-ref holes 4) (list r1 r2 r3 r4 r5 r6)))
  (list-ref (list r1 r2 r3 r4 r5 r6 r7) retval-idx))

(define (concrete-sketch-graph input1 input2)
  (define r1 input1)
  (define r2 input2)
  (define r3 (call-insn (insn #t 1 0 0 0 1) (list r1 r2)))
  (define r4 (call-insn (insn #t 1 1 0 0 -1) (list r1 r2 r3)))
  (define r5 (call-insn (insn #t 0 2 3 0 0) (list r1 r2 r3 r4)))
  (define r6 (call-insn (insn #t 7 4 0 1 0) (list r1 r2 r3 r4 r5)))
  (define r7 (call-insn (insn #f 11 5 0 0 0) (list r1 r2 r3 r4 r5 r6)))
  (list-ref (list r1 r2 r3 r4 r5 r6 r7) 6))

(define binding (time (synthesize #:forall (harvest s-inc s-dec)
                                  #:guarantee (assert (same sketch-graph
                                                            concrete-sketch-graph
                                                            s-inc s-dec)))))
(if (unsat? binding)
    (displayln "synthesis model is unsat")
    (displayln "synthesis model is sat"))
