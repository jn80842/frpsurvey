#lang rosette

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../straightline.rkt")

;; returns streams of all 0s
(define (program1 input)
  (define r1 input)
  (define r2 (constantE 0 r1))
  r2)

;; identity
(define (program2 input)
  (define r1 input)
  (define r2 (constantE 10 r1))
  r1)

(define non-distinguish-input (list 0 0 0))
(define non-distinguish-output (list 0 0 0))
(define distinguish-input (list 1 2 3))

(define holes (list (get-insn-holes)))
(define-symbolic* retval-idx integer?)

(define (holes-program1 input)
  (define r1 input)
  (define r2 (single-insn (list-ref holes 0) (list r1)))
  (list-ref (list r1 r2) retval-idx))

(define holes2 (list (get-insn-holes)))
(define-symbolic* retval-idx2 integer?)

(define (holes-program2 input)
  (define r1 input)
  (define r2 (single-insn (list-ref holes2 0) (list r1)))
  (list-ref (list r1 r2) retval-idx2))

(define (solved-holes-program1 input)
  (define r1 input)
  (define r2-holes (stream-insn 0 0 0 0 0))
  (define retval 1)
  (define r2 (single-insn r2-holes (list r1)))
  (list-ref (list r1 r2) retval))

(define (solved-holes-program2 input)
  (define r1 input)
  (define r2-holes (stream-insn 2 0 0 0 1)) ;; doesn't matter
  (define retval 0)
  (define r2 (single-insn r2-holes (list r1)))
  (list-ref (list r1 r2) retval))


(define b (synthesize #:forall '()
                      #:guarantee (assert (and (same holes-program1 holes-program2 non-distinguish-input)
                                               (not (same holes-program1 holes-program2 distinguish-input))))))

(if (unsat? b)
    (displayln "unsat")
    (begin (print-from-holes holes retval-idx b 1 1)
           (print-from-holes holes2 retval-idx2 b 1 1)))