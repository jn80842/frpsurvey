#lang rosette/safe

(require rosette/lib/synthax)
(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")

(provide (all-defined-out))

(define (same program1 program2 input-stream)
  (assert
   (equal? (program1 input-stream) (program2 input-stream))))

(define-synthax (flapjax-grmr input-stream depth)
  #:base input-stream
  #:else (choose input-stream
                 (mapE (λ (e) (list (get-timestamp e) (+ (get-value e) (choose 1 2 3))))
                       (flapjax-grmr input-stream (sub1 depth)))
                 (constantE (flapjax-grmr input-stream (sub1 depth)) 1)
                 (delayE (flapjax-grmr input-stream (sub1 depth)) (choose 1 2 3))))

(define-synthax (flapjax-grmr2 input-stream1 input-stream2 depth)
  #:base (choose input-stream1 input-stream2)
  #:else (choose input-stream1
                 input-stream2
                 (mapE (λ (e) (list (get-timestamp e) (+ (get-value e) (choose 1 2 3))))
                       (flapjax-grmr input-stream (sub1 depth)))
                 (constantE (flapjax-grmr input-stream (sub1 depth)))
                 (delayE (flapjax-grmr input-stream (sub1 depth)) (choose 1 2 3))))

(define-synthax (integer-constants depth)
  #:base (choose 1 2 3)
  #:else (integer-constants (sub1 depth)))