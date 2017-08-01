#lang rosette/safe

(require rosette/lib/synthax)
(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")

(provide (all-defined-out))

(define (same program1 program2 input-stream)
  (assert
   (equal? (program1 input-stream) (program2 input-stream))))

(define (same2 program1 program2 input1 input2)
  (assert (equal? (program1 input1 input2)
                  (program2 input1 input2))))

(define-synthax (flapjax-grmr input-stream depth)
  #:base input-stream
  #:else (choose input-stream
                 (startsWith (flapjax-grmr input-stream (sub1 depth)) (choose 0 1 2 -1))
                 (collectE (flapjax-grmr input-stream (sub1 depth)) 0 +)
                ; (mapE (λ (e) (list (get-timestamp e) (+ (get-value e) (choose 1 2 3))))
                ;       (flapjax-grmr input-stream (sub1 depth)))
                 (constantE (flapjax-grmr input-stream (sub1 depth)) 1)
                 (delayE (flapjax-grmr input-stream (sub1 depth)) (choose 1 2 3))))

(define-synthax (flapjax-grmr1 input-stream1 input-stream2 depth)
  #:base (choose input-stream1 input-stream2)
  #:else (choose input-stream1
                 input-stream2
                 (collectE (flapjax-grmr1 input-stream1 input-stream2 (sub1 depth)) 0 +)
                ; (mapE (λ (e) (list (get-timestamp e) (+ (get-value e) (choose 1 2 3))))
                ;       (flapjax-grmr1 input-stream1 input-stream2 (sub1 depth)))
                 (constantE (flapjax-grmr1 input-stream1 input-stream2 (sub1 depth)) (choose -1 1))
                ; (delayE (flapjax-grmr1 input-stream1 input-stream2 (sub1 depth)) (choose 1 2 3))
                 (startsWith (flapjax-grmr1 input-stream1 input-stream2 (sub1 depth)) 0)
                 (mergeE (flapjax-grmr1 input-stream1 input-stream2 (sub1 depth))
                         (flapjax-grmr1 input-stream1 input-stream2 (sub1 depth)))
                 ))

(define-synthax (integer-constants depth)
  #:base (choose 1 2 3)
  #:else (integer-constants (sub1 depth)))