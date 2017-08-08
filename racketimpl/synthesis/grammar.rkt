#lang rosette/safe

(require rosette/lib/synthax)
(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")

(provide (all-defined-out))

(define  (same program1 program2 . inputs)
  (assert (equal? (apply program1 inputs)
                  (apply program2 inputs))))

(define (harvest-term v)
  (cond [(vector? v) (vector->list v)]
        [(and (union? v) (eq? 2 (length (union-contents v)))) (car (first (union-contents v)))]
        [(term? v) v]))

(define (harvest-events evt-stream)
  (flatten
  (append (map get-timestamp evt-stream)
          (map harvest-term (map get-value evt-stream)))))

(define (harvest-behavior b)
  (append (list (harvest-term (behavior-init b))) (harvest-events (behavior-changes b))))

(define-synthax (flapjax-grmr input-stream depth)
  #:base input-stream
  #:else (choose input-stream
                 (startsWith (flapjax-grmr input-stream (sub1 depth)) (choose 0 1 2 -1))
                 (collectE (flapjax-grmr input-stream (sub1 depth)) 0 +)
                 (mapE (λ (e) (list (get-timestamp e) (+ (get-value e) (choose 1 2 3))))
                       (flapjax-grmr input-stream (sub1 depth)))
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

(define-synthax (flapjax-grmrB input-behavior depth)
  #:base input-behavior
  #:else (choose input-behavior
                 (liftB (λ (e) (if e 'on 'off)) (flapjax-grmrB input-behavior (sub1 depth)))
                 ))

;; there's probably a more elegant way
(define-synthax (flapjax-grmrB2 inputB1 inputB2 depth)
  #:base (choose inputB1 inputB2)
  #:else (choose inputB1 inputB2
                 (liftB (choose (λ (clock location) (if (or (>= (time-vec->integer clock) 2130) (< (time-vec->integer clock) 800))
                                 'night
                                 (if (equal? location 'home)
                                     'home
                                     'away)))
                                (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none)))
                                (flapjax-grmrB2 inputB1 inputB2 (sub1 depth)) (flapjax-grmrB2 inputB1 inputB2 (sub1 depth)))
                 (liftB (λ (e) (if e 'on 'off)) (flapjax-grmrB2 inputB1 inputB2 (sub1 depth)))
))

(define-synthax (flapjax-grmrB3 inputB1 inputB2 inputB3 depth)
  #:base (choose inputB1 inputB2 inputB3)
  #:else (choose inputB1 inputB2 inputB3
                 (liftB (choose (λ (clock location) (if (or (>= (time-vec->integer clock) 2130) (< (time-vec->integer clock) 800))
                                 'night
                                 (if (equal? location 'home)
                                     'home
                                     'away)))
                                (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none)))
                                (flapjax-grmrB3 inputB1 inputB2 inputB3 (sub1 depth)) (flapjax-grmrB3 inputB1 inputB2 inputB3 (sub1 depth)))
                 (liftB (λ (e) (if e 'on 'off)) (flapjax-grmrB3 inputB1 inputB2 inputB3 (sub1 depth)))
))

(define-synthax (integer-constants depth)
  #:base (choose 1 2 3)
  #:else (integer-constants (sub1 depth)))