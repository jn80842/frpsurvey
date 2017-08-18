#lang rosette/safe

(require rosette/lib/synthax)
 (require rosette/lib/angelic)
(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")

(provide (all-defined-out))

(define  (same program1 program2 . inputs)
  (equal? (apply program1 inputs)
                  (apply program2 inputs)))

(define (harvest-term v)
  (cond [(vector? v) (vector->list v)]
        [(and (union? v) (eq? 2 (length (union-contents v)))) (car (first (union-contents v)))]
        [(term? v) v]))

(define (harvest-events evt-stream)
  (flatten
  (append (map get-timestamp evt-stream)
          (map harvest-term (map get-value evt-stream)))))

(define (harvest-behavior b)
  (flatten (append (list (harvest-term (behavior-init b))) (harvest-events (behavior-changes b)))))

(define (new-flapjaxE-grmr depth inputs)
  (let ([base (apply choose* (list inputs))])
    (if (= 0 depth)
        base
        (choose* base
                 (startsWith (new-flapjaxE-grmr (sub1 depth) inputs))
                 (collectE (new-flapjaxE-grmr (sub1 depth) inputs) 0 +)
                 (mapE (λ (e) (list (get-timestamp e) (+ (get-value e) (choose 1 2 3))))
                       (new-flapjaxE-grmr (sub1 depth) inputs))
                 (constantE (new-flapjaxE-grmr (sub1 depth) inputs) (choose 0 1 -1))
                 (delayE (new-flapjaxE-grmr (sub1 depth) inputs) (choose 1 2 3))))))

(define (small-grmr depth inputs)
  (let ([base (apply choose* (list inputs))])
    (if (= 0 depth)
        base
        (choose* base
                 (delayE (small-grmr (sub1 depth) inputs) (choose* 1 2 3))))))

(define-synthax (flapjaxE-grmr input-stream depth)
  #:base input-stream
  #:else (choose input-stream
                 (startsWith (flapjaxE-grmr input-stream (sub1 depth)) (choose 0 1 2 -1))
                 (collectE (flapjaxE-grmr input-stream (sub1 depth)) 0 +)
                 (mapE (λ (e) (list (get-timestamp e) (+ (get-value e) (choose 1 2 3))))
                       (flapjaxE-grmr input-stream (sub1 depth)))
                 (constantE (flapjaxE-grmr input-stream (sub1 depth)) 1)
                 (delayE (flapjaxE-grmr input-stream (sub1 depth)) (choose 1 2 3))))

(define-synthax (flapjaxE-grmr2 input-stream1 input-stream2 depth)
  #:base (choose input-stream1 input-stream2)
  #:else (choose input-stream1
                 input-stream2
                 (collectE (flapjaxE-grmr2 input-stream1 input-stream2 (sub1 depth)) 0 +)
                 (mapE (λ (e) (list (get-timestamp e) (+ (get-value e) (choose 1 2 3))))
                       (flapjaxE-grmr2 input-stream1 input-stream2 (sub1 depth)))
                 (constantE (flapjaxE-grmr2 input-stream1 input-stream2 (sub1 depth)) (choose -1 1))
                 (delayE (flapjaxE-grmr2 input-stream1 input-stream2 (sub1 depth)) (choose 1 2 3))
                 (startsWith (flapjaxE-grmr2 input-stream1 input-stream2 (sub1 depth)) 0)
                 (mergeE (flapjaxE-grmr2 input-stream1 input-stream2 (sub1 depth))
                         (flapjaxE-grmr2 input-stream1 input-stream2 (sub1 depth)))
                 ))

#;(define-synthax (flapjax-grmrB input-behavior depth)
  #:base input-behavior
  #:else (choose input-behavior
                ; (liftB (λ (t) (<= t 2)) input-behavior)
                ; (constantB (choose 'on 'off))
                 (liftB (λ (t) (<= t 2))
                        (flapjax-grmrB input-behavior (sub1 depth)))
                 ))

;; there's probably a more elegant way
(define-synthax (flapjax-grmrB2 inputB1 inputB2 depth)
  #:base (choose inputB1 inputB2)
  #:else (choose inputB1 inputB2
                 (constantB (choose 'on 'off))
                 (liftB (choose (λ (clock location) (if (or (>= (time-vec->integer clock) 2130) (< (time-vec->integer clock) 800))
                                 'night
                                 (if (equal? location 'home)
                                     'home
                                     'away)))
                                (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none)))
                                (flapjax-grmrB2 inputB1 inputB2 (sub1 depth)) (flapjax-grmrB2 inputB1 inputB2 (sub1 depth)))
                 (liftB (choose (λ (e) (if e 'on 'off))
                                (λ (t) (<= t 2))
                                (λ (c) (or (>= (vector-ref c 0) 4) (>= 2 (vector-ref c 0)))))
                                (flapjax-grmrB2 inputB1 inputB2 (sub1 depth)))
                 (andB (flapjax-grmrB2 inputB1 inputB2 (sub1 depth)) (flapjax-grmrB2 inputB1 inputB2 (sub1 depth)))
                 (ifB (flapjax-grmrB2 inputB1 inputB2 (sub1 depth)) (flapjax-grmrB2 inputB1 inputB2 (sub1 depth)) (flapjax-grmrB2 inputB1 inputB2 (sub1 depth)))
))

(define-synthax (flapjax-grmrB3 inputB1 inputB2 inputB3 depth)
  #:base (choose inputB1 inputB2 inputB3)
  #:else (choose inputB1 inputB2 inputB3
                 (constantB (choose 'on 'off))
                 (liftB (choose (λ (clock location) (if (or (>= (time-vec->integer clock) 2130) (< (time-vec->integer clock) 800))
                                 'night
                                 (if (equal? location 'home)
                                     'home
                                     'away)))
                                (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none)))
                                (flapjax-grmrB3 inputB1 inputB2 inputB3 (sub1 depth)) (flapjax-grmrB3 inputB1 inputB2 inputB3 (sub1 depth)))
                 (liftB (λ (e) (if e 'on 'off)) (flapjax-grmrB3 inputB1 inputB2 inputB3 (sub1 depth)))
                 (andB (flapjax-grmrB3 inputB1 inputB2 inputB3 (sub1 depth)) (flapjax-grmrB3 inputB1 inputB2 inputB3 (sub1 depth)) (flapjax-grmrB3 inputB1 inputB2 inputB3 (sub1 depth)))
                 (ifB (flapjax-grmrB3 inputB1 inputB2 inputB3 (sub1 depth)) (flapjax-grmrB3 inputB1 inputB2 inputB3 (sub1 depth)) (flapjax-grmrB3 inputB1 inputB2 inputB3 (sub1 depth)))
))
