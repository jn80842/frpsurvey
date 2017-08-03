#lang rosette/safe
(provide (all-defined-out))

(define (get-timestamp item)
  (first item))

(define (get-value item)
  (second item))

(define (max-for-current-bitwidth n)
  (sub1 (expt 2 (sub1 n))))

;;;;;; timestamp related helpers ;;;;;;;;
(define ts-comparator (λ (x y) (< (first x) (first y))))

(define (startAtTimestamp ts evt-stream)
  (filter (λ (e) (>= (first e) ts)) evt-stream))
(define (startBehaviorAtTimestamp ts b)
  (let ([filtered-evt-stream (filter (λ (e) (>= (first e) ts)) (behavior-changes b))])
    (if (member ts (map get-timestamp filtered-evt-stream))
        filtered-evt-stream
        (append (list (list ts (valueNow b ts))) filtered-evt-stream))))
(define (endAtTimestamp ts evt-stream)
  (filter (λ (e) (<= (first e) ts)) evt-stream))
(define (boundedTimestampsStream ts1 ts2 evt-stream)
  (filter (λ (e) (and (>= (first e) ts1) (<= (first e) ts2))) evt-stream))

(define (valid-timestamps? stream)
  (and (apply distinct? (map get-timestamp stream))
       (andmap integer? (map get-timestamp stream))
       (andmap positive? (map get-timestamp stream))
       (timestamps-sorted? stream)))

(define (timestamps-sorted? stream)
  (equal? (map get-timestamp stream) (sort (map get-timestamp stream) <)))

(define (timestamps-below-max? max stream)
  (andmap (λ (t) (>= max t)) (map get-timestamp stream)))

;;;; represent times with vectors to allow us to use bitwidth 5 ;;;;
(define (s-time-vec)
  (define-symbolic* hour integer?)
  (define-symbolic* minute-tens integer?)
  (define-symbolic* minute-ones integer?)
  (vector hour minute-tens minute-ones))

(define (time-vec->integer vec)
  (+ (* (vector-ref vec 0) 100) (* (vector-ref vec 1) 10) (vector-ref vec 2)))
(define (integer->time-vec t)
  (vector (quotient t 100) (quotient (remainder t 100) 10) (modulo t 10)))

;; note: time-vecs don't need to be numerically sorted!
;; an smaller time-vec following a larger one represents the next day
;; while logical timesteps are monotonically increasing
;; two equivalent time vecs in sequence is also legal, assumes logical time step is shorter than 1 min
(define (valid-time-vec? vec)
  (and (>= (vector-ref vec 0) 0)
       (<= (vector-ref vec 0) 23)
       (>= (vector-ref vec 1) 0)
       (<= (vector-ref vec 1) 5)
       (>= (vector-ref vec 2) 0)
       (<= (vector-ref vec 2) 9)
       ))

;;;; note for all behavior operators:                       ;;;;
;;;; behaviors *always* have a value (can never be 'no-evt) ;;;;

(struct behavior (init changes)
  #:transparent
  )

(define (valueNow behavior1 ts) ;; altered a bit for our own use
  (let ([filtered-changes (filter (λ (t) (<= (get-timestamp t) ts)) (behavior-changes behavior1))])
    (if (empty? filtered-changes)
        (behavior-init behavior1)
        (get-value (last filtered-changes)))))

(define (project-values b ts)
  (map (λ (t) (list t (valueNow b t))) ts))

(define (projected-behavior b ts)
  (behavior (behavior-init b) (project-values b ts)))

(define (get-missing-timestamps b1 b2)
  (filter (λ (t) (not (member t (map get-timestamp (behavior-changes b1)))))
          (map get-timestamp (behavior-changes b2))))

(define (valid-behavior? b)
  (and (behavior? b)
       (apply distinct? (map get-timestamp (behavior-changes b)))
       (andmap positive? (map get-timestamp (behavior-changes b)))
       (timestamps-sorted? (behavior-changes b))))

(define (valid-time-vec-behavior? b)
  (and (valid-behavior? b)
       (behavior-check valid-time-vec-value? b)))

(define (valid-time-vec-value? t)
  (and (>= 23 (vector-ref t 0))
       (>= (vector-ref t 0) 0)
       (>= 5 (vector-ref t 1))
       (>= (vector-ref t 1) 0)
       (>= 9 (vector-ref t 2))
       (>= (vector-ref t 2) 0)))

(define (all-unique-timestamps . behaviors)
  (sort (remove-duplicates (flatten (map (λ (b) (map get-timestamp (behavior-changes b))) behaviors))) <))

(define (equal-behaviors? b1 b2)
  (let ([enhanced-b1 (project-values b1 (all-unique-timestamps b1 b2))]
        [enhanced-b2 (project-values b2 (all-unique-timestamps b2 b1))])
    (and (eq? (behavior-init b1) (behavior-init b2))
         (eq? enhanced-b1 enhanced-b2))))

;(apply (curry map list) (map (λ (b) (map get-value (project-values b (list 1 2 3 4 5 6)))) (list b b2)))

(define (behavior-check proc . behaviors)
  (let ([all-ts (apply all-unique-timestamps behaviors)])
    (and (apply proc (map behavior-init behaviors))
         (andmap (λ (vs) (apply proc vs))
                 (apply (curry map list) (map (λ (b) (map get-value (project-values b all-ts))) behaviors))))))

(define (implication p q)
  (or (not p) q))

;;;;; make symbolic event streams ;;;;;;;;;;

;; this is kludgy but i don't want to write a recursive function
(define (stream-size n)
  (cond [(eq? n 0) '()]
         [(eq? n 1) '(1)]
         [(eq? n 2) '(1 2)]
         [(eq? n 3) '(1 2 3)]
         [(eq? n 4) '(1 2 3 4)]
         [(eq? n 5) '(1 2 3 4 5)]
         [(eq? n 6) '(1 2 3 4 5 6)]))

(define (boolean-event-stream n)
  (let ([concrete-list (stream-size n)])
    (map (λ (c)
           (define-symbolic* timestamp integer?)
           (assert (>= (length concrete-list) timestamp))
           (assert (> timestamp 0))
           (define-symbolic* value boolean?)
           (list timestamp value)) concrete-list)))

(define (integer-event-stream n)
  (let ([concrete-list (stream-size n)])
    (map (λ (c)
           (define-symbolic* timestamp integer?)
           (define-symbolic* value integer?)
           (list timestamp value)) concrete-list)))

(define (positive-integer-event-stream n)
  (let ([concrete-list (stream-size n)])
    (map (λ (c)
           (define-symbolic* timestamp integer?)
           (assert (>= (length concrete-list) timestamp))
           (assert (> timestamp 0))
           (define-symbolic* value integer?)
           (assert (> value -1))
           (list timestamp value)) concrete-list)))

(define (time-vec-event-stream n)
  (let ([concrete-list (stream-size n)])
    (map (λ (c)
           (define-symbolic* timestamp integer?)
           (define-symbolic* hour integer?)
           (define-symbolic* minute-tens integer?)
           (define-symbolic* minute-ones integer?)
           (list timestamp (vector hour minute-tens minute-ones))) concrete-list)))

;;;;;;;; make symbolic behaviors ;;;;;;;;

(define (boolean-behavior n)
  (define-symbolic* init-val boolean?)
  (behavior init-val (boolean-event-stream n)))

(define (positive-integer-behavior n)
  (define-symbolic* init-val integer?)
  (assert (> init-val 0))
  (behavior init-val (positive-integer-event-stream n)))

(define (integer-behavior n)
  (define-symbolic* init-val integer?)
  (behavior init-val (integer-event-stream n)))

(define (time-vec-behavior n)
  (define-symbolic* hour integer?)
  (define-symbolic* minute-tens integer?)
  (define-symbolic* minute-ones integer?)
  (behavior (vector hour minute-tens minute-ones) (time-vec-event-stream n)))

(define (check-existence-of-solution spec . inputs)
  (displayln "checking assumptions ....")
  (define solved (solve (assert (apply spec inputs))))
  (if (unsat? solved)
      (displayln "no solution for assumptions")
      (begin (displayln "sample solution for assumptions:")
             (map (λ (i) (displayln (evaluate i solved))) inputs)))
  (void))