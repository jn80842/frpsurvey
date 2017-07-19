#lang rosette/safe
(provide (all-defined-out))

(define (get-timestamp item)
  (first item))

(define (get-value item)
  (second item))

;;;;;; timestamp related helpers ;;;;;;;;
(define ts-comparator (λ (x y) (< (first x) (first y))))

(define (startAtTimestamp ts evt-stream)
  (unless (empty? evt-stream)
    (filter (λ (e) (>= (first e) ts)) evt-stream)))
(define (endAtTimestamp ts evt-stream)
  (unless (empty? evt-stream)
    (filter (λ (e) (<= (first e) ts)) evt-stream)))
(define (boundedTimestampsStream ts1 ts2 evt-stream)
  (unless (empty? evt-stream)
    (filter (λ (e) (and (>= (first e) ts1) (<= (first e) ts2))) evt-stream)))

(define (valid-timestamps? stream)
  (and (apply distinct? (map get-timestamp stream))
       (andmap integer? (map get-timestamp stream))
       (andmap positive? (map get-timestamp stream))
       (timestamps-sorted? stream)))

(define (timestamps-sorted? stream)
  (equal? (map get-timestamp stream) (sort (map get-timestamp stream) <)))

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

(define (get-missing-timestamps b1 b2)
  (filter (λ (t) (not (member t (map get-timestamp (behavior-changes b1)))))
          (map get-timestamp (behavior-changes b2))))

(define (valid-behavior? b)
  (and (behavior? b)
       (apply distinct? (map get-timestamp (behavior-changes b)))
       (andmap positive? (map get-timestamp (behavior-changes b)))
       (timestamps-sorted? (behavior-changes b))))

(define (all-unique-timestamps . behaviors)
  (sort (remove-duplicates (flatten (map (λ (b) (map get-timestamp (behavior-changes b))) behaviors))) <))

(define (equal-behaviors? b1 b2)
  (let ([enhanced-b1 (project-values b1 (all-unique-timestamps b1 b2))]
        [enhanced-b2 (project-values b2 (all-unique-timestamps b2 b1))])
    (and (eq? (behavior-init b1) (behavior-init b2))
         (eq? enhanced-b1 enhanced-b2))))

(define (behavior-check b proc)
  (and (proc (behavior-init b))
       (andmap (map proc (behavior-changes b)))))

;;;;; make symbolic event streams ;;;;;;;;;;
(define (boolean-event-stream concrete-list)
  (map (λ (c)
         (define-symbolic* timestamp integer?)
         (assert (>= (length concrete-list) timestamp))
         (assert (> timestamp 0))
         (define-symbolic* value boolean?)
         (list timestamp value)) concrete-list))

(define (positive-integer-event-stream concrete-list)
  (map (λ (c)
         (define-symbolic* timestamp integer?)
         (assert (>= (length concrete-list) timestamp))
         (assert (> timestamp 0))
         (define-symbolic* value integer?)
         (assert (> value -1))
         (list timestamp value)) concrete-list))

(define (time-vec-event-stream concrete-list)
  (map (λ (c)
         (define-symbolic* timestamp integer?)
         (assert (>= (length concrete-list) timestamp))
         (assert (> timestamp 0))
         (define-symbolic* hour integer?)
         (assert (>= 23 hour))
         (assert (> hour 0))
         (define-symbolic* minute-tens integer?)
         (assert (>= 5 minute-tens))
         (assert (> minute-tens 0))
         (define-symbolic* minute-ones integer?)
         (list timestamp (vector hour minute-tens minute-ones))) concrete-list))

;;;;;;;; make symbolic behaviors ;;;;;;;;

(define (boolean-behavior concrete-list)
  (define-symbolic* init-val boolean?)
  (behavior init-val (boolean-event-stream concrete-list)))

(define (positive-integer-behavior concrete-list)
  (define-symbolic* init-val integer?)
  (assert (> init-val 0))
  (behavior init-val (positive-integer-event-stream concrete-list)))

(define (time-vec-behavior concrete-list)
  (define-symbolic* hour integer?)
  (assert (>= 23 hour))
  (assert (> hour 0))
  (define-symbolic* minute-tens integer?)
  (assert (>= 5 minute-tens))
  (assert (> minute-tens 0))
  (define-symbolic* minute-ones integer?)
  (behavior (vector hour minute-tens minute-ones) (time-vec-event-stream concrete-list)))