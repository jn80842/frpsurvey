#lang rosette

(require rosette/lib/synthax)
(provide (all-defined-out))

;(error-print-width 100000000000)

(define (get-timestamp item)
  (first item))

(define (get-value item)
  (second item))

(define (max-for-current-bitwidth n)
  (sub1 (expt 2 (sub1 n))))

(define (print-bitwidth-warning)
  (printf "Current bitwidth is ~a; values larger than ~a will overflow.~n"
          (current-bitwidth) (max-for-current-bitwidth (current-bitwidth))))

(define (all-but-last lst)
  (reverse (cdr (reverse lst))))

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
  (timestamps-sorted-and-distinct? stream))

(define (timestamps-below-max? max stream)
  (andmap (λ (t) (>= max t)) (map get-timestamp stream)))

(define (timestamps-sorted? stream)
  (let ([times (map get-timestamp stream)])
    (andmap <= (all-but-last times) (rest times))))

(define (timestamps-sorted-and-distinct? stream)
  (let ([times (map get-timestamp stream)])
    (andmap < (all-but-last times) (rest times))))

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

#;(define (valueNow behavior1 ts) ;; altered a bit for our own use
  (let ([filtered-changes (filter (λ (t) (<= (get-timestamp t) ts)) (behavior-changes behavior1))])
    (if (empty? filtered-changes)
        (behavior-init behavior1)
        (get-value (last filtered-changes)))))

(define (valueNow behavior1 ts)
  (cond [(empty? (behavior-changes behavior1)) (behavior-init behavior1)]
        [(< ts (get-timestamp (first (behavior-changes behavior1)))) (behavior-init behavior1)]
        [else (valueNow (behavior (get-value (first (behavior-changes behavior1))) (rest (behavior-changes behavior1))) ts)]))

(define (valueNow-List l ts)
  (let ([filtered-changes (filter (λ (t) (<= (get-timestamp t) ts)) l)])
    (if (empty? filtered-changes)
        0
        (get-value (last filtered-changes)))))

(define (project-values b ts)
  (map (λ (t) (list t (valueNow b t))) ts))

(define (project-values-list l ts)
  (map (λ (t) (list t (valueNow-List l t))) ts))

(define (projected-behavior b ts)
  (behavior (behavior-init b) (project-values b ts)))

(define (get-missing-timestamps b1 b2)
  (filter (λ (t) (not (member t (map get-timestamp (behavior-changes b1)))))
          (map get-timestamp (behavior-changes b2))))

(define (valid-behavior? b)
  (timestamps-sorted-and-distinct? (behavior-changes b)))

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
    (and (equal? (behavior-init b1) (behavior-init b2))
         (equal? enhanced-b1 enhanced-b2))))

(define (behavior-check proc . behaviors)
  (let ([all-ts (apply all-unique-timestamps behaviors)])
    (and (apply proc (map behavior-init behaviors))
         (andmap (λ (vs) (apply proc vs))
                 (apply (curry map list) (map (λ (b) (map get-value (project-values b all-ts))) behaviors))))))

(define (implication p q)
  (or (not p) q))

;;;;; make symbolic event streams ;;;;;;;;;;

(define (sym-boolean)
  (define-symbolic* b boolean?)
  b)

(define (sym-integer)
  (define-symbolic* i integer?)
  i)

(define (sym-union-constructor symbol1 symbol2)
  (λ ()
  (define-symbolic* b boolean?)
  (if b symbol1 symbol2)))

(define (sym-time-vec)
  (define-symbolic* hour integer?)
  (define-symbolic* minute-tens integer?)
  (define-symbolic* minute-ones integer?)
  (vector hour minute-tens minute-ones))

(define (new-event-stream constructor n max-ts)
  (for/list ([i n])
    (define-symbolic* ts integer?)
    (assert (positive? ts))
    (assert (>= max-ts ts))
    (list ts (constructor))))

;;;;;;;; make symbolic behaviors ;;;;;;;;

(define (new-behavior constructor n max-ts)
  (behavior (constructor) (new-event-stream constructor n max-ts)))

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

(define (harvest x)
  (if (behavior? x)
      (harvest-behavior x)
      (harvest-events x)))

(define (check-existence-of-solution spec . inputs)
  (displayln "checking assumptions ....")
  (define solved (solve (assert (apply spec inputs))))
  (if (unsat? solved)
      (displayln "no solution for assumptions")
      (begin (displayln "sample solution for assumptions:")
             (map (λ (i) (displayln (evaluate i solved))) inputs)))
  (void))