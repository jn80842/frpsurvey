#lang rosette

(require rosette/lib/synthax)
(provide (all-defined-out))

;(error-print-width 100000000000)

(define (max-for-current-bitwidth n)
  (sub1 (expt 2 (sub1 n))))

(define (print-bitwidth-warning)
  (printf "Current bitwidth is ~a; values larger than ~a will overflow.~n"
          (current-bitwidth) (max-for-current-bitwidth (current-bitwidth))))

(define (all-but-last lst)
  (reverse (cdr (reverse lst))))

;;;; represent times with vectors to allow us to use bitwidth 5 ;;;;
(define (s-time-vec)
  (define-symbolic* hour integer?)
  (define-symbolic* minute-tens integer?)
  (define-symbolic* minute-ones integer?)
  (vector hour minute-tens minute-ones))

(define (is-midnight? time-vec)
  (and (eq? (vector-ref time-vec 0) 0)
       (eq? (vector-ref time-vec 1) 0)
       (eq? (vector-ref time-vec 2) 0)))

(define (time-vec-hour v)
  (vector-ref v 0))
(define (time-vec-min1 v)
  (vector-ref v 1))
(define (time-vec-min2 v)
  (vector-ref v 2))

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

(struct behavior (init changes) #:transparent)

(define (last-behavior-value b1)
  (if (empty? (behavior-changes b1))
      (behavior-init b1)
      (last (behavior-changes b1))))

(define (pad-behavior b1 len)
  (behavior (behavior-init b1) (append (behavior-changes b1) (for/list ([i (- len (length (behavior-changes b1)))])
                                                               (last-behavior-value b1)))))

(define (pad-behavior-changes b1 len)
  (append (behavior-changes b1) (for/list ([i (- len (length (behavior-changes b1)))])
                                  (last-behavior-value b1))))

#;(define (valueNow behavior1 ts) ;; altered a bit for our own use
  (let ([filtered-changes (filter (λ (t) (<= (get-timestamp t) ts)) (behavior-changes behavior1))])
    (if (empty? filtered-changes)
        (behavior-init behavior1)
        (get-value (last filtered-changes)))))

#;(define (valueNow behavior1 ts)
  (cond [(empty? (behavior-changes behavior1)) (behavior-init behavior1)]
        [(< ts (get-timestamp (first (behavior-changes behavior1)))) (behavior-init behavior1)]
        [else (valueNow (behavior (get-value (first (behavior-changes behavior1))) (rest (behavior-changes behavior1))) ts)]))

#;(define (valueNow-List l ts)
  (let ([filtered-changes (filter (λ (t) (<= (get-timestamp t) ts)) l)])
    (if (empty? filtered-changes)
        0
        (get-value (last filtered-changes)))))

#;(define (project-values b ts)
  (map (λ (t) (list t (valueNow b t))) ts))

#;(define (project-values-list l ts)
  (map (λ (t) (list t (valueNow-List l t))) ts))

#;(define (projected-behavior b ts)
  (behavior (behavior-init b) (project-values b ts)))

(define (valid-time-vec-behavior? b)
  (behavior-check valid-time-vec-value? b))

(define (valid-time-vec-value? t)
  (and (>= 23 (vector-ref t 0))
       (>= (vector-ref t 0) 0)
       (>= 5 (vector-ref t 1))
       (>= (vector-ref t 1) 0)
       (>= 9 (vector-ref t 2))
       (>= (vector-ref t 2) 0)))

#;(define (all-unique-timestamps . behaviors)
  (sort (remove-duplicates (flatten (map (λ (b) (map get-timestamp (behavior-changes b))) behaviors))) <))

#;(define (equal-behaviors? b1 b2)
  (let ([enhanced-b1 (project-values b1 (all-unique-timestamps b1 b2))]
        [enhanced-b2 (project-values b2 (all-unique-timestamps b2 b1))])
    (and (equal? (behavior-init b1) (behavior-init b2))
         (equal? enhanced-b1 enhanced-b2))))

(define (behavior-check proc . behaviors)
  (and (apply proc (map behavior-init behaviors))
       (andmap (λ (vs) (apply proc vs)) (apply (curry map list) (map behavior-changes behaviors)))))

(define (implication p q)
  (or (not p) q))

(define (empty-event? e)
  (eq? 'no-evt e))
(define (not-empty-event? e)
  (not (eq? 'no-evt e)))

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

(define (new-event-stream constructor n)
  (for/list ([i n])
    (define-symbolic* b boolean?)
    (if b
        (constructor)
        'no-evt)))

;;;;;;;; make symbolic behaviors ;;;;;;;;

(define (new-behavior constructor n)
  (behavior (constructor) (for/list ([i n])
                            (constructor))))

(define  (same program1 program2 . inputs)
  (equal? (apply program1 inputs)
                  (apply program2 inputs)))

(define (harvest-term v)
  (cond [(vector? v) (vector->list v)]
        [(and (union? v) (eq? 2 (length (union-contents v)))) (car (first (union-contents v)))]
        [(term? v) v]))

(define (harvest-events evt-stream)
  (append (map (λ (s) (caar (union-contents s))) evt-stream)
          (filter term? (map (λ (s) (cdar (union-contents s))) evt-stream))))

(define (harvest-behavior b)
  (flatten (append (list (harvest-term (behavior-init b))) (map harvest-term (behavior-changes b)))))

(define (harvest . x)
  (apply append (map harvest-stream x)))

(define (harvest-stream x)
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