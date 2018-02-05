#lang rosette

(provide (all-defined-out))

(struct behavior (init changes) #:transparent)

(define (is-empty? e)
  (eq? e 'no-evt))

(define (sym-integer)
  (define-symbolic* i integer?)
  i)

(define (new-event-stream constructor n)
  (for/list ([i n])
    (define-symbolic* b boolean?)
    (if b
        (constructor)
        'no-evt)))

(define (new-behavior constructor n)
  (behavior (constructor) (for/list ([i n])
                            (constructor))))

(define (eval-behavior-graph graph . inputs)
  (behavior (apply graph (map behavior-init inputs))
            (apply (curry map graph) (map behavior-changes inputs))))
(define (eval-stream-graph graph . inputs)
  (apply (curry map graph) inputs))

(define  (same program1 program2 . inputs)
  (equal? (apply program1 inputs)
                  (apply program2 inputs)))

(define (harvest-term v)
  (cond [(vector? v) (vector->list v)]
        [(and (union? v) (eq? 2 (length (union-contents v)))) (car (first (union-contents v)))]
        [(term? v) v]
        [else '()]))

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
