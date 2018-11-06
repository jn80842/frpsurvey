#lang rosette

(require rosette/lib/synthax)
(provide (all-defined-out))

;(error-print-width 100000000000)

;; NB: it's possible to create distinct symbolic vars
;; which *don't* refer to same var but have same name
;; always get vars through these functions just so
;; naming is clear

(define (get-sym-bool)
  (define-symbolic* b boolean?)
  b)
(define (get-sym-int)
  (define-symbolic* i integer?)
  i)

;; event streams are a list of events; each entry in the list represents a single time step
;; an event is a symbolic union over a symbol represents no event and a symbolic value

(define NOEVENT 'no-evt)

(define (empty-event? e)
  (eq? NOEVENT e))

(define (not-empty-event? e)
  (not (eq? NOEVENT e)))

(define (new-event-stream constructor n)
  (for/list ([i n])
    (if (get-sym-bool)
        (constructor)
        NOEVENT)))

;; behaviors are a struct containing an initial value and a list of (symbolic) values
;; each entry in the list represents a single timestep
;; no entry in that list can be a non event; behaviors have a value at every timestep

(struct behavior (init changes) #:transparent)

(define (behavior-predicate proc . behaviors)
  (and (apply proc (map behavior-init behaviors))
       (andmap (λ (vs) (apply proc vs)) (apply (curry map list) (map behavior-changes behaviors)))))

(define (new-behavior constructor n)
  (behavior (constructor) (for/list ([i n])
                            (constructor))))

;; bitwidth helpers

(define (max-for-current-bitwidth n)
  (sub1 (expt 2 (sub1 n))))

(define (print-bitwidth-warning)
  (printf "Current bitwidth is ~a; values larger than ~a will overflow.~n"
          (current-bitwidth) (max-for-current-bitwidth (current-bitwidth))))

;; other helpers

(define  (same program1 program2 . inputs)
  (equal? (apply program1 inputs)
                  (apply program2 inputs)))
