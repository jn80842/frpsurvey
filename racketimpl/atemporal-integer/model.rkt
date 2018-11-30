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

;; event streams are a symbolic union over a symbolic value and the special no event symbol

(define NOEVENT 'no-evt)

(define (empty-event? e)
  (eq? NOEVENT e))

(define (not-empty-event? e)
  (not (eq? NOEVENT e)))

(define (new-event-stream constructor)
  (if (get-sym-bool)
      (constructor)
      NOEVENT))

;; behaviors are a symbolic value (since behaviors must have a value at all times

(define (new-behavior constructor)
  (constructor))

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
