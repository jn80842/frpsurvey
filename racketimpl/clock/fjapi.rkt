#lang rosette
(provide (all-defined-out))

(require "fjmodel.rkt")

(define (identityE history)
  (last history))

;; oneE

(define (zeroE e)
  'no-evt)

(define (mapE proc history)
  (proc (last history)))

(define (mapE2 proc history1 history2)
  (let ([e1 (last history1)]
        [e2 (last history2)])
    (if (empty-event? e2)
        'no-evt
        (proc e1 e2))))

(define (mergeE history1 history2)
  (let ([e1 (last history1)]
        [e2 (last history2)])
    (if (empty-event? e1)
        e2
        e1)))

;; switchE

(define (filterE proc history)
  (let ([e (last history)])
    (if (proc e)
        e
        'no-evt)))

(define (ifE guardE trueE falseE)
  (if (last guardE) (last trueE) (last falseE)))

(define (constantE const e)
  const)

;; stateful operators need list of all history up to current timestep
(define (collectE init proc lst)
  (foldl (λ (n m) (if (empty-event? n) m (proc n m))) init lst))

(define (andE history1 history2)
  (let ([e1 (last history1)]
        [e2 (last history2)])
    (if (not-empty-event? e2)
        (and e1 e2)
        'no-evt)))

(define (orE history1 history2)
  (let ([e1 (last history1)]
        [e2 (last history2)])
    (if (not-empty-event? e2)
        (or e1 e2)
        'no-evt)))

(define (notE history)
  (not (last history)))

;;is it possible to improve on this?
(define (filterRepeatsE lst)
  (letrec ([f (λ (evt rest)
                (cond [(empty? rest) evt]
                      [(equal? evt (first rest)) 'no-evt]
                      [(not-empty-event? (first rest)) evt]
                      [else (f evt (cdr rest))]))])
    (if (empty-event? (last lst))
        'no-evt
        (f (last lst) (cdr (reverse lst))))))

(define (snapshotE e b-input)
  (let ([b (behavior-changes b-input)])
    (last b)))

;; stateful operators need list of all history up to current timestep
(define (onceE lst)
  (if (findf (λ (e) (not (empty-event? e))) (take lst (sub1 (length lst))))
      'no-evt
      (last lst)))

;; skipFirstE

;; stateful operators need list of all history up to current timestep
(define (delayE interval lst)
  (if (>= interval (length lst))
        'no-evt
        (list-ref lst (- (length lst) (add1 interval)))))

;; blindE

;; calmE

;; stateful operators need list of all history up to current timestep
(define (timerE interval lst)
  (if (equal? 0 (modulo (length lst) interval))
      #t
      'no-evt))

;; stateful operators need list of all history up to current timestep
(define (startsWith init-value evt-stream)
  (behavior init-value (for/list ([i (range (length evt-stream))])
                         (findf (λ (e) (not (empty-event? e)))
                                (reverse (cons init-value (take evt-stream (add1 i))))))))

;; this is wrong
(define (changes behaviorB)
    (behavior-changes behaviorB))

(define (constantB const inputB)
  (behavior const (map (λ (e) const) (behavior-changes inputB))))

;; delayB

;; switchB

(define (andB behavior1 behavior2)
  (behavior (and (behavior-init behavior1) (behavior-init behavior2))
            (map (λ (b1 b2) (and b1 b2)) (behavior-changes behavior1) (behavior-changes behavior2))))

(define (orB behavior1 behavior2)
  (behavior (or (behavior-init behavior1) (behavior-init behavior2))
            (map (λ (b1 b2) (or b1 b2)) (behavior-changes behavior1) (behavior-changes behavior2))))

(define (notB behavior1)
  (behavior (not (behavior-init behavior1)) (map not (behavior-changes behavior1))))

(define (liftB1 proc argB)
  (behavior (proc (behavior-init argB)) (map proc (behavior-changes argB))))

(define (liftB2 proc argB1 argB2)
  (behavior (proc (behavior-init argB1) (behavior-init argB2))
            (map proc (behavior-changes argB1) (behavior-changes argB2))))

(define (ifB conditionB trueB falseB)
  (behavior (if (behavior-init conditionB) (behavior-init trueB) (behavior-init falseB))
            (map (λ (cB tB fB) (if cB tB fB))
                 (behavior-changes conditionB)
                 (behavior-changes trueB)
                 (behavior-changes falseB))))

(define (collectB init-val proc b1)
  (let ([b-init (proc init-val (behavior-init b1))])
         (letrec ([collect (λ (lst prev)
                       (if (empty? lst)
                           '()
                           (cons (proc (first lst) prev) (collect (rest lst) (proc (first lst) prev)))))])
           (behavior b-init (collect (behavior-changes b1) b-init)))))

;; timerB

;; blindB

;; calmB