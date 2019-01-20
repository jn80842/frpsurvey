#lang rosette

(require "fjapi.rkt")
(require "operators.rkt")
(require "sketch.rkt")

#;(provide all-defined-out)
(provide print-input-model)

;; (output-smt "smtoutput/")

(current-bitwidth 3)

(define ENTER (bv 0 (bitvector 1)))
(define EXIT (bv 1 (bitvector 1)))

(define MOVEUP (bv 0 (bitvector 2)))
(define MOVEDOWN (bv 1 (bitvector 2)))
(define STOP (bv 2 (bitvector 2)))

(define MOVINGUP (bv 0 (bitvector 2)))
(define MOVINGDOWN (bv 1 (bitvector 2)))
(define STOPPED (bv 2 (bitvector 2)))

(define EMPTY (bv 0 (bitvector 1)))
(define OCCUPIED (bv 1 (bitvector 1)))

(define INCBYONE (bv 0 (bitvector 1)))
(define DECBYONE (bv 1 (bitvector 1)))

(define-symbolic* top? boolean?)
(define-symbolic* top (bitvector 1))
(define sym-top (if top? top NOEVENT))

(define-symbolic* bottom? boolean?)
(define-symbolic* bottom (bitvector 1))
(define sym-bottom (if bottom? bottom NOEVENT))

;; stepsMovement must always have a value, so it is not a symbolic union
(define-symbolic* sym-stepsMovement (bitvector 2))

;; userCounter must always have a value, so it is not a symbolic union
;; in model 3, it can only have the states "escalator empty" and "escalator not empty"
(define-symbolic* sym-userCounter (bitvector 2))

(define (print-input-model m)
  (if (evaluate top? m)
      (println (if (equal? (evaluate top m) ENTER)
                   "topSensor: ENTER"
                   "topSensor: EXIT"))
      (println "topSensor: 'no-evt"))
  (if (evaluate bottom? m)
      (println (if (equal? (evaluate bottom m) ENTER)
                   "bottomSensor: ENTER"
                   "bottomSensor: EXIT"))
      (println "bottomSensor: 'no-evt"))
  (let ([evaled-steps (evaluate sym-stepsMovement m)])
    (if (term? evaled-steps)
        (println "stepsMovement: not set")
        (println (cond [(equal? evaled-steps MOVEUP) "stepsMovement: MOVINGUP"]
                       [(equal? evaled-steps MOVEDOWN) "stepsMovement: MOVINGDOWN"]
                       [(equal? evaled-steps STOP) "stepsMovement: STOPPED"]
                       [else "stepsMovement: illegal value"]))))
  (if (term? (evaluate sym-userCounter m))
      (println "userCounter: not set")
      (println (if (equal? (evaluate sym-userCounter m) EMPTY)
                   "userCounter: EMPTY"
                   "userCounter: OCCUPIED"))))

(define-symbolic* stepsUpdate? boolean?)
(define-symbolic* stepsUpdate (bitvector 2))
(define sym-stepsUpdate (if stepsUpdate? stepsUpdate NOEVENT))

(define-symbolic* userCounterUpdate? boolean?)
(define-symbolic* userCounterUpdate (bitvector 1))
(define sym-userCounterUpdate (if userCounterUpdate? userCounterUpdate NOEVENT))

;; constraints on inputs
(define (input-constraints topSensor bottomSensor stepsMovement userCounter)
  ;; stepsMovement can't have value 3
  (not (equal? stepsMovement (bv 3 (bitvector 2)))))

;; theta zero
;; ((enterEvent(bottom) && !exitEvent(top) && !(steps == MOVEDOWN))
;; || (enterEvent(top) && !exitEvent(bottom) && !(steps == MOVEUP)))
;; <-> users := users + 1
(define (theta0 topSensor bottomSensor stepsMovement userCounter stepsUpdate userCounterUpdate)
  (iff (or (and (equal? bottomSensor ENTER) (or (empty-event? topSensor) (not (equal? topSensor EXIT))) (not (equal? stepsMovement MOVINGDOWN)))
           (and (equal? topSensor ENTER) (or (empty-event? bottomSensor) (not (equal? bottomSensor EXIT))) (not (equal? stepsMovement MOVINGUP))))
       (equal? userCounterUpdate INCBYONE)))

;; theta one
;; ((exitEvent(top) && !enterEvent(bottom) && !(steps == MOVEDOWN))
;; || (exitEvent(bottom) && !enterEvent(top) && !(steps == MOVEUP)))
;; <-> users := users - 1
(define (theta1 topSensor bottomSensor stepsMovement userCounter stepsUpdate userCounterUpdate)
  (iff (or (and (equal? topSensor EXIT) (or (empty-event? bottomSensor) (not (equal? bottomSensor ENTER))) (not (equal? stepsMovement MOVINGDOWN)))
           (and (equal? bottomSensor EXIT) (or (empty-event? topSensor) (not (equal? topSensor ENTER))) (not (equal? stepsMovement MOVINGUP))))
       (equal? userCounterUpdate DECBYONE)))

(define (handwrittenfunc-theta0 topSensor bottomSensor stepsMovement userCounter)
  (constantE INCBYONE (filterE identity (orE (andE (maskOffE (filterE (λ (e) (equal? e EXIT)) topSensor)
                                                             (filterE (λ (e) (equal? e ENTER)) bottomSensor))
                                                   (notE (mapE (λ (e) (equal? e MOVINGDOWN)) stepsMovement)))
                                             (andE (maskOffE (filterE (λ (e) (equal? e EXIT)) bottomSensor)
                                                             (filterE (λ (e) (equal? e ENTER)) topSensor))
                                                   (notE (mapE (λ (e) (equal? e MOVINGUP)) stepsMovement)))))))
(define (smallfunc topSensor bottomSensor stepsMovement userCounter)
  (orE (andE (maskOffE (filterE (λ (e) (equal? e EXIT)) topSensor)
                       (filterE (λ (e) (equal? e ENTER)) bottomSensor))
             (notE (mapE (λ (e) (equal? e MOVINGDOWN)) stepsMovement)))
       (andE (maskOffE (filterE (λ (e) (equal? e EXIT)) bottomSensor)
                       (filterE (λ (e) (equal? e ENTER)) topSensor))
             (notE (mapE (λ (e) (equal? e MOVINGUP)) stepsMovement)))))

(define small-sk (get-symbolic-sketch 14 4))

(define (handwrittenregs-theta0 topSensor bottomSensor stepsMovement userCounter)
  (let* ([R1 topSensor]
         [R2 bottomSensor]
         [R3 stepsMovement]
         [R4 userCounter]
         [R5 (filterE (λ (e) (equal? e EXIT)) R1)]
         [R6 (filterE (λ (e) (equal? e ENTER)) R2)]
         [R7 (maskOffE R5 R6)]
         [R8 (mapE (λ (e) (equal? e MOVINGDOWN)) R3)]
         [R9 (notE R8)]
         [R10 (andE R7 R9)]
         [R11 (filterE (λ (e) (equal? e EXIT)) R2)]
         [R12 (filterE (λ (e) (equal? e ENTER)) R1)]
         [R13 (maskOffE R11 R12)]
         [R14 (mapE (λ (e) (equal? e MOVINGUP)) R3)]
         [R15 (notE R14)]
         [R16 (andE R13 R15)]
         [R17 (orE R10 R16)]
         [R18 (filterE identity R17)]
         [R19 (constantE INCBYONE R18)])
    R19))

(define theta0-sk (get-symbolic-sketch 15 4))

(clear-asserts!)
#;(let ([evaled-sk ((get-sketch-function theta0-sk) sym-top sym-bottom sym-stepsMovement sym-userCounter)]
      [constraints (assert (input-constraints sym-top sym-bottom sym-stepsMovement sym-userCounter))])
  (let ([b (time (synthesize #:forall (symbolics (list sym-top sym-bottom sym-stepsMovement sym-userCounter))
                    #:guarantee (assert (equal? evaled-sk (handwrittenfunc-theta0 sym-top sym-bottom sym-stepsMovement sym-userCounter)))
;(assert (theta0 sym-top sym-bottom sym-stepsMovement sym-userCounter sym-stepsUpdate evaled-sk)))))
                    ))])
    (if (unsat? b)
        (println "unsat")
        (begin (println b) (print-sketch theta0-sk b)))
    ))
(clear-asserts!)

;; top NOEVENT bottom NOEVENT -> userCounterUpdate NOEVENT
(define (ex1 topSensor bottomSensor stepsMovement userCounter stepsUpdate userCounterUpdate)
  (implies (and (equal? topSensor NOEVENT) (equal? bottomSensor NOEVENT))
           (equal? userCounterUpdate NOEVENT)))
;; top NOEVENT bottom ENTER stepsMovement MOVINGUP -> INCBYONE
(define (ex2 topSensor bottomSensor stepsMovement userCounter stepsUpdate userCounterUpdate)
  (implies (and (equal? topSensor NOEVENT) (equal? bottomSensor ENTER) (equal? stepsMovement MOVINGUP))
           (equal? userCounterUpdate INCBYONE)))
;; top ENTER bottom ENTER stepsMovement MOVINGUP -> INCBYONE
(define (ex3 topSensor bottomSensor stepsMovement userCounter stepsUpdate userCounterUpdate)
  (implies (and (equal? topSensor ENTER) (equal? bottomSensor ENTER) (equal? stepsMovement MOVINGUP))
           (equal? userCounterUpdate INCBYONE)))
;; top EXIT bottom ENTER stepsMovement MOVINGUP -> NOEVENT
(define (ex4 topSensor bottomSensor stepsMovement userCounter stepsUpdate userCounterUpdate)
  (implies (and (equal? topSensor EXIT) (equal? bottomSensor ENTER) (equal? stepsMovement MOVINGUP))
           (equal? userCounterUpdate NOEVENT)))
;; bottom EXIT stepsMovement -> NOEVENT
(define (ex5 topSensor bottomSensor stepsMovement userCounter stepsUpdate userCounterUpdate)
  (implies (and (equal? bottomSensor EXIT) (equal? stepsMovement MOVINGDOWN))
           (equal? userCounterUpdate NOEVENT)))
;; top ENTER bottom NOEVENT stepsMovement MOVINGDOWN -> INCBYONE
(define (ex6 topSensor bottomSensor stepsMovement userCounter stepsUpdate userCounterUpdate)
   (implies (and (equal? topSensor ENTER) (equal? bottomSensor NOEVENT) (equal? stepsMovement MOVINGDOWN))
            (equal? userCounterUpdate INCBYONE)))
;; top ENTER bottom EXIT stepsMovement MOVINGDOWN -> NOEVENT
(define (ex7 topSensor bottomSensor stepsMovement userCounter stepsUpdate userCounterUpdate)
  (implies (and (equal? topSensor ENTER) (equal? bottomSensor EXIT) (equal? stepsMovement MOVINGDOWN))
           (equal? userCounterUpdate NOEVENT)))
;; top ENTER bottom ENTER stepsMovement MOVINGDOWN -> INCBYONE
(define (ex8 topSensor bottomSensor stepsMovement userCounter stepsUpdate userCounterUpdate)
  (implies (and (equal? topSensor ENTER) (equal? bottomSensor ENTER) (equal? stepsMovement MOVINGDOWN))
           (equal? userCounterUpdate INCBYONE)))
;; top EXIT stepsMovement MOVINGDOWN -> NOEVENT
(define (ex9 topSensor bottomSensor stepsMovement userCounter stepsUpdate userCounterUpdate)
  (implies (and (equal? topSensor EXIT) (equal? stepsMovement MOVINGDOWN))
           (equal? userCounterUpdate NOEVENT)))
;; top ENTER bottom NOEVENT stepsMovement MOVINGUP -> NOEVENT
(define (ex10 topSensor bottomSensor stepsMovement userCounter stepsUpdate userCounterUpdate)
  (implies (and (equal? topSensor ENTER) (equal? bottomSensor NOEVENT) (equal? stepsMovement MOVINGUP))
           (equal? userCounterUpdate NOEVENT)))
;; top ENTER bottom NOEVENT stepsMovement MOVINGUP -> NOEVENT

#;(let ([evaled-sk ((get-sketch-function theta0-sk) sym-top sym-bottom sym-stepsMovement sym-userCounter)]
      [constraints (assert (input-constraints sym-top sym-bottom sym-stepsMovement sym-userCounter))])
  (let ([b (time (synthesize #:forall (symbolics (list sym-top sym-bottom sym-stepsMovement sym-userCounter))
                             #:guarantee (begin
                                           (assert (ex1 sym-top sym-bottom sym-stepsMovement sym-userCounter NOEVENT evaled-sk))
                                           (assert (ex2 sym-top sym-bottom sym-stepsMovement sym-userCounter NOEVENT evaled-sk))
                                           (assert (ex3 sym-top sym-bottom sym-stepsMovement sym-userCounter NOEVENT evaled-sk))
                                           (assert (ex4 sym-top sym-bottom sym-stepsMovement sym-userCounter NOEVENT evaled-sk))
                                           (assert (ex5 sym-top sym-bottom sym-stepsMovement sym-userCounter NOEVENT evaled-sk))
                                           (assert (ex6 sym-top sym-bottom sym-stepsMovement sym-userCounter NOEVENT evaled-sk))
                                           (assert (ex7 sym-top sym-bottom sym-stepsMovement sym-userCounter NOEVENT evaled-sk))
                                           (assert (ex8 sym-top sym-bottom sym-stepsMovement sym-userCounter NOEVENT evaled-sk))
                                           (assert (ex9 sym-top sym-bottom sym-stepsMovement sym-userCounter NOEVENT evaled-sk))
                                           (assert (ex10 sym-top sym-bottom sym-stepsMovement sym-userCounter NOEVENT evaled-sk))
                                           )))])
    (if (unsat? b)
        (println "unsat")
        (print-sketch theta0-sk b))))
(clear-asserts!)
