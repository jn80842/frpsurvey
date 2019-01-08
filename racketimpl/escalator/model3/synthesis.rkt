#lang rosette

(require "fjapi.rkt")
(require "operators.rkt")
(require "sketch.rkt")

(current-bitwidth #f)

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

(define (handwrittenregs3-theta0 topSensor bottomSensor stepsMovement userCounter)
  (let* ([R1 topSensor]
         [R2 bottomSensor]
         [R3 stepsMovement]
         [R4 userCounter]
         [R5 (call-stream-insn (list-ref operator-list 6) (stream-insn 6 0 0 0 0 1 0) (list R1 R2 R3 R4))]
         [R6 (call-stream-insn (list-ref operator-list 6) (stream-insn 6 1 0 0 0 0 0) (list R1 R2 R3 R4 R5))]
         [R7 (call-stream-insn (list-ref operator-list 14) (stream-insn 14 4 5 0 0 0 0) (list R1 R2 R3 R4 R5 R6))]
         [R8 (call-stream-insn (list-ref operator-list 2) (stream-insn 2 2 0 0 0 3 0) (list R1 R2 R3 R4 R5 R6 R7))]
         [R9 (call-stream-insn (list-ref operator-list 12) (stream-insn 12 7 0 0 0 0 0) (list R1 R2 R3 R4 R5 R6 R7 R8))]
         [R10 (call-stream-insn (list-ref operator-list 10) (stream-insn 10 6 8 0 0 0 0) (list R1 R2 R3 R4 R5 R6 R7 R8 R9))]
         [R11 (call-stream-insn (list-ref operator-list 6) (stream-insn 6 1 0 0 0 1 0) (list R1 R2 R3 R4 R5 R6 R7 R8 R9 R10))]
         [R12 (call-stream-insn (list-ref operator-list 6) (stream-insn 6 0 0 0 0 0 0) (list R1 R2 R3 R4 R5 R6 R7 R8 R9 R10 R11))]
         [R13 (call-stream-insn (list-ref operator-list 14) (stream-insn 14 10 11 0 0 0 0) (list R1 R2 R3 R4 R5 R6 R7 R8 R9 R10 R11 R12))]
         [R14 (call-stream-insn (list-ref operator-list 2) (stream-insn 2 2 0 0 0 2 0) (list R1 R2 R3 R4 R5 R6 R7 R8 R9 R10 R11 R12 R13))]
         [R15 (call-stream-insn (list-ref operator-list 12) (stream-insn 12 13 0 0 0 0 0) (list R1 R2 R3 R4 R5 R6 R7 R8 R9 R10 R11 R12 R13 R14))]
         [R16 (call-stream-insn (list-ref operator-list 10) (stream-insn 10 12 14 0 0 0 0) (list R1 R2 R3 R4 R5 R6 R7 R8 R9 R10 R11 R12 R13 R14 R15))]
         [R17 (call-stream-insn (list-ref operator-list 11) (stream-insn 11 9 15 0 0 0 0) (list R1 R2 R3 R4 R5 R6 R7 R8 R9 R10 R11 R12 R13 R14 R15 R16))]
         [R18 (call-stream-insn (list-ref operator-list 5) (stream-insn 5 16 0 0 0 0 0) (list R1 R2 R3 R4 R5 R6 R7 R8 R9 R10 R11 R12 R13 R14 R15 R16 R17))]
         [R19 (call-stream-insn (list-ref operator-list 8) (stream-insn 8 17 0 0 5 0 0) (list R1 R2 R3 R4 R5 R6 R7 R8 R9 R10 R11 R12 R13 R14 R15 R16 R17 R18))])
    R19))

(define handwritten-sk (sketch (list (stream-insn 6 0 0 0 0 1 0)
                                     (stream-insn 6 1 0 0 0 0 0)
                                     (stream-insn 14 4 5 0 0 0 0)
                                     (stream-insn 2 2 0 0 0 3 0)
                                     (stream-insn 12 7 0 0 0 0 0)
                                     (stream-insn 10 6 8 0 0 0 0)
                                     (stream-insn 6 1 0 0 0 1 0)
                                     (stream-insn 6 0 0 0 0 0 0)
                                     (stream-insn 14 10 11 0 0 0 0)
                                     (stream-insn 2 2 0 0 0 2 0)
                                     (stream-insn 12 13 0 0 0 0 0)
                                     (stream-insn 10 12 14 0 0 0 0)
                                     (stream-insn 11 9 15 0 0 0 0)
                                     (stream-insn 5 16 0 0 0 0 0)
                                     (stream-insn 8 17 0 0 5 0 0)) 18 4))

(define theta0-sk (get-symbolic-sketch 15 4))

(clear-asserts!)
#;(let ([evaled-sk ((get-sketch-function theta0-sk) sym-top sym-bottom sym-stepsMovement sym-userCounter)]
      [constraints (assert (input-constraints sym-top sym-bottom sym-stepsMovement sym-userCounter))])
  (time (synthesize #:forall (symbolics (list sym-top sym-bottom sym-stepsMovement sym-userCounter))
                    #:guarantee (assert (equal? evaled-sk (handwrittenfunc-theta0 sym-top sym-bottom sym-stepsMovement sym-userCounter))))))
                    ;(assert (theta0 sym-top sym-bottom sym-stepsMovement sym-userCounter sym-stepsUpdate evaled-sk)))))
(clear-asserts!)