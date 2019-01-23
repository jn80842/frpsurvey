#lang rosette

(require "api.rkt")
(require "operators.rkt")
(require "sketch.rkt")

;; (current-bitwidth 3)

(define ENTER (bv 0 (bitvector 2)))
(define EXIT (bv 1 (bitvector 2)))
(define NONE (bv 2 (bitvector 2)))

(define MOVEUP (bv 0 (bitvector 2)))
(define MOVEDOWN (bv 1 (bitvector 2)))
(define STOP (bv 2 (bitvector 2)))

(define MOVINGUP (bv 0 (bitvector 2)))
(define MOVINGDOWN (bv 1 (bitvector 2)))
(define STOPPED (bv 2 (bitvector 2)))

(define EMPTY (bv 0 (bitvector 1)))
(define OCCUPIED (bv 1 (bitvector 1)))

(define INCBYONE (bv 0 (bitvector 2)))
(define DECBYONE (bv 1 (bitvector 2)))
(define NOUPDATE (bv 2 (bitvector 2)))

(define-symbolic* sym-top (bitvector 2))
(define-symbolic* sym-bottom (bitvector 2))
(define-symbolic* sym-stepsMovement (bitvector 2))

;; theta zero
;; ((enterEvent(bottom) && !exitEvent(top) && !(steps == MOVEDOWN))
;; || (enterEvent(top) && !exitEvent(bottom) && !(steps == MOVEUP)))
;; <-> users := users + 1
(define (theta0 topSensor bottomSensor stepsMovement userCounterUpdate)
    (iff (or (and (equal? bottomSensor ENTER)
                  (not (equal? topSensor EXIT))
                  (not (equal? stepsMovement MOVINGDOWN)))
           (and (equal? topSensor ENTER)
                (not (equal? bottomSensor EXIT))
                (not (equal? stepsMovement MOVINGUP))))
       (equal? userCounterUpdate INCBYONE)))

;; theta one
;; ((exitEvent(top) && !enterEvent(bottom) && !(steps == MOVEDOWN))
;; || (exitEvent(bottom) && !enterEvent(top) && !(steps == MOVEUP)))
;; <-> users := users - 1
(define (theta1 topSensor bottomSensor stepsMovement userCounterUpdate)
  (iff (or (and (equal? topSensor EXIT)
                (not (equal? bottomSensor ENTER))
                (not (equal? stepsMovement MOVINGDOWN)))
           (and (equal? bottomSensor EXIT)
                (not (equal? topSensor ENTER))
                (not (equal? stepsMovement MOVINGUP))))
       (equal? userCounterUpdate DECBYONE)))

(define sk (get-symbolic-sketch 15 3))
(define evaled-sk ((get-sketch-function sk) sym-top sym-bottom sym-stepsMovement))

;; rule out bv values that have no meaning for our enumerations
(assert (not (equal? sym-top (bv 3 (bitvector 2)))))
(assert (not (equal? sym-bottom (bv 3 (bitvector 2)))))

#;(define b (time (synthesize #:forall (list sym-top sym-bottom sym-stepsMovement)
                            #:guarantee (assert (theta0 sym-top sym-bottom sym-stepsMovement evaled-sk)))))

(define-symbolic f (~> (bitvector 2) (bitvector 2) (bitvector 2) (bitvector 2)))

(println "synthesizing LUT for specs")

(define b (time (synthesize  #:forall (list sym-top sym-bottom sym-stepsMovement)
                             #:guarantee (begin (assert (theta0 sym-top sym-bottom sym-stepsMovement
                                                                (f sym-top sym-bottom sym-stepsMovement)))
                                                (assert (theta1 sym-top sym-bottom sym-stepsMovement
                                                                (f sym-top sym-bottom sym-stepsMovement)))
                                                (assert (not (equal? THREE (f sym-top sym-bottom sym-stepsMovement))))))))

(define g (evaluate f b))

(println "synthesizing FRP program for LUT")

(current-bitwidth 3)

(define b1 (time (synthesize #:forall (list sym-top sym-bottom sym-stepsMovement)
                             #:guarantee (assert (equal? (g sym-top sym-bottom sym-stepsMovement)
                                                         evaled-sk)))))