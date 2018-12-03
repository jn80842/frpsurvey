#lang rosette

;; to find package installation location:
;; (require setup/dirs)
;; (find-pkgs-dir)
;; (find-user-pkgs-dir)

(require "../model.rkt")
(require "../fjapi.rkt")
(require "../operators.rkt")
(require "../sketch.rkt")

(current-bitwidth #f)

(define ENTER 0)
(define EXIT 1)

(define MOVEUP 0)
(define MOVEDOWN 1)
(define STOP 2)

(define MOVINGUP 0)
(define MOVINGDOWN 1)
(define STOPPED 2)

(define (get-sym-enum4)
  (define-symbolic* enum-b1 boolean?)
  (define-symbolic* enum-b2 boolean?)
  (if enum-b1 (if enum-b2 -1 0) (if enum-b2 1 2)))

(define-symbolic* top? boolean?)
;(define-symbolic* top integer?)
;(define sym-top (if top? top NOEVENT))
(define sym-top (if top? (get-sym-enum4) NOEVENT))
(define-symbolic* bottom? boolean?)
;(define-symbolic* bottom integer?)
;(define sym-bottom (if bottom? bottom NOEVENT))
(define sym-bottom (if bottom? (get-sym-enum4) NOEVENT))
;(define-symbolic* sym-stepsMovement integer?)
(define sym-stepsMovement (get-sym-enum4))
(define-symbolic* sym-userCounter integer?)
(define-symbolic* stepsUpdate? boolean?)
;(define-symbolic* stepsUpdate integer?)
;(define sym-stepsUpdate (if stepsUpdate? stepsUpdate NOEVENT))
(define sym-stepsUpdate (if stepsUpdate? (get-sym-enum4) NOEVENT))
(define-symbolic* userCounterUpdate? boolean?)
;(define-symbolic* userCounterUpdate integer?)
;(define sym-userCounterUpdate (if userCounterUpdate? userCounterUpdate NOEVENT))
(define sym-userCounterUpdate (if userCounterUpdate? (get-sym-enum4) NOEVENT))

#;(define (print-sensor s)
  (cond [(equal? s ENTER) "ENTER"]
        [(equal? s EXIT) "EXIT"]
        [else s]))

#;(define (print-stepsMovement s)
  (cond [(equal? s MOVINGUP) "MOVINGUP"]
        [(equal? s MOVINGDOWN) "MOVINGDOWN"]
        [(equal? s STOPPED) "STOPPED"]
        [else s]))
#;(define (print-stepsUpdate s)
  (cond [(equal? s MOVEUP) "MOVEUP"]
        [(equal? s MOVEDOWN) "MOVEDOWN"]
        [(equal? s STOP) "STOP"]
        [else s]))

#;(define (print-model m)
  (if (unsat? m)
      (format "UNSAT")
      (begin (println (format "topSensor: ~a" (if (evaluate top? m) (print-sensor (evaluate top m)) "NOEVENT")))
             (println (format "bottomSensor: ~a" (if (evaluate bottom? m) (print-sensor (evaluate bottom m)) "NOEVENT")))
             (println (format "stepsMovement: ~a" (print-stepsMovement (evaluate sym-stepsMovement m))))
             (println (format "userCounter: ~a" (evaluate sym-userCounter m)))
             (println (format "stepsUpdate: ~a" (if (evaluate stepsUpdate? m) (print-stepsUpdate (evaluate stepsUpdate m)) "NOEVENT")))
             (println (format "userCounterUpdate: ~a" (if (evaluate userCounterUpdate? m) (evaluate userCounterUpdate m) "NOEVENT"))))))

(define (iff p q)
  (or (and p q) (and (not p) (not q))))

(define (call-constraint constraint)
  (constraint sym-top sym-bottom sym-stepsMovement sym-userCounter sym-stepsUpdate sym-userCounterUpdate))

;; psi
;; original version of psi says that certain events cannot happen simultaneously
;; steps cannot move up and down at the same time
;; top/bottom cannot sense an entrance and exit at the same time
;; our psi says that our sym vars are enumerations, basically
(define (psi topSensor bottomSensor stepsMovement userCounter stepsUpdate userCounterUpdate)
  (and (or (equal? topSensor ENTER) (equal? topSensor EXIT) (empty-event? topSensor))
       (or (equal? bottomSensor ENTER) (equal? bottomSensor EXIT) (empty-event? bottomSensor))
       (or (equal? stepsMovement MOVINGUP) (equal? stepsMovement MOVINGDOWN) (equal? stepsMovement STOPPED))
       (>= userCounter 0)
       (or (equal? stepsUpdate MOVEUP) (equal? stepsUpdate MOVEDOWN) (equal? stepsUpdate STOP) (empty-event? stepsUpdate))
       (or (equal? userCounterUpdate 1) (equal? userCounterUpdate -1) (empty-event? userCounterUpdate))))

;; additional constraints to rule out illegal inputs
(define (illegal-state topSensor bottomSensor stepsMovement userCounter)
       ;; if steps are stopped, the user count must be 0
  (and (not (and (equal? stepsMovement STOPPED) (not (equal? userCounter 0))))
       ;; if steps are moving, the user count cannot be 0
       (not (and (not (equal? stepsMovement STOPPED)) (equal? userCounter 0)))
       ;; if the user count is 0, there can't be an EXIT
       (iff (equal? userCounter 0) (and (not (equal? topSensor EXIT)) (not (equal? bottomSensor EXIT))))))

;; theta zero
;; ((enterEvent(bottom) && !exitEvent(top) && !(steps == MOVEDOWN))
;; || (enterEvent(top) && !exitEvent(bottom) && !(steps == MOVEUP)))
;; <-> users := users + 1
(define (theta0 topSensor bottomSensor stepsMovement userCounter stepsUpdate userCounterUpdate)
  (iff (or (and (equal? bottomSensor ENTER) (or (empty-event? topSensor) (not (equal? topSensor EXIT))) (not (equal? stepsMovement MOVINGDOWN)))
           (and (equal? topSensor ENTER) (or (empty-event? bottomSensor) (not (equal? bottomSensor EXIT))) (not (equal? stepsMovement MOVINGUP))))
       (equal? userCounterUpdate 1)))

;; theta one
;; ((exitEvent(top) && !enterEvent(bottom) && !(steps == MOVEDOWN))
;; || (exitEvent(bottom) && !enterEvent(top) && !(steps == MOVEUP)))
;; <-> users := users - 1
(define (theta1 topSensor bottomSensor stepsMovement userCounter stepsUpdate userCounterUpdate)
  (iff (or (and (equal? topSensor EXIT) (not (equal? bottomSensor ENTER)) (not (equal? stepsMovement MOVINGDOWN)))
           (and (equal? bottomSensor EXIT) (not (equal? topSensor ENTER)) (not (equal? stepsMovement MOVINGUP))))
       (equal? userCounterUpdate -1)))

;; theta two
;; (stepMovementChangeE == MOVEUP) --> userCounterB == 0 && enterEvent(bottom)
(define (theta2 topSensor bottomSensor stepsMovement userCounter stepsUpdate userCounterUpdate)
  (implies (equal? stepsUpdate MOVEUP) (and (equal? userCounter 0) (equal? bottomSensor ENTER))))

;; theta three
;; (stepMovementChangeE == MOVEDOWN) --> userCounterB == 0 && enterEvent(top)
(define (theta3 topSensor bottomSensor stepsMovement userCounter stepsUpdate userCounterUpdate)
  (implies (equal? stepsUpdate MOVEDOWN) (and (equal? userCounter 0) (equal? topSensor ENTER))))

;; theta four
;; userCounterB == 0 && enterEvent(bottom) && !enterEvent(top) --> stepsMovementChangeE == MOVEUP
(define (theta4 topSensor bottomSensor stepsMovement userCounter stepsUpdate userCounterUpdate)
  (implies (and (equal? userCounter 0) (equal? bottomSensor ENTER) (not (equal? topSensor ENTER)))
           (equal? stepsUpdate MOVEUP)))

;; theta five
;; userCounterB == 0 && enterEvent(top) && !enterEvent(bottom) --> stepsMovementChangeE == MOVEDOWN
(define (theta5 topSensor bottomSensor stepsMovement userCounter stepsUpdate userCounterUpdate)
  (implies (and (equal? userCounter 0) (equal? topSensor ENTER) (not (equal? bottomSensor ENTER)))
           (equal? stepsUpdate)))

;; theta six
;; userCounterB == 0 && enterEvent(top) && enterEvent(bottom) --> stepsMovementChangeE == MOVEDOWN || stepsMovementChangeE == MOVEUP
(define (theta6 topSensor bottomSensor stepsMovement userCounter stepsUpdate userCounterUpdate)
  (implies (and (equal? userCounter 0) (equal? topSensor ENTER) (equal? bottomSensor ENTER))
           (or (equal? stepsUpdate MOVEUP) (equal? stepsUpdate MOVEDOWN))))

;; theta seven
;; ! users == 0 && X (users == 0 && ! enterEvent(top) && ! enterEvent(bottom)) <-> steps := STOP
;; we don't have access to the next steps, so we alter this spec a little
(define (theta7 topSensor bottomSensor stepsMovement userCounter stepsUpdate userCounterUpdate)
  (iff (and (not (equal? stepsMovement STOPPED)) (equal? userCounter 0) (not (equal? topSensor ENTER)) (not (equal? bottomSensor ENTER)))
       (equal? stepsUpdate STOP)))

;; while escalator is not moving down, entrance from the bottom increments user count if there is no simultaneous exit from top
;; 6 operators
(define (inc-moving-up topSensor bottomSensor stepsMovement userCounter)
  (constantE 1 (andE (maskOffE (filterE (λ (e) (equal? e EXIT)) topSensor)
                               (filterE (λ (e) (equal? e ENTER)) bottomSensor))
                     (mapE (λ (e) (not (equal? e MOVINGDOWN))) stepsMovement))))

(define (inc-moving-up-reg topSensor bottomSensor stepsMovement userCounter)
  (define r1 topSensor)
  (define r2 bottomSensor)
  (define r3 stepsMovement)
  (define r4 userCounter)
  (define r5 (mapE (λ (e) (not (equal? e MOVINGDOWN))) r3))
  (define r6 (filterE (λ (e) (equal? e EXIT)) r1))
  (define r7 (filterE (λ (e) (equal? e ENTER)) r2))
  (define r8 (maskOffE r6 r7))
  (define r9 (andE r5 r8))
  (define r10 (constantE 1 r9))
  r10)
;; similarly when escalator is moving up
;; 6 operators
(define (inc-moving-down topSensor bottomSensor stepsMovement userCounter)
  (constantE 1 (andE (maskOffE (filterE (λ (e) (equal? e EXIT)) bottomSensor)
                               (filterE (λ (e) (equal? e ENTER)) topSensor))
                     (mapE (λ (e) (not (equal? e MOVINGUP))) stepsMovement))))
;; 13 operators
(define (inc-users topSensor bottomSensor stepsMovement userCounter)
  (mergeE (inc-moving-up topSensor bottomSensor stepsMovement userCounter)
          (inc-moving-down topSensor bottomSensor stepsMovement userCounter)))

;; while escalator is not moving down, exit from the top decrements user count if there is no simultaneous entrance from bottom
;; 6 operators
(define (dec-moving-down topSensor bottomSensor stepsMovement userCounter)
  (constantE -1 (andE (maskOffE (filterE (λ (e) (equal? e ENTER)) bottomSensor)
                                (filterE (λ (e) (equal? e EXIT)) topSensor))
                      (mapE (λ (e) (not (equal? e MOVINGDOWN))) stepsMovement))))
;; similarly when escalator is moving up
;; 6 operators
(define (dec-moving-up topSensor bottomSensor stepsMovement userCounter)
  (constantE -1 (andE (maskOffE (filterE (λ (e) (equal? e ENTER)) topSensor)
                                (filterE (λ (e) (equal? e EXIT)) bottomSensor))
                      (mapE (λ (e) (not (equal? e MOVINGUP))) stepsMovement))))
;; 13 operators
(define (dec-users topSensor bottomSensor stepsMovement userCounter)
  (mergeE (dec-moving-down topSensor bottomSensor stepsMovement userCounter)
          (dec-moving-up topSensor bottomSensor stepsMovement userCounter)))
;; 27 operators
(define (get-userCounterUpdateE topSensor bottomSensor stepsMovement userCounter)
  (mergeE (inc-users topSensor bottomSensor stepsMovement userCounter)
          (dec-users topSensor bottomSensor stepsMovement userCounter)))

;; sending signal to escalator to MOVEDOWN
;; 6 operators
(define (send-move-down topSensor bottomSensor stepsMovement userCounter)
  (constantE MOVEDOWN (maskOffE (filterE (λ (e) (equal? e ENTER)) (maskOnE (mapE (λ (e) (equal? e 0)) userCounter)
                                                                           bottomSensor))
                                (filterE (λ (e) (equal? e ENTER)) (maskOnE (mapE (λ (e) (equal? e 0)) userCounter)
                                                                           topSensor)))))
;; sending signal to escalator to MOVEUP

;; 6 operators
(define (send-move-up topSensor bottomSensor stepsMovement userCounter)
  (constantE MOVEUP (maskOffE (filterE (λ (e) (equal? e ENTER)) (maskOnE (mapE (λ (e) (equal? e 0)) userCounter)
                                                                         topSensor))
                              (filterE (λ (e) (equal? e ENTER)) (maskOnE (mapE (λ (e) (equal? e 0)) userCounter)
                                                                         bottomSensor)))))
;; sending signal to escalator to STOP
;; 7 operators
(define (send-stop topSensor bottomSensor stepsMovement userCounter)
  (constantE STOP (maskOffE (filterE (λ (e) (equal? e ENTER)) (mergeE bottomSensor topSensor))
                            (andE (mapE (λ (e) (equal? e 0)) userCounter) (mapE (λ (e) (not (equal? e STOPPED))) stepsMovement)))))

;; 21 operators
(define (get-stepsUpdateE topSensor bottomSensor stepsMovement userCounter)
  (mergeE (send-move-down topSensor bottomSensor stepsMovement userCounter)
          (mergeE (send-move-up topSensor bottomSensor stepsMovement userCounter)
                  (send-stop topSensor bottomSensor stepsMovement userCounter))))

;; verify that handwritten solution conforms to specs
(define m (let ([userCounterUpdateE (get-userCounterUpdateE sym-top sym-bottom sym-stepsMovement sym-userCounter)]
                [stepsUpdateE (get-stepsUpdateE sym-top sym-bottom sym-stepsMovement sym-userCounter)])
            (verify #:assume (assert (and (psi sym-top sym-bottom sym-stepsMovement sym-userCounter stepsUpdateE userCounterUpdateE) (illegal-state sym-top sym-bottom sym-stepsMovement sym-userCounter stepsUpdateE userCounterUpdateE)))
                    #:guarantee (assert (and (theta0 sym-top sym-bottom sym-stepsMovement sym-userCounter stepsUpdateE userCounterUpdateE)
                                             (theta1 sym-top sym-bottom sym-stepsMovement sym-userCounter stepsUpdateE userCounterUpdateE)
                                             (theta2 sym-top sym-bottom sym-stepsMovement sym-userCounter stepsUpdateE userCounterUpdateE)
                                             (theta3 sym-top sym-bottom sym-stepsMovement sym-userCounter stepsUpdateE userCounterUpdateE)
                                             (theta4 sym-top sym-bottom sym-stepsMovement sym-userCounter stepsUpdateE userCounterUpdateE)
                                             (theta5 sym-top sym-bottom sym-stepsMovement sym-userCounter stepsUpdateE userCounterUpdateE)
                                             (theta6 sym-top sym-bottom sym-stepsMovement sym-userCounter stepsUpdateE userCounterUpdateE)
                                             (theta7 sym-top sym-bottom sym-stepsMovement sym-userCounter stepsUpdateE userCounterUpdateE))))))

;; synthesis experiments:
;; synthesize based on paper specs

;; synthesize userCounter increment program
(define userCounterIncUp-sk (get-symbolic-sketch 6 4))
(define evaled-userCounterIncUp-sk ((get-sketch-function userCounterIncUp-sk) sym-top sym-bottom sym-stepsMovement sym-userCounter))
;; took 120s with only ref impl as spec
;; took 533s with both i/o and ref impl as spec
;; took 5s with only i/o
(define b (time (synthesize #:forall (symbolics (list sym-top sym-bottom sym-stepsMovement sym-userCounter))
                            #:guarantee (assert ;(and ;(equal? ((get-sketch-function userCounterIncUp-sk) NOEVENT ENTER MOVINGUP 0) 1)
                                                     ;(equal? ((get-sketch-function userCounterIncUp-sk) NOEVENT ENTER MOVINGUP 1) 1)
                                                     ;(equal? ((get-sketch-function userCounterIncUp-sk) NOEVENT ENTER MOVINGUP 2) 1)
                                                     ;(equal? ((get-sketch-function userCounterIncUp-sk) EXIT NOEVENT MOVINGUP 1) NOEVENT)
                                                     ;(equal? ((get-sketch-function userCounterIncUp-sk) EXIT ENTER MOVINGUP 1) NOEVENT)
                                                     ;(equal? ((get-sketch-function userCounterIncUp-sk) EXIT ENTER MOVINGUP 1) NOEVENT)
                                                     ;(equal? ((get-sketch-function userCounterIncUp-sk) EXIT ENTER MOVINGDOWN 1) NOEVENT)
                                         (equal? evaled-userCounterIncUp-sk (inc-moving-up sym-top sym-bottom sym-stepsMovement sym-userCounter))
                                         ))))
(clear-asserts!)
;(define userCounterInc-sk (get-symbolic-sketch 13 4))
;(define evaled-userCounterInc-sk ((get-sketch-function userCounterInc-sk) sym-top sym-bottom sym-stepsMovement sym-userCounter))
#;(define b (time (synthesize #:forall (symbolics (list sym-top sym-bottom sym-stepsMovement sym-userCounter))
                            #:guarantee (assert (equal? evaled-userCounterInc-sk (inc-users sym-top sym-bottom sym-stepsMovement sym-userCounter))))))
;; synthesized answer
(define (synthesized-function input1 input2 input3 input4)
  (define r1 input1)
  (define r2 input2)
  (define r3 input3)
  (define r4 input4)
  (define r5 (mapE (λ (i) (and (>= i 1) (<= i 1))) r1))
  (define r6 (mapE (λ (i) (or (>= i 2) (<= i 0))) r3))
  (define r7 (mapE (λ (i) (and (>= i 0) (<= i 0))) r2))
  (define r8 (andE r6 r7))
  (define r9 (maskOffE r5 r8))
  (define r10 (constantE 1 r9))
  r10)
(clear-asserts!)
(define concrete-sk (sketch (list (stream-insn 6 2 0 0 1 1 0)
                                  (stream-insn 3 0 0 0 0 1 0)
                                  (stream-insn 3 1 0 0 0 0 0)
                                  (stream-insn 7 5 6 0 0 0 0)
                                  (stream-insn 0 4 7 0 0 0 0)
                                  (stream-insn 1 8 0 0 0 1 0))
                            9 4))

;; synthesize userCounterUpdate program
(define userCounter-sk(get-symbolic-sketch 27 4))

#;(define synthed-userCounter
  (let ([evaled-userCounter-sk ((get-sketch-function userCounter-sk) sym-top sym-bottom sym-stepsMovement sym-userCounter)])
    (time (synthesize #:forall (symbolics (list sym-top sym-bottom sym-stepsMovement sym-userCounter))
                      #:guarantee (assert (and (theta0 sym-top sym-bottom sym-stepsMovement sym-userCounter NOEVENT evaled-userCounter-sk)
                                               (theta1 sym-top sym-bottom sym-stepsMovement sym-userCounter NOEVENT evaled-userCounter-sk)))))))
;; synthesize stepsUpdate program
(define stepsUpdate-sk (get-symbolic-sketch 21 4))
;; synthesize based on input/output traces
;; synthesize based on invariants not present in paper specs