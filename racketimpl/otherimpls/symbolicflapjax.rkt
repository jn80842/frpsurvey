#lang rosette/safe

;; each item in list is a logical timestep
;; an event stream is a list containing either values or #f (to represent no event)
;; a behavior is a list of values (values must be preset at each timestep)

(define (clicksE concrete-list)
  (map (λ (c) (define-symbolic* click boolean?)
         click) concrete-list))
(define (constE s-events const)
  (map (λ (c) (if c const c)) s-events))
(define (mergeE events1 events2)
  (map (λ (e1 e2) (if e1 e1 e2)) events1 events2))
(define (startsWith s-events init)
  (letrec ([incrfold (λ (x-lst p)
                       (if (equal? (length x-lst) 0)
                           '()
                       (if (car x-lst)
                           (append (list (car x-lst)) (incrfold (cdr x-lst) (car x-lst)))
                           (append (list p) (incrfold (cdr x-lst) p)))))])
   (append (list init) (incrfold s-events init))))
    
(define (collectE s-behavior init proc)
  (letrec ([collect (λ (x-lst init)
                      (if (equal? (length x-lst) 0)
                          '()
                          (append (list (if (car x-lst) (proc init (car x-lst)) init)) (collect (cdr x-lst) (proc init (car x-lst))))))])
    (collect s-behavior init)))

(define (graph inc-clicks dec-clicks)
  (startsWith (collectE (mergeE (constE inc-clicks 1) (constE dec-clicks -1)) 0 +) 0))

(displayln (graph (clicksE (list 1)) (clicksE (list 1))))

(define inc-clicks (clicksE (list 1 2 3)))
(define dec-clicks (clicksE (list 1 2 3)))

(define sol (solve (assert (equal? (last (graph inc-clicks dec-clicks)) -1))))
(displayln (evaluate inc-clicks sol))
(displayln (evaluate dec-clicks sol))

(define inc-clicks2 (clicksE (list 1 2 3)))
(define dec-clicks2 (clicksE (list 1 2 3)))

(define (first-always-0 inc dec)
  (assert (= (first (graph inc dec)) 0)))

(define sol2 (verify (first-always-0 inc-clicks2 dec-clicks2)))

;;;;;;;;;;;;;;;;;;;;;;;
;;;; IoT example ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;


;; isNightBehavior = liftB(λ(b) (or (< b 8) (> b 20)), clockBehavior)
;; isHomeBehavior = liftB(λ(b) (equal? phoneBehavior 'home), phoneBehavior)
;; modeBehavior = condB([isNightBehavior, constB('night)],[isHomeBehavior, constB('home)],[constB(#t),constB('away)])

;(define (liftB proc behavior)

(define (mode-graph phone-behavior clock-behavior)
  (let* ([isNightBehavior (or (< clock-behavior 8) (> clock-behavior 20))]
        [isHomeBehavior (equal? phone-behavior 'home)]
        [modeBehavior (cond [isNightBehavior 'night]
                            [isHomeBehavior 'home]
                            [else 'away])])
    modeBehavior))
        


;; input: motion sensor event stream
;; output: light behavior








