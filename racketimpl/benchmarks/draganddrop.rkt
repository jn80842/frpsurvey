#lang rosette/safe

(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")

;;;;;; drag and drop ;;;;;

;; assume that all mouse down events are on the draggable element
;; would b

(define i-mouse-down (list (list 3 'down)
                           (list 10 'down)))
(define i-mouse-up (list (list 6 'up)
                         (list 11 'up)))
(define i-mouse-pos (behavior (vector 0 0) (list (list 2 (vector 110 210))
                                                 (list 4 (vector 50 100))
                                                 (list 5 (vector 100 150))
                                                 (list 8 (vector 200 220))
                                                 (list 9 (vector 210 230))
                                                 (list 10 (vector 100 150))
                                                 (list 11 (vector 210 240))
                                                 (list 13 (vector 75 75)))))

(define o-element-pos (behavior (vector 100 200) (list (list 4 (vector 50 100))
                                                       (list 5 (vector 100 150))
                                                       (list 10 (vector 100 150))
                                                       (list 11 (vector 210 240)))))

;; inputs: mouse-up, mouse-down, mouse-pos (vector with 2 vals)
(define s-mouse-up '())
(define s-mouse-down '())
(define s-mouse-pos '())

;(define moveEe '()) ;; event stream of coord deltas followed by #f after mouse up
;; i believe that putting in the #f is only for memory purposes so leaving it out for now
(define (moveEe mouse-downE mouse-posB)
  (mapE (λ (e) (begin
                ; (define current-pos (valueNow mouse-posB (get-timestamp e)))
                ; (define moveEltE (mapE (λ (e) (list (get-timestamp e) #f)) (changes mouse-upB)))
                ; (define documentB (startsAtTimestamp (changes mouse-posB) (get-timestamp e)))
                 (list (get-timestamp e)
                ; (mapE (λ (e1) (list (get-timestamp e1) current-pos));(vector (- (vector-ref (get-value e1) 0)
                                                        ;         (vector-ref current-pos 0))
                                                        ;      (- (vector-ref (get-value e1) 1)
                                                        ;         (vector-ref current-pos 1)))))
                       (startAtTimestamp (get-timestamp e) (changes mouse-posB)))))
        mouse-downE))

(define (dropEe upE)
  (mapE (λ (e) (list (get-timestamp e) (zeroE))) upE))
(define (dragE moveEe dropEe)
  (switchE (mergeE moveEe dropEe)))









