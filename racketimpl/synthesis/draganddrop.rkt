#lang rosette/safe

(require rosette/lib/synthax)
(require "../fjmodels.rkt")
(require "../rosettefjapi.rkt")
(require "../benchmarks/draganddrop.rkt")
(require "grammar.rkt")

(current-bitwidth 5)
(printf "Current bitwidth: ~a~n" (current-bitwidth))

(define-synthax (evts-grmr evt-stream depth)
  #:base evt-stream
  #:else (choose evt-stream
                 (zeroE)
                 (mapE (choose (λ (e) 
                                 (list (get-timestamp e)
                                       (startBehaviorAtTimestamp (get-timestamp e) mouse-posB)))
                               (λ (e) (list (get-timestamp e) (zeroE)))) (evts-grmr evt-stream (sub1 depth)))
                 (mergeE (evts-grmr evt-stream (sub1 depth))
                 ))

(define-synthax (stream-of-streams-grmr input depth)
  #:base input
  #:else (choose input
                 (switchE input (sub1 depth))))

  (define-synthax (behavior-grmr input depth)
    #:base input
    #:else (choose input
                   (startsWith (behavior-grmr input (sub1 depth)) 
                 
                 

(define (moveEe mouse-downE mouse-posB)
  (mapE (λ (e) 
          (list (get-timestamp e)
                (startBehaviorAtTimestamp (get-timestamp e) mouse-posB)))
        mouse-downE))

(define (dropEe upE)
  (mapE (λ (e) (list (get-timestamp e) (zeroE))) upE))
(define (dragE moveEe dropEe)
  (switchE (mergeE moveEe dropEe)))

(define (elt-positionB mouse-up mouse-down mouse-pos init-pos)
  (startsWith (dragE (moveEe mouse-down mouse-pos) (dropEe mouse-up)) init-pos))

(define stream-length 3)

(define s-mouse-up (symbolic-click-event-stream 'up stream-length))
(define s-mouse-down (symbolic-click-event-stream 'down stream-length))
(define s-mouse-pos (coord-behavior stream-length))
(define-symbolic* init-elt-x integer?)
(define-symbolic* init-elt-y integer?)
(define s-init-elt-pos (vector init-elt-x init-elt-y))
