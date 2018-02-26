#lang rosette

(require "../rxapi.rkt")

;; constants
(define ticker-interval 3)
(define paddle-width 10)
(define paddle-speed 2)
(define canvas-width 30)
(define initial-objects '())

;; at every time interval, get the time delta between this and the last tick
(define (ticker-graph length)
  (define r1 (intervalO ticker-interval length))
  (define r2 (mapO-with-datetime (λ (dt e) (list dt 0)) r1))
  (define r3 (scanO-no-seed (λ (n m) (list (first n) (- (first n) (first m)))) r2))
  r3)

(define (keyup-proc keycode)
  (if (equal? keycode 'left)
      -1
      (if (equal? keycode 'right)
          1
          0)))
(define (keydown-proc keycode) 0)

;; send keyup and keydown events with values indicating which direction to move paddle
(define (events-graph keyup-stream keydown-stream)
  (define r1 keyup-stream)
  (define r2 keydown-stream)
  (define r3 (fromEventO keyup-stream keyup-proc))
  (define r4 (fromEventO keydown-stream keydown-proc))
  (define r5 (mergeO r1 r2))
  (define r6 (distinctUntilChangedO r3))
  r6)

(define (new-paddle-position pos inputs)
  (let* ([deltaTime (caar inputs)]
         [direction (last inputs)]
         [next (+ pos (* direction deltaTime paddle-speed))])
    (max (min next (/ (- canvas-width paddle-width) 2))
         (/ paddle-width 2))))

;; stream of paddle positions
(define (paddle-graph ticker events)
  (define r1 ticker)
  (define r2 events)
  (define r3 (withLatestFromO ticker events))
  (define r4 (scanO-seed new-paddle-position (/ paddle-width 2) r3))
  (define r5 (distinctUntilChangedO r4))
  r5)

(define (objects-state objects inputs)
  (let ([ticker (first inputs)]
        [paddle (second inputs)])
    3))
    
;; send current game state: positions of all objects, live bricks, collisions, score
(define (objects-graph ticker paddle)
  (define r1 ticker)
  (define r2 paddle)
  (define r3 (withLatestFromO r1 r2))
  (define r4 (scanO-seed objects-state initial-objects r3))
  r4)

(define (game-graph ticker paddle objects)
  (define r1 ticker)
  (define r2 paddle)
  (define r3 objects)
  (define r4 (combineLatestO r1 r2 r3))
  (define r5 (sampleO ticker-interval r4))
  ;; subscribe ????
  r5)