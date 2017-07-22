#lang rosette/safe

(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")

;;;;;; drag and drop ;;;;;

;; assume that all mouse down events are on the draggable element

(define init-elt-pos (vector 100 200))
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

(define o-element-pos (behavior init-elt-pos (list (list 4 (vector 50 100))
                                                   (list 5 (vector 100 150))
                                                   (list 10 (vector 100 150))
                                                   (list 11 (vector 210 240)))))

;(define moveEe '()) ;; event stream of coord deltas followed by #f after mouse up
;; simplifying from flapjax ver: while dragging is active, elt has the same coords as mouse
;; i believe that putting in the #f is only for memory purposes so leaving it out for now
(define (moveEe mouse-downE mouse-posB)
  (mapE (λ (e) ;(begin
                ; (define current-pos (valueNow mouse-posB (get-timestamp e)))
                ; (define moveEltE (mapE (λ (e) (list (get-timestamp e) #f)) (changes mouse-upB)))
                ; (define documentB (startsAtTimestamp (changes mouse-posB) (get-timestamp e)))
                 (list (get-timestamp e)
                ; (mapE (λ (e1) (list (get-timestamp e1) current-pos));(vector (- (vector-ref (get-value e1) 0)
                                                        ;         (vector-ref current-pos 0))
                                                        ;      (- (vector-ref (get-value e1) 1)
                                                        ;         (vector-ref current-pos 1)))))
                       (startBehaviorAtTimestamp (get-timestamp e) mouse-posB)))
        mouse-downE))

(define (dropEe upE)
  (mapE (λ (e) (list (get-timestamp e) (zeroE))) upE))
(define (dragE moveEe dropEe)
  (switchE (mergeE moveEe dropEe)))

(define (elt-positionB mouse-up mouse-down mouse-pos init-pos)
  (startsWith (dragE (moveEe mouse-down mouse-pos) (dropEe mouse-up)) init-pos))

(printf "Checking concrete inputs ... ")
(if (equal? (dragE (moveEe i-mouse-down i-mouse-pos) (dropEe i-mouse-up))
            (changes o-element-pos))
    (printf "ok!\n")
    (printf "something's wrong\n"))

;; inputs: mouse-up, mouse-down, mouse-pos (vector with 2 vals), initial element position
(current-bitwidth 5)

(define-symbolic* init-elt-x integer?)
(define-symbolic* init-elt-y integer?)
(define s-init-elt-pos (vector init-elt-x init-elt-y))

(define (symbolic-click-event-stream symbol concrete-list)
  (map (λ (c)
         (define-symbolic* timestamp integer?)
        ; (assert (>= (length concrete-list) timestamp))
        ; (assert (> timestamp 0))
         (define-symbolic* click-evt boolean?)
         (define click-union (if click-evt symbol 'no-evt))
         (list timestamp click-union)) concrete-list))

(define s-mouse-up (symbolic-click-event-stream 'up (list 1 2 )))
(define s-mouse-down (symbolic-click-event-stream 'down (list 1 2 )))

(define (vector-event-stream concrete-list)
  (map (λ (v)
         (define-symbolic* timestamp integer?)
         ;(assert (>= (length concrete-list) timestamp))
         ;(assert (> timestamp 0))
         (define-symbolic* x integer?)
         ;(assert (>= x 0))
         (define-symbolic* y integer?)
         ;(assert (>= y 0))
         (list timestamp (vector x y))) concrete-list))
(define (vector-behavior concrete-list)
  (define-symbolic* init-x integer?)
  (assert (> init-x 0))
  (define-symbolic* init-y integer?)
  (assert (> init-y 0))
  (behavior (vector init-x init-y) (vector-event-stream concrete-list)))
         
(define s-mouse-pos (vector-behavior (list 1 2 )))

(printf "current bitwidth: ~a~n" (current-bitwidth))
(printf "length of mouse up events: ~a~n" (length s-mouse-up))
(printf "length of mouse down events: ~a~n" (length s-mouse-down))
(printf "length of changes in mouse position behavior: ~a~n" (length (changes s-mouse-pos)))

(define (drag-and-drop-assumptions mouse-up mouse-down mouse-pos init-elt-pos)
;  (assert
  (and (valid-timestamps? mouse-up)
       (valid-timestamps? mouse-down)
       (valid-behavior? mouse-pos)
       (andmap (λ (v) (and (>= (vector-ref (get-value v) 0) 0)
                           (>= (vector-ref (get-value v) 1) 0))) (behavior-changes mouse-pos))
       (>= (vector-ref init-elt-pos 0) 0)
       (>= (vector-ref init-elt-pos 1) 0)
       ))

(define (dragging-intervals mouse-down mouse-up)
  (map (λ (down-e) (list (get-timestamp down-e) (get-timestamp (findf (λ (up-e) (and (eq? (get-value up-e) 'up)
                                                                                     (<= (get-timestamp down-e) (get-timestamp up-e))))
                                                                      mouse-up))))
       (filter (λ (e) (eq? (get-value e) 'down)) mouse-down)))
(define (dropping-intervals mouse-up mouse-down)
  (map (λ (up-e) (list up-e (findf (λ (down-e) (and (eq? (get-value down-e) 'down)
                                                    (<= (get-timestamp up-e) (get-timestamp down-e))))
                                   mouse-down)))
       (filter (λ (e) (eq? (get-value e) 'up)) mouse-up)))
(define (bounded-by-events start end evt-stream)
  (if (and (not start) (not end))
      evt-stream
      (if (and start end)
          (boundedTimestampsStream (get-timestamp start) (get-timestamp end) evt-stream)
          (if start
              (startAtTimestamp (get-timestamp start) evt-stream)
              (endAtTimestamp (get-timestamp end) evt-stream)))))

(define (drag-and-drop-guarantees mouse-up mouse-down mouse-pos init-elt-pos)
  (let ([output-posB (elt-positionB mouse-up mouse-down mouse-pos init-elt-pos)])
   ; (assert
     (and (valid-behavior? output-posB)
                 (eq? init-elt-pos (behavior-init output-posB))
                 ;; until a mouse down event occurs, the element position never changes
                 (let* ([first-mouse-down (findf (λ (e) (eq? (get-value e) 'down)) mouse-down)]
                        [until-first-mouse-down-output (if first-mouse-down
                                                           (endAtTimestamp (get-timestamp first-mouse-down) (changes output-posB))
                                                           (changes output-posB))])
                   (andmap (λ (e) (eq? (get-value e) (behavior-init output-posB))) until-first-mouse-down-output))
                 ;; for every interval begun with a mouse down and ending with a mouse up
                 ;; the mouse position and the elt position is the same
                 (eq?
                  (map (λ (bounds) (boundedTimestampsStream (list-ref bounds 0) (list-ref bounds 1) (changes output-posB)))
                       (dragging-intervals mouse-down mouse-up))
                  (map (λ (bounds) (boundedTimestampsStream (list-ref bounds 0) (list-ref bounds 1) (changes mouse-pos)))
                       (dragging-intervals mouse-down mouse-up)))
                 ;; for every interval begun with a mouse up and ending with a mouse down
                 ;; the elt position does not change for its initial value
                 (andmap (λ (positions) (eq? 1 (length (remove-duplicates positions))))
                         (map (λ (pairs) (bounded-by-events (list-ref pairs 0) (list-ref pairs 1) (changes output-posB)))
                              (dropping-intervals mouse-up mouse-down)))
                 )))

(define (drag-and-drop-guarantees-debug mouse-up mouse-down mouse-pos init-elt-pos)
  (let ([output-posB (elt-positionB mouse-up mouse-down mouse-pos init-elt-pos)])
    (valid-behavior? output-posB)
    )
  )

(define solved (solve (begin
                          (assert (drag-and-drop-assumptions s-mouse-up s-mouse-down
                                                             s-mouse-pos s-init-elt-pos))
                          (assert (drag-and-drop-guarantees s-mouse-up s-mouse-down
                                                                  s-mouse-pos s-init-elt-pos)))
                        ))

(if (unsat? solved)
    (displayln "no solution for assumptions")
    (begin
      (displayln "sample solution for assumptions:")
      (displayln (evaluate s-mouse-up solved))
      (displayln (evaluate s-mouse-down solved))
      (displayln (evaluate s-mouse-pos solved))
      (displayln (elt-positionB s-mouse-up s-mouse-down s-mouse-pos s-init-elt-pos))))

(displayln "Verify drag and drop spec")
(define begin-time (current-seconds))
(define verified (verify #:assume (drag-and-drop-assumptions s-mouse-up s-mouse-down s-mouse-pos)
                         #:guarantee (drag-and-drop-guarantees s-mouse-up s-mouse-down s-mouse-pos s-init-elt-pos)))
(if (unsat? verified)
    (displayln "Spec is verified")
    (printf "Model that violates spec is found: mouse-down ~a, mouse-up ~a, mouse-pos ~a~n"
               (evaluate s-mouse-down verified)
               (evaluate s-mouse-up verified)
               (evaluate s-mouse-pos verified)))
(define end-time (current-seconds))
(printf "Took ~a seconds~n" (- end-time begin-time))










