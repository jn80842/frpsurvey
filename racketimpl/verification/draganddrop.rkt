#lang rosette/safe

(require rosette/query/debug rosette/lib/render)

(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")
(require "../benchmarks/draganddrop/rkt")

;;;;;; drag and drop ;;;;;

;; assume that all mouse down events are on the draggable element
;; since the source of the mouse down events should be from that element
;; mouse up events are from the entire document

(define init-elt-pos (vector 100 200))
(define i-mouse-down (list (list 3 'down)
                           (list 10 'down)))
(define i-mouse-up (list (list 6 'up)
                         (list 11 'up)))

(define i-mouse-pos (behavior (vector 0 0) (list (list 2 (vector 100 200))
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
  (startsWith init-pos (dragE (moveEe mouse-down mouse-pos) (dropEe mouse-up))))

(printf "Checking concrete inputs ... ")
(if (equal-behaviors? ;(dragE (moveEe i-mouse-down i-mouse-pos) (dropEe i-mouse-up))
     (elt-positionB i-mouse-up i-mouse-down i-mouse-pos init-elt-pos)
            o-element-pos)
    (printf "ok!\n")
    (printf "something's wrong\n"))

(define s-init-elt-pos (sym-coords))

(if (>= max-timestamp (sub1 (expt 2 (sub1 (current-bitwidth)))))
    (displayln "MAX TIMESTAMP IS TOO HIGH and WILL CAUSE OVERFLOW")
    (printf "max timestamp is ~a~n" max-timestamp))

(define s-mouse-up (new-event-stream (sym-union-constructor 'up 'no-evt) stream-length))
(define s-mouse-down (new-event-stream (sym-union-constructor 'down 'no-evt) stream-length))

(define s-mouse-pos (new-behavior sym-coords stream-length))

(printf "current bitwidth ~a, maximum possible value is ~a~n"
        (current-bitwidth) (max-for-current-bitwidth (current-bitwidth)))
(printf "length of mouse up events: ~a~n" (length s-mouse-up))
(printf "length of mouse down events: ~a~n" (length s-mouse-down))
(printf "length of changes in mouse position behavior: ~a~n" (length (changes s-mouse-pos)))

(define (click-sequence state transition)
  (cond [(and (eq? 'waiting-for-up state) (eq? 'up (get-value transition))) 'waiting-for-down]
        [(and (eq? 'waiting-for-down state) (eq? 'down (get-value transition))) 'waiting-for-up]
        [else #f]))

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

(check-existence-of-solution drag-and-drop-assumptions s-mouse-up s-mouse-down s-mouse-pos s-init-elt-pos)

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










