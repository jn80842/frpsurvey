#lang rosette/safe

(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")

;;;;;; drag and drop ;;;;;

;; assume that all mouse down events are on the draggable element


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

;(define moveEe '()) ;; event stream of coord deltas followed by #f after mouse up
;; simplifying from flapjax ver: while dragging is active, elt has the same coords as mouse
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
(define (elt-positionB mouse-up mouse-down mouse-pos)
  (startsWith (dragE (moveEe mouse-down mouse-pos) (dropEe mouse-up)) (behavior-init mouse-pos)))

(printf "Checking concrete inputs ... ")
(if (equal? (dragE (moveEe i-mouse-down i-mouse-pos) (dropEe i-mouse-up))
            (changes o-element-pos))
    (printf "ok!\n")
    (printf "something's wrong\n"))

;; inputs: mouse-up, mouse-down, mouse-pos (vector with 2 vals)
(current-bitwidth 7)

(define (symbolic-click-event-stream symbol concrete-list)
  (map (λ (c)
         (define-symbolic* timestamp integer?)
         (assert (>= (length concrete-list) timestamp))
         (assert (> timestamp 0))
         (define-symbolic* click-evt boolean?)
         (define click-union (if click-evt symbol 'no-evt))
         (list timestamp click-union)) concrete-list))

(define s-mouse-up (symbolic-click-event-stream 'up (list 1 2 3)))
(define s-mouse-down (symbolic-click-event-stream 'down (list 1 2 3)))

(define (vector-event-stream concrete-list)
  (map (λ (v)
         (define-symbolic* timestamp integer?)
         (assert (>= (length concrete-list) timestamp))
         (assert (> timestamp 0))
         (define-symbolic* x integer?)
         (assert (>= x 0))
         (define-symbolic* y integer?)
         (assert (>= y 0))
         (list timestamp (vector x y))) concrete-list))
(define (vector-behavior concrete-list)
  (define-symbolic* init-x integer?)
  (assert (> init-x 0))
  (define-symbolic* init-y integer?)
  (assert (> init-y 0))
  (behavior (vector init-x init-y) (vector-event-stream concrete-list)))
         
(define s-mouse-pos (vector-behavior (list 1 2 3)))

(printf "current bitwidth: ~a~n" (current-bitwidth))
(printf "length of mouse up events: ~a~n" (length s-mouse-up))
(printf "length of mouse down events: ~a~n" (length s-mouse-down))
(printf "length of changes in mouse position behavior: ~a~n" (length (changes s-mouse-pos)))

(define (drag-and-drop-assumptions mouse-up mouse-down mouse-pos)
  (assert (and (valid-timestamps? mouse-up)
               (valid-timestamps? mouse-down)
               (valid-behavior? mouse-pos))))

(define solved (solve (drag-and-drop-assumptions s-mouse-up s-mouse-down s-mouse-pos)))

(if (unsat? solved)
    (displayln "no solution for assumptions")
    (begin
      (displayln "sample solution for assumptions:")
      (displayln (evaluate s-mouse-up solved))
      (displayln (evaluate s-mouse-down solved))
      (displayln (evaluate s-mouse-pos solved))))

(define (drag-and-drop-guarantees mouse-up mouse-down mouse-pos)
  (let ([output-posB (elt-positionB mouse-up mouse-down mouse-pos)])
    (assert (and (valid-behavior? output-posB)
                 #t))))

(displayln "Verify drag and drop spec\n")
(define begin-time (current-seconds))
(define verified (verify #:assume (drag-and-drop-assumptions s-mouse-up s-mouse-down s-mouse-pos)
                         #:guarantee (drag-and-drop-guarantees s-mouse-up s-mouse-down s-mouse-pos)))
(if (unsat? verified)
    (displayln "Spec is verified")
    (printf "Model that violates spec is found: mouse-down ~a, mouse-up ~a, mouse-pos ~a~n"
               (evaluate s-mouse-down verified)
               (evaluate s-mouse-up verified)
               (evaluate s-mouse-pos verified)))
(define end-time (current-seconds))
(printf "Took ~a seconds~n" (- end-time begin-time))

#;(define (valid-behavior? b)
  (and (behavior? b)
       (apply distinct? (map get-timestamp (behavior-changes b)))
       (andmap positive? (map get-timestamp (behavior-changes b)))
       (timestamps-sorted? (behavior-changes b))))









