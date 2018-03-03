#lang rosette/safe

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")

(provide (all-defined-out))

;; (allows live changes to program graph)
;;
;;                startsWith
;;                /         \
;;             init-pos    switchE
;;                           |
;;                         mergeE
;;                      /          \
;;                 mapE               mapE
;;             /       \            /       \
;; (λ) (mouse-pos) mouse-down   (λ) (zeroE) mouse-up

;; (no live changes to program graph)
;;
;;                 mapE2
;;               /         \
;;           snapshotE     mousePos
;;          /        \
;;    startsWith    mousePos
;;    /         \
;;   #f         mergeE
;;           /          \
;;     constantE      constantE
;;     /      \       /       \
;;    #f   mouseUp   #t    mouseDown

;(current-bitwidth 5)
(define stream-length 3)
(define max-timestamp (* 2 stream-length))
(define max-mouse-pos 3)

;;(struct coords (x y) #:transparent)

(define (sym-coords)
  (define-symbolic* x integer?)
;  (assert (>= x 0))
;  (assert (<= x max-mouse-pos))
  (define-symbolic* y integer?)
;  (assert (>= y 0))
;  (assert (<= y max-mouse-pos))
  (coords x y))

#;(define (harvest-coords c)
  (list (car (first (union-contents c)))
        (coords-x (cdr (first (union-contents c))))
        (coords-y (cdr (first (union-contents c))))))

(define (harvest-coords-stream clist)
  (apply append (map harvest-coords clist)))

(define s-mouse-up (new-event-stream (λ () 'click) stream-length))
(define s-mouse-down (new-event-stream (λ () 'click) stream-length))
(define s-mouse-pos (new-event-stream sym-coords stream-length))

(define concrete-mouse-up '(no-evt no-evt no-evt click no-evt))
(define concrete-mouse-down '(no-evt click no-evt no-evt no-evt))
(define concrete-mouse-pos (list (coords 4 4) (coords 5 5) (coords 5 6) (coords 2 3) (coords 1 4)))

(define (drag-and-drop-graph mouse-up mouse-down mouse-pos)
  (mapE2 (λ (x y) (if x y 'no-evt)) (snapshotE mouse-pos (startsWith #f (mergeE (constantE #f mouse-up) (constantE #t mouse-down))))
          mouse-pos))

#;(define (drag-and-drop-assumptions mouse-up mouse-down mouse-pos init-elt-pos)
  (let ([actual-ups (filter (λ (e) (not (eq? 'no-evt (get-value e)))) mouse-up)]
        [actual-downs (filter (λ (e) (not (eq? 'no-evt (get-value e)))) mouse-down)])
    (and (valid-timestamps? mouse-up)
         (valid-timestamps? mouse-down)
         ;; no up and down can occur at the same time
         (apply distinct? (append (map get-timestamp actual-ups) (map get-timestamp actual-downs)))
         ;; every up has to be followed by a down (and not a second up)
         ;; if down is followed by anything, it has to be followed by an up
         (foldl
          (λ (state transition) (cond [(and (eq? 'waiting-for-up state) (eq? 'up (get-value transition))) 'waiting-for-down]
                                      [(and (eq? 'waiting-for-down state) (eq? 'down (get-value transition))) 'waiting-for-up]
                                      [else #f]))
          #t
          (sort (append actual-ups actual-downs) (λ (e1 e2) (< (get-timestamp e1) (get-timestamp e2)))))
         (valid-behavior? mouse-pos)
         ;; initial placement of element must be >0
         (>= (coords-x init-elt-pos) 0)
         (<= (coords-x init-elt-pos) max-mouse-pos)
         (>= (coords-y init-elt-pos) 0)
         (<= (coords-y init-elt-pos) max-mouse-pos)
       )))

#;(define (click-sequence state transition)
  (cond [(and (eq? 'waiting-for-up state) (eq? 'up (get-value transition))) 'waiting-for-down]
        [(and (eq? 'waiting-for-down state) (eq? 'down (get-value transition))) 'waiting-for-up]
        [else #f]))

#;(define (dragging-intervals mouse-down mouse-up)
  (map (λ (down-e) (list (get-timestamp down-e) (get-timestamp (findf (λ (up-e) (and (eq? (get-value up-e) 'up)
                                                                                     (<= (get-timestamp down-e) (get-timestamp up-e))))
                                                                      mouse-up))))
       (filter (λ (e) (eq? (get-value e) 'down)) mouse-down)))
#;(define (dropping-intervals mouse-up mouse-down)
  (map (λ (up-e) (list up-e (findf (λ (down-e) (and (eq? (get-value down-e) 'down)
                                                    (<= (get-timestamp up-e) (get-timestamp down-e))))
                                   mouse-down)))
       (filter (λ (e) (eq? (get-value e) 'up)) mouse-up)))
#;(define (bounded-by-events start end evt-stream)
  (if (and (not start) (not end))
      evt-stream
      (if (and start end)
          (boundedTimestampsStream (get-timestamp start) (get-timestamp end) evt-stream)
          (if start
              (startAtTimestamp (get-timestamp start) evt-stream)
              (endAtTimestamp (get-timestamp end) evt-stream)))))

#;(define (drag-and-drop-guarantees mouse-up mouse-down mouse-pos init-elt-pos output-posB)
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
       ))