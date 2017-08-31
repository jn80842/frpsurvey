#lang rosette/safe

(require rosette/lib/synthax)
(require "../fjmodels.rkt")
(require "../rosettefjapi.rkt")
(require "../benchmarks/draganddrop.rkt")

(provide (all-defined-out))

(current-bitwidth 5)
(define stream-length 3)
(define max-timestamp (* 2 stream-length))
(define max-mouse-pos 3)

(define (sym-coords)
  (define-symbolic* x integer?)
  (define-symbolic* y integer?)
  (vector x y))

(define (drag-and-drop-assumptions mouse-up mouse-down mouse-pos init-elt-pos)
  (let ([actual-ups (filter (λ (e) (not (eq? 'no-evt (get-value e)))) mouse-up)]
        [actual-downs (filter (λ (e) (not (eq? 'no-evt (get-value e)))) mouse-down)])
    (and (valid-timestamps? mouse-up)
         (valid-timestamps? mouse-down)
         ;; timestamps don't need to be bigger than both sets of mouse events together
         ;(timestamps-below-max? max-timestamp mouse-up)
         (andmap (λ (t) (>= max-timestamp t)) (append (map get-timestamp mouse-up) (map get-timestamp mouse-down)))
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
         ;; all mouse positions must be >0
         ;; and bound upper value to limit verification time
         (andmap (λ (v) (and (>= (vector-ref (get-value v) 0) 0)
                             (<= (vector-ref (get-value v) 0) max-mouse-pos)
                             (>= (vector-ref (get-value v) 1) 0)
                             (<= (vector-ref (get-value v) 1) max-mouse-pos))) (behavior-changes mouse-pos))
         ;; limit timestamps on mouse position to limit verification time
         (andmap (λ (e) (>= max-timestamp (get-timestamp e))) (changes mouse-pos))
         ;; initial placement of element must be >0
         (>= (vector-ref init-elt-pos 0) 0)
         (<= (vector-ref init-elt-pos 0) max-mouse-pos)
         (>= (vector-ref init-elt-pos 1) 0)
         (<= (vector-ref init-elt-pos 1) max-mouse-pos)
       )))
