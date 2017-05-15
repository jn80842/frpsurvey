#lang rosette

;; mouse program
;; inputs are mouse x and y position
;; outputs are x and y of cursor element and x and y of tail (5 sec delay)

;; given just one input (time + x and y pos), what output should we see?


;; concrete inputs
(define mouse-x 2)
(define mouse-y 2)
(define mouse-timestamp 5)

;; symbolic inputs
(define-symbolic cursor-timestamp cursor-x cursor-y integer?)
(define-symbolic tail-timestamp tail-x tail-y integer?)
  
(define single-input
  (solve
   (begin
     (assert (equal? mouse-timestamp cursor-timestamp))
     (assert (equal? (+ mouse-timestamp 1) tail-timestamp))

     (assert (equal? mouse-x cursor-x))
     (assert (equal? mouse-y cursor-y))
     (assert (equal? (+ mouse-x 3) tail-x))
     (assert (equal? mouse-y tail-y))
     )))

