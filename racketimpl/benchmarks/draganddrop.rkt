#lang rosette/safe

(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")

;;;;;; drag and drop ;;;;;

;; value of all mouse events are x and y coordinates in a vector
(define drag-mouse-down (λ () (list (list 1 (vector 0 0)) (list 10 (vector 2 3)))))
(define drag-mouse-up (λ () (list (list 5 (vector 10 20)) (list 13 (vector 20 40)))))
(define mouse-movements (λ () (list (list 1 (vector 0 0))
                                    (list 2 (vector 1 1))
                                    (list 5 (vector 10 20))
                                    (list 10 (vector 2 3))
                                    (list 13 (vector 20 40)))))

(define coordE (mapE (λ (mm) (vector (- (vector-ref mm 0) 1)
                                                              (- (vector-ref mm 1) 1)))
                             (λ () (filter (λ (mm) (> (first mm) 3)) (mouse-movements)))))

(define dropEe (mapE (λ (e) (list (first e) (zeroE e))) drag-mouse-up))
(define moveEe (mapE (λ (e) (define startX (vector-ref (second e) 0))
                       (define startY (vector-ref (second e) 1))
                       (list (first e)
                       (mapE (λ (mm) (list (first mm)
                               (vector (- (vector-ref (second mm) 0) startX)
                                       (- (vector-ref (second mm) 1) startY))))
                             (startAtTimestamp (first e) (startsWith mouse-movements
                                                                     (mapE (λ (e) (list (first e) #f)) drag-mouse-up))))))
                       drag-mouse-down))

;(define dragE (switchE (mergeE dropEe moveEe)))
