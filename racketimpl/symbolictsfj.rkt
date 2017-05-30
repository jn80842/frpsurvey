#lang rosette/safe

(require "rosettefjapi.rkt")

;; inputs and outputs are lists of pairs of timestamps and values
;; event streams consist of timestamp/value pairs when some event occurs (if no entry, no event is occurring
;; behaviors consist of timestamp/value pairs when some value *changes* (if no entry, behavior holds its previous value)

(define (clicksE concrete-list)
 (λ ()
    (map (λ (c)
         (define-symbolic* timestamp integer?)
         (list timestamp 'click)) concrete-list)))
;(define (s-click)
;  (define-symbolic* isclick boolean?)
;  (if isclick 'click (void)))



(define (inc-dec-button-graph inc dec)
  (startsWith
  (collectE
               (mergeE (constantE inc 1) (constantE dec -1)) 0 +)  (λ () (list (list 0 70)))))

(define concrete-inc-clicks (λ () (list (list 1 'click) (list 4 'click))))
(define concrete-dec-clicks (λ () (list (list 2 'click) (list 3 'click) (list 5 'click))))

(displayln ((inc-dec-button-graph concrete-inc-clicks concrete-dec-clicks)))

(define s-inc (clicksE (list )))
(define s-dec (clicksE (list )))

(define button-sol (solve
                    (begin
                      (assert (equal? (second (last ((inc-dec-button-graph (λ () s-inc) (λ () s-dec))))) 0))
                      ;; each sequence of events should have distinct timestamps
                      (assert (apply distinct? (map (λ (e) (first e)) s-inc)))
                      (assert (apply distinct? (map (λ (e) (first e)) s-dec)))
                      ;; all timestamps should be positive
                      (assert (andmap positive? (map (λ (e) (first e)) (append s-inc s-dec))))
                      )))

;;;;; mouse tail ;;;;

(define (timestamp-list concrete-list)
  (map (λ (x) (define-symbolic* timestamp integer?) timestamp) concrete-list))

(define (integer-events-for-timestamps timestamps)
  (λ () (map (λ (ts) (define-symbolic* value integer?) (list ts value)) timestamps)))

(define (integer-event-list concrete-list)
  (λ () (map (λ (c)
         (define-symbolic* timestamp integer?)
         (define-symbolic* value integer?)
         (list timestamp value)) concrete-list)))

(define (mouse-tail-graph mouse-x-event-stream mouse-y-event-stream)
  (let ([tail-x (mapE (λ (e) (list (first e) (+ (second e) 50))) (delayE mouse-x-event-stream 3))]
        [tail-y (delayE mouse-y-event-stream 3)])
    (list (tail-x) (tail-y))))

(define mouse-timestamps (timestamp-list (list 1 2 3)))
(define mouse-x (integer-events-for-timestamps mouse-timestamps))
(define mouse-y (integer-events-for-timestamps mouse-timestamps))

;(displayln (mouse-tail-graph mouse-x mouse-y))

;;(define (unique-timestamps timestamps)
;;  (for-each ([ts timestamps])
;;(define mouse-tail-sol (solve (assert

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






;;;;; motion detector and porch light
(define (light-graph md-events)
  (mergeE (constantE md-events 'on) (constantE (calmE (delayE md-events 5) 5) 'off)))

(define concrete-motion (λ () (list (list 1 'd) (list 3 'd) (list 10 'd) (list 20 'd))))

(define s-motion (clicksE (list 1 2)))

(define sol (solve (assert (equal? (last (second (light-graph s-motion))) 'off))))

(define inc-clicks2 (clicksE (list 1 2 3)))
(define dec-clicks2 (clicksE (list 1 2 3)))

(define (first-always-0 inc dec)
  (assert (= (first (inc-dec-button-graph inc dec)) 0)))

(define sol2 (verify (first-always-0 inc-clicks2 dec-clicks2)))