#lang rosette/safe

;; inputs and outputs are lists of pairs of timestamps and values
;; event streams consist of timestamp/value pairs when some event occurs (if no entry, no event is occurring
;; behaviors consist of timestamp/value pairs when some value *changes* (if no entry, behavior holds its previous value)

(define (clicksE concrete-list)
  (map (λ (c)
                  (define-symbolic* timestamp integer?)
                  (list timestamp 'click)) concrete-list))

(define (constE s-events const)
  (map (λ (s) (list (first s) const)) s-events))

(define (mergeE s-events1 s-events2)
  (sort (append s-events1 s-events2) (λ (x y) (< (first x) (first y)))))

(define (collectE s-behavior init proc)
  (letrec ([collect (λ (x-lst prev)
                      (if (equal? (length x-lst) 0)
                          '()
                          (let ([new-ts (first (first x-lst))]
                                [new-val (proc (second (first x-lst)) prev)])
                            (append (list (list new-ts new-val))
                                    (collect (cdr x-lst) new-val)))))])
    (collect s-behavior init)))

(define (startsWith s-events init) ;; adds special timestep 0
  (append (list (list 0 init)) s-events))

(define (graph inc dec)
  (startsWith (collectE (mergeE (constE inc 1) (constE dec -1)) 0 +) 0))

(define concrete-inc-clicks (list (list 1 'click) (list 4 'click)))
(define concrete-dec-clicks (list (list 2 'click) (list 3 'click) (list 5 'click)))

(displayln (graph concrete-inc-clicks concrete-dec-clicks))

(define s-inc (clicksE (list 1)))
(define s-dec (clicksE (list 1)))

(displayln (graph s-inc s-dec))


(define (delayE evt-stream interval)
  (map (λ(s) (list (+ (first s) interval) (second s))) evt-stream))

(define (calmE evt-stream interval)
  (letrec ([calm (λ (evt-lst)
                  (if (equal? (length evt-lst) 1)
                      evt-lst ;; the last event is always propagated
                      (let ([current (first evt-lst)]
                            [next (second evt-lst)])
                        (if (< (- (first next) (first current)) interval)
                            (calm (cdr evt-lst)) ;; the first one is too close to the second, don't propagate
                            (append (list current) (calm (cdr evt-lst)))))))]) ;; propagate the first one
    (calm evt-stream)))

;;;;; motion detector and porch light
(define (light-graph md-events)
  (mergeE (constE md-events 'on) (constE (calmE (delayE md-events 5) 5) 'off)))

(define concrete-motion (list (list 1 'd) (list 3 'd) (list 10 'd) (list 20 'd)))

(define s-motion (clicksE (list 1 2)))

(define sol (solve (assert (equal? (last (second (light-graph s-motion))) 'off))))

(define inc-clicks2 (clicksE (list 1 2 3)))
(define dec-clicks2 (clicksE (list 1 2 3)))

(define (first-always-0 inc dec)
  (assert (= (first (graph inc dec)) 0)))

(define sol2 (verify (first-always-0 inc-clicks2 dec-clicks2)))