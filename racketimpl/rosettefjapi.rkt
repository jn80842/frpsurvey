#lang rosette/safe
(provide (all-defined-out))

(define (oneE evt-stream)
  (list (first evt-stream))) ;; propagate only the first event

(define (zeroE evt-stream)
  (void)) ;; a stream that never fires

(define (mapE proc evt-stream)
  (map (λ (e) (list (first e) (proc (second e)))) evt-stream))

(define (mergeE s-events1 s-events2)
  (sort (append s-events1 s-events2) (λ (x y) (< (first x) (first y)))))

(define (switchE stream-of-streams)
  (sort (flatten stream-of-streams) (λ (x y) (< (first x) (first y)))))

;; condE

;; filterE

;; ifE

(define (constantE s-events const)
  (map (λ (s) (list (first s) const)) s-events))

(define (collectE s-behavior init proc)
  (letrec ([collect (λ (x-lst prev)
                      (if (equal? (length x-lst) 0)
                          '()
                          (let ([new-ts (first (first x-lst))]
                                [new-val (proc (second (first x-lst)) prev)])
                            (append (list (list new-ts new-val))
                                    (collect (cdr x-lst) new-val)))))])
    (collect s-behavior init)))

;; andE

;; orE

;; notE

;; filterRepeatsE

;; send/receive

;; snapshotE

;; onceE

;; skipFirstE

(define (delayE evt-stream interval)
  (map (λ(s) (list (+ (first s) interval) (second s))) evt-stream))

;; blindE

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

(define (startsWith s-events init) ;; adds special timestep 0
  (append (list (list 0 init)) s-events))

