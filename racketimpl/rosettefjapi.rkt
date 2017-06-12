#lang rosette/safe
(provide (all-defined-out))

;;;;; helpers ;;;;;
(define ts-comparator (λ (x y) (< (first x) (first y))))
(define (startAtTimestamp ts evt-stream-f)
  (λ ()
    (let ([evt-stream (evt-stream-f)])
      (unless (empty? evt-stream)
      (filter (λ (e) (> (first e) ts)) evt-stream)))))

(define (get-timestamp item)
  (first item))

(define (get-value item)
  (second item))

;;;;; flapjax API ;;;;;

(define (oneE evt-stream-f)
  (λ ()
    (let ([evt-stream (evt-stream-f)])
      (unless (empty? evt-stream)
      (list (first evt-stream)))))) ;; propagate only the first event

(define (zeroE evt-stream-f)
  (λ ()
    (let ([evt-stream (evt-stream-f)])
  (map (λ (e) (list (first e) (void))) evt-stream)))) ;; a stream that never fires

(define (mapE proc evt-stream-f) ;; proc operates over both timestamp and value (kind of a cheat)
  (λ ()
    (let ([evt-stream (evt-stream-f)])
  (map (λ (e) (proc e)) evt-stream))))

(define (mergeE evt-stream1-f evt-stream2-f) ;; note: mergeE can actually take any num of args
  (λ ()
    (let ([evt-stream1 (evt-stream1-f)]
          [evt-stream2 (evt-stream2-f)])
      (sort (append evt-stream1 evt-stream2) (λ (x y) (< (first x) (first y)))))))

(define (switchE stream-of-streams-f)
  (λ ()
    (let ([stream-of-streams (map (λ (f) (apply (second f) '()))
                                  stream-of-streams-f)])
      (sort (flatten stream-of-streams) (λ (x y) (< (first x) (first y)))))))

;; condE

;; filterE

;; ifE

(define (constantE evt-stream-f const)
  (λ ()
    (let ([evt-stream (evt-stream-f)])
      (map (λ (s) (list (first s) (if (equal? 'no-evt (second s)) 'no-evt const))) evt-stream))))

(define (collectE evt-stream-f init proc)
  (λ ()
    (let ([evt-stream (evt-stream-f)])
      (letrec ([collect (λ (x-lst prev)
                          (if (equal? (length x-lst) 0)
                              '()
                              (let* ([new-ts (first (first x-lst))]
                                    [input-val (second (first x-lst))]
                                    [new-val (if (equal? input-val 'no-evt) prev (proc (second (first x-lst)) prev))])
                                (append (list (list new-ts new-val))
                                        (collect (cdr x-lst) new-val)))))])
        (collect evt-stream init)))))

;; andE

;; orE

;; notE

;; filterRepeatsE

;; send/receive

;; snapshotE

;; onceE

;; skipFirstE

(define (delayE evt-stream-f interval)
  (λ ()
    (let ([evt-stream (evt-stream-f)])
      (map (λ(s) (list (+ (first s) interval) (second s))) evt-stream))))

;; blindE

(define (calmE evt-stream-f interval)
  (λ ()
    (let ([evt-stream (evt-stream-f)])
      (letrec ([calm (λ (evt-lst)
                       (if (equal? (length evt-lst) 1)
                           evt-lst ;; the last event is always propagated
                           (let ([current (first evt-lst)]
                                 [next (second evt-lst)])
                             (if (< (- (first next) (first current)) interval)
                                 (calm (cdr evt-lst)) ;; the first one is too close to the second, don't propagate
                                 (append (list current) (calm (cdr evt-lst)))))))]) ;; propagate the first one
    (calm evt-stream)))))

(define (startsWith evt-stream-f init-value)
  (λ ()
    (let* ([evt-stream (evt-stream-f)])
      (append (list (list 0 init-value)) evt-stream))))



