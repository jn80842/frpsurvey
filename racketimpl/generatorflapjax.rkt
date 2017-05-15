#lang racket
(require racket/generator)

;; simplifying assumption: inputs contain items for every tick of the clock
;; val of #f means no event occurred

(define (get-map-gen func evt-stream)
  (generator ()
             (let loop ([evt (evt-stream)])
               (begin
                 (yield (func evt))
                 (loop (evt-stream))))))

(define (get-merge-gen stream1 stream2)
  (generator ()
             (let loop ([evt1 (stream1)]
                        [evt2 (stream2)])
               (begin
                 (yield evt1)
                 (yield evt2)
                 (loop (stream1) (stream2))))))

(define (get-const-gen const input-evt-stream)
  (generator ()
             (let loop ([evt (input-evt-stream)])
               (if (void? evt)
                   #f
                   (begin
                     (yield (if evt const #f))
                     (loop (input-evt-stream)))))))

(define (get-if-gen stream1 stream2 stream3)
  (generator ()
             (let loop ([evt1 (stream1)]
                        [evt2 (stream2)]
                        [evt3 (stream3)])
               (begin
                 (if evt1
                     (yield evt2)
                     (yield evt3))
                 (loop (stream1) (stream2) (stream3))))))

(define (get-collect-gen evt-stream func init-val)
  (generator ()
             (let loop ([collectVal init-val]
                        [evt-val (evt-stream)])
               (begin
                 (let ([collected-val (func init-val evt-val)])
                   (yield collected-val)
                   (loop collected-val (evt-stream)))))))

(define (get-filter-repeats-gen evt-stream)
  (generator ()
             (let loop ([evt (evt-stream)]
                        [prev-evt #f])
               (begin
                 (if (equal? evt prev-evt)
                     (yield #f)
                     (yield evt))
                 (loop (evt-stream) evt)))))

(define (get-delay-gen evt-stream delay-time)
  (generator ()
            (let loop ([pending (list)]
                       [evtVal (evt-stream)])
              (begin
                (let ([ready (filter (λ (p) (equal? (first p) 0)) pending)]
                      [still-pending (map (λ (p) (list (sub1 (first p)) (second p)))
                                          (filter (λ (p) (not (equal? (first p) 0))) pending))])
                  (if (equal? (length ready) 0)
                      (yield #f)
                      (for ([evt-pair ready])
                        (yield (second evt-pair))))
                  (loop (append still-pending (list (list delay-time evtVal))) (evt-stream)))))))

(define (get-blind-gen evt-stream interval)
  (generator ()
             (let loop ([evt (evt-stream)]
                        [last-evt-time #f])
               (if (and last-evt-time (> last-evt-time interval))
                   (begin
                     (yield evt)
                     (loop (evt-stream) 0))
                   (begin
                     (yield #f)
                     (loop (evt-stream) (add1 last-evt-time)))))))

(define (get-starts-with-gen evt-stream init-val)
  (generator ()
             (yield init-val)
             (let loop ([evt (evt-stream)])
               (begin
                 (yield evt)
                 (loop (evt-stream))))))


(define inc-button (sequence->generator (list #t #f #f #f #f #f #t)))
(define dec-button (sequence->generator (list #f #f #t #t #f #f #f)))

(define inc-const-gen (get-const-gen 1 inc-button))
(define dec-const-gen (get-const-gen -1 dec-button))
(define merged-buttons-gen (get-merge-gen inc-const-gen dec-const-gen))
(define collected-buttons-gen (get-collect-gen merged-buttons-gen (λ (init new) (if new (+ init new) init)) merged-buttons-gen))
(define inc-dec-buttons-gen (get-starts-with-gen collected-buttons-gen 0))












                                      