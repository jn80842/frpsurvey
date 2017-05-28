#lang racket
(require racket/generator)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO                         ;;
;; rename to match flapjax      ;;
;; distinguish evt & behavior   ;;
;; merge produces tuples        ;;
;; keep explicit track of time? ;;
;; symbolic execution ????      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; simplifying assumption: inputs contain items for every tick of the clock
;; special symbol 'no-evt means no event occurred

(define (no-evt? x)
  (equal? x 'no-evt))

(define (oneE)
  #t)

(define (zeroE)
  (generator ()
             (infinite-generator (yield 'no-evt))))

(define (mapE func evt-stream)
  (generator ()
             (let loop ([evt (evt-stream)])
               (begin
                 (if (void? evt)
                     (yield 'no-evt)
                 (if (no-evt? evt)
                     (yield evt)
                     (yield (func evt))))
                 (loop (evt-stream))))))

(define (mergeE stream1 stream2)
  (generator ()
             (let loop ([evt1 (stream1)]
                        [evt2 (stream2)])
               (begin
                 (unless (no-evt? evt1) (yield evt1))
                 (unless (no-evt? evt2) (yield evt2))
                 (unless (not (and (no-evt? evt1) (no-evt? evt2))) (yield 'no-evt))
                 (loop (stream1) (stream2))))))

(define (switchE evt-stream)
  #t)

(define (condE bool-evt-pair)
  #t)

(define (filterE evt-stream pred)
  (generator ()
             (let loop ([evt (evt-stream)])
               (begin
                 (if (pred evt)
                     (yield evt)
                     (yield 'no-evt))
                 (loop (evt-stream))))))

(define (ifE stream1 stream2 stream3)
  (generator ()
             (let loop ([evt1 (stream1)]
                        [evt2 (stream2)]
                        [evt3 (stream3)])
               (begin
                 (if evt1
                     (yield evt2)
                     (yield evt3))
                 (loop (stream1) (stream2) (stream3))))))

(define (constE const input-evt-stream)
  (generator ()
             (let loop ([evt (input-evt-stream)])
               (if (void? evt)
                   'no-evt
                   (begin
                     (yield (if (not (no-evt? evt)) const 'no-evt))
                     (loop (input-evt-stream)))))))

(define (collectE evt-stream func init-val)
  (generator ()
             (let loop ([prev-val init-val]
                        [evt-val (evt-stream)])
               (begin
                 (let ([collected-val (func prev-val evt-val)])
                   (yield collected-val)
                   (loop collected-val (evt-stream)))))))

(define (andE stream1 stream2)
  #t)

(define (orE stream1 stream2)
  #t)

(define (notE stream)
  #t)

(define (receiverE)
  #t)

(define (sendEventE a evt-stream)
  #t)

(define (filterRepeatE evt-stream)
  (generator ()
             (let loop ([evt (evt-stream)]
                        [prev-evt 'no-evt])
               (begin
                 (if (equal? evt prev-evt)
                     (yield 'no-evt)
                     (yield evt))
                 (loop (evt-stream) evt)))))

(define (snapshotE evt-stream behavior)
  #t)

(define (onceE evt-stream)
  #t)

(define (skipFirstE evt-stream)
  #t)

(define (delayE evt-stream delay-time)
  (generator ()
            (let loop ([pending (list)]
                       [evtVal (evt-stream)])
              (begin
                (let ([ready (filter (λ (p) (equal? (first p) 0)) pending)]
                      [still-pending (map (λ (p) (list (sub1 (first p)) (second p)))
                                          (filter (λ (p) (not (equal? (first p) 0))) pending))])
                  (if (equal? (length ready) 0)
                      (yield 'no-evt)
                      (for ([evt-pair ready])
                        (yield (second evt-pair))))
                  (loop (append still-pending (list (list delay-time evtVal))) (evt-stream)))))))

(define (blindE evt-stream interval)
  (generator ()
             (let loop ([evt (evt-stream)]
                        [last-evt-time #f])
               (if (and last-evt-time (> last-evt-time interval))
                   (begin
                     (yield evt)
                     (loop (evt-stream) 0))
                   (begin
                     (yield 'no-evt)
                     (loop (evt-stream) (add1 last-evt-time)))))))

(define (calmE evt-stream interval-behavior)
  #t)

(define (startsWith evt-stream init-val)
  (generator ()
             (yield init-val)
             (let loop ([evt (evt-stream)]
                        [val init-val])
               (begin
                 (if (or (no-evt? evt) (void? evt))
                     (begin
                       (yield val)
                       (loop (evt-stream) val))
                     (begin
                       (yield evt)
                       (loop (evt-stream) evt)))))))

(define (changes behavior)
  #t)

(define (constB const)
  (infinite-generator (yield const)))

(define (delayB sourceB delay)
  #t)

(define (switchB sourceBB)
  #t)

(define (andB valueB1 valueB2)
  #t)

(define (orB valueB1 valueB2)
  #t)

(define (notB valueB)
  #t)

(define (liftB func argB)
  #t)
;(define lift-behavior get-map-gen) ;; in this model they're the same thing

(define (condB condResultB)
  #t)

(define (ifB conditionB trueB falseB)
  #t)
;(define if-behavior get-if-gen)

(define (timerB interval)
  #t)

(define (blindB sourceB intervalB)
  #t)

(define (calmB sourceB intervalB)
  #t)

;; so we can have infinite streams of events
(define (get-event-stream gen)
  (generator ()
             (let loop ([g (gen)])
               (if (void? g)
                   (yield 'no-evt)
                   (yield g))
               (loop (gen)))))

;;;;;; Web example ;;;;;;;;;

(define inc-button (get-event-stream (sequence->generator (list 'click 'no-evt 'no-evt 'no-evt 'no-evt 'no-evt 'click))))
(define dec-button (get-event-stream (sequence->generator (list 'no-evt 'no-evt 'click 'click 'no-evt 'no-evt 'no-evt))))

(define inc-const-gen (constE 1 inc-button)) ;; 1 n n n n n 1 n n n ...
(define dec-const-gen (constE -1 dec-button)) ;; n n -1 -1 n n n
(define merged-buttons-gen (mergeE inc-const-gen dec-const-gen)) ;; 1 n n n -1 -1 n 1 n n n n 
(define collected-buttons-gen (collectE merged-buttons-gen (λ (l new) (if (not (no-evt? new)) (+ l new) l)) 0)) ;; 1 1 0 -1 -1 0 0 ....
(define inc-dec-buttons-gen (startsWith collected-buttons-gen 0))


;;;;;;; IoT example ;;;;;;;

(define clock-times (get-event-stream (sequence->generator (list 2000 2030 2100 2101 2102 2103 2104 2105 2106 905 906 907 908 909 910 911 912))))
(define user-location (get-event-stream (sequence->generator (list "work" "car" "home" "home" "home" "home" "home" "home" "home" "home" "home" "home" "home"))))
(define (motion-detector) (get-event-stream (sequence->generator (list 'no-evt 'no-evt #t 'no-evt 'no-evt 'no-evt 'no-evt 'no-evt 'no-evt #t))))

(define is-night-gen (get-map-gen (λ(v) (or (> v 2059) (< v 800))) clock-times))
(define home-or-away-gen (get-starts-with-gen (get-map-gen (λ(v) (if (equal? v "home") "home" "away")) user-location) "home"))
(define mode-gen (get-if-gen is-night-gen (get-const-behavior "night") home-or-away-gen))
(define color-gen (get-if-gen is-night-gen (get-const-behavior 'orange) (get-const-behavior 'white)))

(define light-on-gen (get-const-gen 'on (motion-detector)))
(define light-off-gen (get-delay-gen (get-const-gen 'off (motion-detector)) 5))
(define merged-light-signals (get-merge-gen light-on-gen light-off-gen))
(define light-on-off-behavior (get-starts-with-gen merged-light-signals 'off))
(define is-light-on-behavior (lift-behavior (λ (v) (equal? v 'on)) light-on-off-behavior))
(define light-behavior (if-behavior is-light-on-behavior color-gen (get-const-behavior 'off)))










                                      