#lang rosette/safe
(provide (all-defined-out))

(require "fjmodels.rkt")

;;;;;;;;; operator wishlist ;;;;;;
;;
;; highFreqE: propagate events that occur x seconds after prev
;; collectE but where internal value and output value can be different
;; mapE, collectE, liftB etc where the functions have access to timestamps
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; flapjax API ;;;;;

(define (oneE evt-stream)
  (unless (empty? evt-stream)
    (list (first evt-stream)))) ;; propagate only the first event

(define (zeroE)
  '()) ;; a stream that never fires

(define (mapE proc evt-stream) ;; proc operates over both timestamp and value (kind of a cheat)
  (map (λ (e) (proc e)) evt-stream))

#;(define (mergeE evt-stream1 evt-stream2) ;; note: mergeE can actually take any num of args
  (sort (append evt-stream1 evt-stream2) (λ (x y) (< (get-timestamp x) (get-timestamp y))))) ;))

(define (mergeE evt-stream1 evt-stream2)
  (cond [(empty? evt-stream1) evt-stream2]
        [(empty? evt-stream2) evt-stream1]
        [(< (get-timestamp (first evt-stream1)) (get-timestamp (first evt-stream2)))
         (append (list (first evt-stream1)) (mergeE (rest evt-stream1) evt-stream2))]
        [else (append (list (first evt-stream2)) (mergeE evt-stream1 (rest evt-stream2)))]))

(define (switchE stream-of-streams)
  (if (empty? stream-of-streams)
      '()
      (let ([ts (map (λ (e) (get-timestamp e)) stream-of-streams)])
        (apply append (filter (λ (l) (not (void? l)))
               (map (λ (start-ts end-ts vals) (if end-ts
                                           (boundedTimestampsStream start-ts end-ts (get-value vals))
                                           (startAtTimestamp start-ts (get-value vals))))
             ts (append (rest ts) (list #f)) stream-of-streams))))))

;; condE

(define (filterE pred stream)
  (filter (λ (e) (pred (get-value e))) stream))

(define (ifE guard-stream true-stream false-stream)
  ;; split guard stream into true and false values
  ;; filter true and false stream by split guard stream respectively
  ;; merge filtered true and false streams and return
  (let* ([if-ts (map get-timestamp guard-stream)]
        [filtered-true (filter (λ (e) (member (get-timestamp e) if-ts)) true-stream)]
        [filtered-false (filter (λ (e) (member (get-timestamp e) if-ts)) false-stream)]
        [filtered-if (filter (λ (e) (member (get-timestamp e) (map get-timestamp (append true-stream false-stream)))) guard-stream)])
    ;; for each entry in guard-stream
    ;; if true-stream has an entry at that ts and its true, return the true-stream entry
    ;; else, return the false-stream entry from that ts
    (map (λ (e) (if (and (member (get-timestamp e) (map get-timestamp filtered-true))
                       (get-value (findf (λ (n) (= (get-timestamp n) (get-timestamp e))) filtered-if)))
                  (findf (λ (n) (= (get-timestamp n) (get-timestamp e))) filtered-true)
                  (findf (λ (n) (= (get-timestamp n) (get-timestamp e))) filtered-false))) filtered-if)))

(define (constantE const evt-stream)
  (map (λ (s) (list (get-timestamp s) (if (equal? 'no-evt (get-value s)) 'no-evt const))) evt-stream)) ;))

(define (collectE init proc evt-stream)
  (letrec ([collect (λ (x-lst prev)
                      (if (equal? (length x-lst) 0)
                          '()
                          (let* ([new-ts (get-timestamp (first x-lst))]
                                 [input-val (get-value (first x-lst))]
                                 [new-val (if (equal? input-val 'no-evt) prev (proc (get-value (first x-lst)) prev))])
                            (append (list (list new-ts new-val))
                                    (collect (cdr x-lst) new-val)))))])
    (collect evt-stream init)))

;; andE

;; orE

(define (notE evt-stream)
  (map (λ (e) (list (get-timestamp e) (not (get-value e)))) evt-stream))

;; filterRepeatsE

;; send/receive

(define (snapshotE evt-stream behavior1)
  (let ([real-evt-stream (filter (λ (e) (not (eq? (get-value e) 'no-evt))) evt-stream)])
    (map (λ (t) (list (get-timestamp t) (valueNow behavior1 (get-timestamp t)))) real-evt-stream)))

;; onceE

;; skipFirstE

(define (delayE interval evt-stream)
  (map (λ(s) (list (+ (get-timestamp s) interval) (get-value s))) evt-stream))

(define (blindE interval evt-stream)
  (letrec ([f (λ (evts last-sent)
                ;; if all events have been processed, done!
                (cond [(empty? evts) '()]
                      ;; if time between current event and last sent is geq interval
                      ;; propagate event, update last sent, and continue
                      [(<= interval (- (get-timestamp (first evts)) last-sent))
                       (append (list (first evts)) (f (rest evts) (get-timestamp (first evts))))]
                      ;; else, continue without propagating current event
                      [else (f (rest evts) last-sent)]))])
    (f evt-stream (- interval))))

(define (calmE interval evt-stream)
  (letrec ([emit-event (λ (evt) (list (list (+ (get-timestamp evt) interval) (get-value evt))))]
           [f (λ (evts buffered-evt)
                ;; if all events have been processed, propagate buffered event
                (cond [(and (empty? evts) buffered-evt) (emit-event buffered-evt)]
                      ;; if all events have been processed and no event is buffered, done!
                      [(empty? evts) '()]
                      ;; if nothing has been buffered, buffer event and continue
                      [(false? buffered-evt) (f (rest evts) (first evts))]
                      ;; if time between buffered event and next event is geq interval
                      ;; propagate buffered event and continue
                      [(<= interval (- (get-timestamp (first evts)) (get-timestamp buffered-evt)))
                       (append (emit-event buffered-evt) (f (rest evts) (first evts)))]
                      ;; if time between buffered event and next event is too small
                      ;; discard buffered event and continue
                      [else (f (rest evts) (first evts))]))])
    (f evt-stream #f)))

(define (startsWith init-value evt-stream)
  (behavior init-value evt-stream))

(define (changes behaviorB)
    (behavior-changes behaviorB))

(define (constantB const)
  (behavior const '()))

(define (delayB interval behavior1)
  (behavior (behavior-init behavior1) (delayE interval (behavior-changes behavior1))))

;; valueNow: since valueNow shouldn't be exposed to end users, it's in fjmodels.rkt

;; switchB
;; switchBB takes a behavior of behaviors: (behavior behavior1 (list behavior2 behavior3 behavior4)))
;; and returns a behavior: (behavior (behavior-init behavior1) (append (behavior-changes behavior1) (behavior-changes behavior2) ...)))

(define (andB behavior1 behavior2)
  (let* ([unique-ts (sort (remove-duplicates (append (map get-timestamp (behavior-changes behavior1))
                                                     (map get-timestamp (behavior-changes behavior2)))) <)]
         [enhanced-b1 (project-values behavior1 unique-ts)]
         [enhanced-b2 (project-values behavior2 unique-ts)])
    (behavior (and (behavior-init behavior1) (behavior-init behavior2))
              (map (λ (b1 b2) (list (get-timestamp b1) (and (get-value b1) (get-value b2)))) enhanced-b1 enhanced-b2)))
  )

(define (orB behavior1 behavior2)
  (let* ([unique-ts (sort (remove-duplicates (append (map get-timestamp (behavior-changes behavior1))
                                                     (map get-timestamp (behavior-changes behavior2)))) <)]
         [enhanced-b1 (project-values behavior1 unique-ts)]
         [enhanced-b2 (project-values behavior2 unique-ts)])
    (behavior (or (behavior-init behavior1) (behavior-init behavior2))
              (map (λ (b1 b2) (list (get-timestamp b1) (or (get-value b1) (get-value b2)))) enhanced-b1 enhanced-b2))))

(define (notB behavior1)
  (behavior (not (behavior-init behavior1)) (notE (behavior-changes behavior1))))

(define (liftB proc . argBs)
  (let* ([unique-ts (sort (remove-duplicates (flatten (map (λ (b) (map get-timestamp (behavior-changes b))) argBs))) <)]
         [enhanced-argBs (map (λ (b) (project-values b unique-ts)) argBs)])
  (behavior (apply proc (map behavior-init argBs)) (apply (curry map (λ e (list (get-timestamp (first e)) (apply proc (map get-value e))))) enhanced-argBs))))

(define (liftB1 proc argB)
  (behavior (proc (behavior-init argB)) (map (λ (b) (list (get-timestamp b) (proc (get-value b)))) (behavior-changes argB))))

(define (liftB2 proc argB1 argB2)
  (behavior (proc (behavior-init argB1) (behavior-init argB2))
            (letrec ([f (λ (b1 b2) (cond [(or (empty? (behavior-changes b1))
                                              (empty? (behavior-changes b2))) '()]
                                         [(eq? (get-timestamp (first (behavior-changes b1)))
                                               (get-timestamp (first (behavior-changes b2))))
                                          (append (list (list (get-timestamp (first (behavior-changes b1)))
                                                              (proc (get-value (first (behavior-changes b1))) (get-value (first (behavior-changes b2))))))
                                                  (f (behavior (get-value (first (behavior-changes b1))) (rest (behavior-changes b1)))
                                                     (behavior (get-value (first (behavior-changes b2))) (rest (behavior-changes b2)))))]
                                         [(< (get-timestamp (first (behavior-changes b1))) (get-timestamp (first (behavior-changes b2))))
                                          (append (list (list (get-timestamp (first (behavior-changes b1)))
                                                              (proc (get-value (first (behavior-changes b1))) (valueNow b2 (get-timestamp (first (behavior-changes b1)))))))
                                                        (f (behavior (get-value (first (behavior-changes b1))) (rest (behavior-changes b1))) b2))]
                                         [else (append (list (list (get-timestamp (first (behavior-changes b2)))
                                                                   (proc (valueNow b1 (get-timestamp (first (behavior-changes b2)))) (get-value (first (behavior-changes b2))))))
                                                       (f b1 (behavior (get-value (first (behavior-changes b2))) (rest (behavior-changes b2)))))]))])
              (f argB1 argB2))))

;; is there an easier way??
(define (condB behaviorpairs)
  (let* ([all-bool-ts (flatten (map (λ (b) (map get-timestamp (behavior-changes (first b)))) behaviorpairs))] ;; every timestamp mentioned in any boolean behavior
         [all-val-ts (flatten (map (λ (b) (map get-timestamp (behavior-changes (second b)))) behaviorpairs))] ;; every timestamp mentioned in any value behavior
         [unique-ts (sort (remove-duplicates (append all-bool-ts all-val-ts)) <)] ;; sorted list of all unique timestamps
         [enhanced-behaviorpairs (map (λ (bp) (list (behavior (behavior-init (first bp)) (project-values (first bp) unique-ts)) ;; every behavior with every timestamp made explicit
                                                    (behavior (behavior-init (second bp)) (project-values (second bp) unique-ts)))) behaviorpairs)]
         [fused-pairs (map (λ (bp) (behavior (list (behavior-init (first bp)) (behavior-init (second bp)))
                                             (map (λ (b1 b2) (list (get-timestamp b1) (list (get-value b1) (get-value b2))))
                                                  (behavior-changes (first bp)) (behavior-changes (second bp))))) enhanced-behaviorpairs)]
         [guarded-init (ormap (λ (b) (if (first (behavior-init b)) (behavior-init b) #f)) fused-pairs)]
         [final-init (if (first guarded-init) (second guarded-init) #f)]
         [final-changes (map (λ (tsitems) (list (get-timestamp (first tsitems))
                                                (ormap (λ (item) (if (first (get-value item)) (get-value item) #f)) tsitems)))
                             (apply (curry map list) (map behavior-changes fused-pairs)))]
         )
    (behavior final-init (map (λ (c) (if (get-value c) (list (get-timestamp c) (second (get-value c))) c)) final-changes))))

(define (ifB conditionB trueB falseB)
  (let* ([unique-ts (sort (remove-duplicates (append (map get-timestamp (behavior-changes conditionB))
                                                     (map get-timestamp (behavior-changes trueB))
                                                     (map get-timestamp(behavior-changes falseB)))) <)]
        [enhanced-condB (project-values conditionB unique-ts)]
        [enhanced-trueB (project-values trueB unique-ts)]
        [enhanced-falseB (project-values falseB unique-ts)])
  (behavior (if (behavior-init conditionB) (behavior-init trueB) (behavior-init falseB))
            (map (λ (c t f) (list (get-timestamp c) (if (get-value c) (get-value t) (get-value f)))) enhanced-condB enhanced-trueB enhanced-falseB))))

;; timerB

(define (blindB interval b)
  (behavior (behavior-init b) (blindE interval (changes b))))

(define (calmB interval b)
  (behavior (behavior-init b) (calmE interval (changes b))))

