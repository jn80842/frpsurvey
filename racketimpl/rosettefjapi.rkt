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

(define (mergeE evt-stream1 evt-stream2) ;; note: mergeE can actually take any num of args
  (sort (append evt-stream1 evt-stream2) (λ (x y) (< (get-timestamp x) (get-timestamp y))))) ;))

(define (switchE stream-of-streams)
  (if (empty? stream-of-streams)
      '()
      (let ([ts (map (λ (e) (get-timestamp e)) stream-of-streams)])
        (apply append (filter (λ (l) (not (void? l)))
               (map (λ (start-ts end-ts vals) (if end-ts
                                           (boundedTimestampsStream start-ts end-ts (get-value vals))
                                           (startAtTimestamp start-ts (get-value vals))))
             ts (append (list-tail ts 1) (list #f)) stream-of-streams))))))

;; condE

(define (filterE stream pred)
  (filter (λ (e) (if (pred (get-value e)) (list (get-timestamp e) (pred (get-value e))) #f)) stream))

(define (ifE guard-stream value-stream)
  ;; filter both lists to ts they have in common
  ;; if guard evals to true, propagate corresponding value
  (let ([filtered-guard-stream (filter (λ (e) (and (get-value e) (member (get-timestamp e) (map get-timestamp value-stream))))
                                       guard-stream)])
    (filter (λ (e) (member (get-timestamp e) (map get-timestamp filtered-guard-stream))) value-stream)))

(define (constantE evt-stream const)
  (map (λ (s) (list (get-timestamp s) (if (equal? 'no-evt (get-value s)) 'no-evt const))) evt-stream)) ;))

(define (collectE evt-stream init proc)
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

(define (delayE evt-stream interval)
  (map (λ(s) (list (+ (get-timestamp s) interval) (get-value s))) evt-stream))

(define (blindE evt-stream interval)
  (letrec ([f (λ (evts last-sent)
                ;; if all events have been processed, done!
                (cond [(empty? evts) '()]
                      ;; if time between current event and last sent is geq interval
                      ;; propagate event, update last sent, and continue
                      [(<= interval (- (get-timestamp (first evts)) last-sent))
                       (append (list (first evts)) (f (list-tail evts 1) (get-timestamp (first evts))))]
                      ;; else, continue without propagating current event
                      [else (f (list-tail evts 1) last-sent)]))])
    (f evt-stream (- interval))))

(define (calmE evt-stream interval)
  (letrec ([emit-event (λ (evt) (list (list (+ (get-timestamp evt) interval) (get-value evt))))]
           [f (λ (evts buffered-evt)
                ;; if all events have been processed, propagate buffered event
                (cond [(and (empty? evts) buffered-evt) (emit-event buffered-evt)]
                      ;; if all events have been processed and no event is buffered, done!
                      [(empty? evts) '()]
                      ;; if nothing has been buffered, buffer event and continue
                      [(false? buffered-evt) (f (list-tail evts 1) (first evts))]
                      ;; if time between buffered event and next event is geq interval
                      ;; propagate buffered event and continue
                      [(<= interval (- (get-timestamp (first evts)) (get-timestamp buffered-evt)))
                       (append (emit-event buffered-evt) (f (list-tail evts 1) (first evts)))]
                      ;; if time between buffered event and next event is too small
                      ;; discard buffered event and continue
                      [else (f (list-tail evts 1) (first evts))]))])
    (f evt-stream #f)))

(define (startsWith evt-stream init-value)
  (behavior init-value evt-stream))

(define (changes behaviorB)
    (behavior-changes behaviorB))

(define (constantB const)
  (behavior const '()))

;; delayB

;; valueNow: since valueNow shouldn't be exposed to end users, it's in fjmodels.rkt

;; switchB
;; switchBB takes a behavior of behaviors: (behavior behavior1 (list behavior2 behavior3 behavior4)))
;; and returns a behavior: (behavior (behavior-init behavior1) (append (behavior-changes behavior1) (behavior-changes behavior2) ...)))

(define (andB behavior1 behavior2)
  ;(λ ()
    (let* ([unique-ts (sort (remove-duplicates (append (map get-timestamp (behavior-changes behavior1))
                                                       (map get-timestamp (behavior-changes behavior2)))) <)]
           [enhanced-b1 (project-values behavior1 unique-ts)]
           [enhanced-b2 (project-values behavior2 unique-ts)])
      (behavior (and (behavior-init behavior1) (behavior-init behavior2))
                (map (λ (x y) (list (get-timestamp x) (and (get-value x) (get-value y)))) enhanced-b1 enhanced-b2)))
    )

;; orB

(define (notB behavior1)
  (behavior (not (behavior-init behavior1)) (map (λ (b) (list (get-timestamp b) (not (get-value b)))) (behavior-changes behavior1))))

(define (liftB1 proc argB)
  (behavior (proc (behavior-init argB)) (map (λ (e) (list (get-timestamp e) (proc (get-value e)))) (behavior-changes argB))))

(define (liftB2 proc argB1 argB2)
  (let* ([unique-ts (sort (remove-duplicates (append (map get-timestamp (behavior-changes argB1)) (map get-timestamp (behavior-changes argB2)))) <)]
         [enhanced-argB1 (project-values argB1 unique-ts)]
         [enhanced-argB2 (project-values argB2 unique-ts)])
    (behavior (proc (behavior-init argB1) (behavior-init argB2)) (map (λ (e1 e2) (list (get-timestamp e1) (proc (get-value e1) (get-value e2)))) enhanced-argB1 enhanced-argB2))))

(define (liftB proc . argBs)
  (let* ([unique-ts (sort (remove-duplicates (flatten (map (λ (b) (map get-timestamp (behavior-changes b))) argBs))) <)]
         [enhanced-argBs (map (λ (b) (project-values b unique-ts)) argBs)])
  (behavior (apply proc (map behavior-init argBs)) (apply (curry map (λ e (list (get-timestamp (first e)) (apply proc (map get-value e))))) enhanced-argBs))))

(define (liftB-no-let proc . argBs)
  (behavior (apply proc (map behavior-init argBs)) (apply (curry map (λ e (list (get-timestamp (first e)) (apply proc (map get-value e)))))
                                                          (map (λ (b) (project-values b (sort (remove-duplicates (flatten (map (λ (b) (map get-timestamp (behavior-changes b))) argBs))) <))) argBs))))

(define (liftB-no-enhanced proc . argBs)
  (behavior (apply proc (map behavior-init argBs)) (apply (curry map (λ e (list (get-timestamp (first e)) (apply proc (map get-value e))))) (map behavior-changes argBs))))

(define (liftB-list proc . argLists)
  (let* ([unique-ts (sort (remove-duplicates (flatten (map (λ (l) (map get-timestamp l)) argLists))) <)]
         [enhanced-Lists (map (λ (l) (project-values-list l unique-ts)) argLists)])
    (apply (curry map (λ e (list (get-timestamp (first e)) (apply proc (map get-value e))))) enhanced-Lists)))

(define (liftB-list-no-let proc argLists)
  (apply (curry map (λ e (list (get-timestamp (first e)) (apply proc (map get-value e))))) argLists))

(define (liftB-list-no-let-no-apply proc argLists)
  (map (λ e (list (get-timestamp (first e)) (apply proc (map get-value e)))) argLists))

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

;; blindB

;; calmB

