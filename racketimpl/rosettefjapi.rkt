#lang rosette/safe
(provide (all-defined-out))

(require "fjmodels.rkt")

;;;;; flapjax API ;;;;;

(define (oneE evt-stream)
  (unless (empty? evt-stream)
    (list (first evt-stream)))) ;; propagate only the first event

(define (zeroE evt-stream)
  (map (λ (e) (list (first e) (void))) evt-stream)) ;; a stream that never fires

(define (mapE proc evt-stream) ;; proc operates over both timestamp and value (kind of a cheat)
  (map (λ (e) (proc e)) evt-stream))

(define (mergeE evt-stream1 evt-stream2) ;; note: mergeE can actually take any num of args
  (sort (append evt-stream1 evt-stream2) (λ (x y) (< (get-timestamp x) (get-timestamp y))))) ;))

(define (switchE stream-of-streams)
  (if (empty? stream-of-streams)
      '()
      (let ([ts (map (λ (e) (get-timestamp e)) stream-of-streams)])
        (apply append (map (λ (start-ts end-ts vals) (if end-ts
                                           (boundedTimestampsStream start-ts end-ts (get-value vals))
                                           (startAtTimestamp start-ts (get-value vals))))
             ts (append (list-tail ts 1) (list #f)) stream-of-streams)))))

;; condE

(define (filterE stream pred)
  (filter (λ (e) (if (pred (get-value e)) (list (get-timestamp e) (pred (get-value e))) #f)) stream))

;; ifE

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

;; notE

;; filterRepeatsE

;; send/receive

(define (snapshotE evt-stream behavior1)
  (let ([real-evt-stream (filter (λ (e) (not (eq? (get-value e) 'no-evt))) evt-stream)])
    (map (λ (t) (list (get-timestamp t) (valueNow behavior1 (get-timestamp t)))) real-evt-stream)))

;; onceE

;; skipFirstE

(define (delayE evt-stream-f interval)
  (λ ()
    (let ([evt-stream (evt-stream-f)])
      (map (λ(s) (list (+ (get-timestamp s) interval) (get-value s))) evt-stream))))

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

#;(define (startsWith evt-stream init-value)
  ;(λ ()
  ;  (let* ([evt-stream (evt-stream-f)])
      (append (list (list 0 init-value)) evt-stream)) ;))

(define (startsWith evt-stream init-value)
  (behavior init-value evt-stream))

(define (changes behaviorB)
  (λ ()
    (behavior-changes behaviorB)))

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

(define (liftB proc . argBs)
  (let* ([unique-ts (sort (remove-duplicates (flatten (map (λ (b) (map get-timestamp (behavior-changes b))) argBs))) <)]
         [enhanced-argBs (map (λ (b) (project-values b unique-ts)) argBs)])
  (behavior (apply proc (map behavior-init argBs)) (apply (curry map (λ e (list (get-timestamp (first e)) (apply proc (map get-value e))))) enhanced-argBs))))

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

