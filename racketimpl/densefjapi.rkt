#lang rosette
(provide (all-defined-out))

(require "dense-fjmodels.rkt")

;;;;;;;;; operator wishlist ;;;;;;
;;
;; highFreqE: propagate events that occur x seconds after prev
;; collectE but where internal value and output value can be different
;; mapE, collectE, liftB etc where the functions have access to timestamps
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; flapjax API ;;;;;

(define (oneE evt-stream)
  (let ([val (findf not-empty-event? evt-stream)])
    (if val (list val) '())))

(define (zeroE)
  '()) ;; a stream that never fires

;; is it better to return e or no-evt if event is empty?
(define (mapE proc evt-stream)
  (map (λ (e) (if (empty-event? e) e (proc e))) evt-stream))

(define (mergeE evt-stream1 evt-stream2)
  (map (λ (evt1 evt2) (if (empty-event? evt2) evt1 evt2))
       evt-stream1 evt-stream2))

(define (switchE stream-of-streams)
  (letrec ([f (λ (s-of-s current-stream)
                (if (and (empty? s-of-s) (empty? current-stream))
                    '()
                    (let ([next-stream (first s-of-s)])
                      (cond [(and (or (empty-event? next-stream) (empty? next-stream)) (empty? current-stream)) (append (list 'no-evt) (f (rest s-of-s) current-stream))]
                            [(and (or (empty-event? next-stream) (empty? next-stream)) (not (empty? current-stream))) (append (list (first current-stream)) (f (rest s-of-s)
                                                                                                                                     (rest current-stream)))]
                            [else (append (list (first next-stream)) (f (rest s-of-s) (rest next-stream)))]))))])
    (f stream-of-streams '())))


#;(define (switchE stream-of-streams)
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
  (map (λ (e) (if (and (not-empty-event? e) (pred e)) e 'no-evt)) stream))

(define (ifE guard-stream true-stream false-stream)
  (map (λ (guard true false) (if (empty-event? guard) 'no-evt (if guard true false)))
       guard-stream true-stream false-stream))

(define (constantE const evt-stream)
  (map (λ (x) (if (empty-event? x) 'no-evt const)) evt-stream))

(define (collectE init proc evt-stream)
  (letrec ([collect (λ (x-lst prev)
                      (if (empty? x-lst)
                          '()
                          (let ([evt (first x-lst)])
                            (if (empty-event? evt)
                                (cons 'no-evt (collect (rest x-lst) prev))
                                (cons (proc evt prev) (collect (rest x-lst) (proc evt prev)))))))])
    (collect evt-stream init)))

;; andE

;; orE

(define (notE evt-stream)
  (map (λ (e) (if (empty-event? e) e (not e))) evt-stream))

;; filterRepeatsE

;; send/receive

#;(define (snapshotE evt-stream behavior1)
  (let ([real-evt-stream (filter (λ (e) (not (eq? (get-value e) 'no-evt))) evt-stream)])
    (map (λ (t) (list (get-timestamp t) (valueNow behavior1 (get-timestamp t)))) real-evt-stream)))

;; onceE

;; skipFirstE

(define (delayE interval evt-stream)
  (append (for/list ([i interval]) 'no-evt) evt-stream))

(define (blindE interval evt-stream)
  (letrec ([f (λ (evts wait-time)
                (cond [(empty? evts) '()]
                      [(and (>= 0 wait-time) (not-empty-event? (first evts))) (cons (first evts) (f (rest evts) interval))]
                      [else (cons 'no-evt (f (rest evts) (sub1 wait-time)))]))])
    (f evt-stream 0)))

(define (calmE interval evt-stream)
  (letrec ([f (λ (evts last-sent buffered-evt)
                (cond [(and (empty? evts) (empty-event? buffered-evt)) '()]
                      [(and (empty? evts) (>= last-sent interval)) (list buffered-evt)]
                      [(empty? evts) (cons 'no-evt (f evts (add1 last-sent) buffered-evt))]
                      [else (let ([output (if (>= last-sent interval) buffered-evt 'no-evt)]
                                  [new-buffer (if (not-empty-event? (first evts)) (first evts) buffered-evt)]
                                  [new-last-sent (cond [(and (not-empty-event? (first evts))
                                                          (>= last-sent interval)) 0]
                                                       [(< last-sent interval) (add1 last-sent)]
                                                       [else last-sent])])
                              (cons output (f (rest evts) new-last-sent new-buffer)))]))])
    (f evt-stream 0 'no-evt)))

(define (startsWith init-value evt-stream)
  (letrec ([f (λ (current evts)
             (cond [(empty? evts) '()]
                   [(empty-event? (first evts)) (cons current (f current (rest evts)))]
                   [else (cons (first evts) (f (first evts) (rest evts)))]))])
  (behavior init-value (f init-value evt-stream))))

#;(define (changes behaviorB)
    (behavior-changes behaviorB))

(define (constantB const)
  (behavior const '()))

#;(define (delayB interval behavior1)
  (behavior (behavior-init behavior1) (delayE interval (behavior-changes behavior1))))

;; valueNow: since valueNow shouldn't be exposed to end users, it's in fjmodels.rkt

;; switchB
;; switchBB takes a behavior of behaviors: (behavior behavior1 (list behavior2 behavior3 behavior4)))
;; and returns a behavior: (behavior (behavior-init behavior1) (append (behavior-changes behavior1) (behavior-changes behavior2) ...)))

(define (andB behavior1 behavior2)
  (let* ([max-len (max (length (behavior-changes behavior1)) (length (behavior-changes behavior2)))]
         [padded-b1 (pad-behavior behavior1 max-len)]
         [padded-b2 (pad-behavior behavior2 max-len)])
  (behavior (and (behavior-init behavior1) (behavior-init behavior2))
            (map (λ (b1 b2) (and b1 b2)) (behavior-changes padded-b1) (behavior-changes padded-b2)))))

(define (orB behavior1 behavior2)
  (let* ([max-len (max (length (behavior-changes behavior1)) (length (behavior-changes behavior2)))]
         [padded-b1 (pad-behavior behavior1 max-len)]
         [padded-b2 (pad-behavior behavior2 max-len)])
    (behavior (or (behavior-init behavior1) (behavior-init behavior2))
              (map (λ (b1 b2) (or b1 b2)) (behavior-changes padded-b1) (behavior-changes padded-b2)))))

(define (notB behavior1)
  (behavior (not (behavior-init behavior1)) (notE (behavior-changes behavior1))))

#;(define (liftB proc . argBs)
  (let* ([unique-ts (sort (remove-duplicates (flatten (map (λ (b) (map get-timestamp (behavior-changes b))) argBs))) <)]
         [enhanced-argBs (map (λ (b) (project-values b unique-ts)) argBs)])
  (behavior (apply proc (map behavior-init argBs)) (apply (curry map (λ e (list (get-timestamp (first e)) (apply proc (map get-value e))))) enhanced-argBs))))

(define (liftB1 proc argB)
  (behavior (proc (behavior-init argB)) (map proc (behavior-changes argB))))

(define (liftB2 proc argB1 argB2)
  (behavior (proc (behavior-init argB1) (behavior-init argB2))
            (map proc (behavior-changes argB1) (behavior-changes argB2))))

;; is there an easier way??
#;(define (condB behaviorpairs)
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
  (let ([max-len (max (length (behavior-changes conditionB)) (length (behavior-changes trueB)) (length (behavior-changes falseB)))])
  (behavior (if (behavior-init conditionB) (behavior-init trueB) (behavior-init falseB))
            (map (λ (cB tB fB) (if cB tB fB))
                 (pad-behavior-changes conditionB max-len)
                 (pad-behavior-changes trueB max-len)
                 (pad-behavior-changes falseB max-len)))))
;; timerB

#;(define (blindB interval b)
  (behavior (behavior-init b) (blindE interval (changes b))))

#;(define (calmB interval b)
  (behavior (behavior-init b) (calmE interval (changes b))))

