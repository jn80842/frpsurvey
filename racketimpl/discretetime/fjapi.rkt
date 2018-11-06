#lang rosette
(provide (all-defined-out))

(require "model.rkt")

;;;;;;;;; operator wishlist ;;;;;;
;;
;; highFreqE: propagate events that occur x seconds after prev
;; collectE but where internal value and output value can be different
;; mapE, collectE, liftB etc where the functions have access to timestamps
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;; flapjax grammar ;;;;;
;;
;; S -> R | S R
;; R -> E | B
;; E -> identityE | onceE | zeroE | mapE Pred E E | mergeE E E
;;       | filterE Pred E | ifE E E E | constantE const E | collectE int Pred E 
;;       | filterRepeatsE E | snapshotE E B | delayE const E | timerE const E
;;       | changes B | BoolE 
;; BoolE -> andE BoolE BoolE | orE BoolE BoolE | notE BoolE 
;; B -> startsWith const E | constantB const B | delayB const B | andB B B | orB B B
;;       | notB B | liftB Pred B | ifB B B B | timerB const B 

;;;;; flapjax API ;;;;;

(define (identityE evt-stream)
  evt-stream)

(define (onceE evt-stream)
  (for/list ([i (range (length evt-stream))])
    (if (findf (λ (e) (not (empty-event? e))) (take evt-stream i))
        'no-evt
        (list-ref evt-stream i))))

(define (zeroE)
  '()) ;; a stream that never fires

;; is it better to return e or no-evt if event is empty?
(define (mapE proc evt-stream)
  (map (λ (e) (if (empty-event? e) e (proc e))) evt-stream))

(define (mapE2 proc evt-stream1 evt-stream2)
  (map (λ (e1 e2) (if (or (empty-event? e1) (empty-event? e2)) 'no-evt (proc e1 e2))) evt-stream1 evt-stream2))

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

#;(define (collectE init proc evt-stream)
  (letrec ([collect (λ (x-lst prev)
                      (if (empty? x-lst)
                          '()
                          (let ([evt (first x-lst)])
                            (if (empty-event? evt)
                                (cons 'no-evt (collect (rest x-lst) prev))
                                (cons (proc evt prev) (collect (rest x-lst) (proc evt prev)))))))])
    (collect evt-stream init)))

#;(define (collectE init proc stream)
  (map (λ (s n) (if (empty-event? s) 'no-evt n))
       stream
       (list-tail (reverse (foldl (λ (n lst) (cons (if (empty-event? n) (first lst) (proc n (first lst))) lst))
                                  (list init) stream)) 1)))

(define (collectE init proc lst)
  (for/list ([i (range (length lst))])
    (if (empty-event? (list-ref lst i))
        'no-evt
        (foldl (λ (n m) (if (empty-event? n) m (proc n m))) init (take lst (add1 i))))))

(define (collectE-plus init stream)
  (map (λ (s n) (if (empty-event? s) 'no-evt n))
       stream
       (list-tail (reverse (foldl (λ (n lst) (cons (if (empty-event? n) (first lst) (+ n (first lst))) lst))
                                  (list init) stream)) 1)))
(define (collectE-minus init stream)
  (map (λ (s n) (if (empty-event? s) 'no-evt n))
       stream
       (list-tail (reverse (foldl (λ (n lst) (cons (if (empty-event? n) (first lst) (- n (first lst))) lst))
                                  (list init) stream)) 1)))

(define (andE evt-stream1 evt-stream2)
  (map (λ (e1 e2) (if (and (not (empty-event? e1))
                           (not (empty-event? e2)))
                      (and e1 e2)
                      'no-evt)) evt-stream1 evt-stream2))

;; orE

(define (notE evt-stream)
  (map (λ (e) (if (empty-event? e) e (not e))) evt-stream))

(define (filterRepeatsE evt-stream)
  (letrec ([f (λ (evt rest)
                (cond [(empty? rest) evt]
                      [(equal? evt (first rest)) 'no-evt]
                      [(not-empty-event? (first rest)) evt]
                      [else (f evt (cdr rest))]))])
    (for/list ([i (range 1 (add1 (length evt-stream)))])
      (let ([lst (take evt-stream i)])
        (if (empty-event? (last lst))
            'no-evt
            (f (last lst) (cdr (reverse lst))))))))

;; send/receive

#;(define (snapshotE evt-stream behavior1)
  (let ([real-evt-stream (filter (λ (e) (not (eq? (get-value e) 'no-evt))) evt-stream)])
    (map (λ (t) (list (get-timestamp t) (valueNow behavior1 (get-timestamp t)))) real-evt-stream)))
(define (snapshotE evt-stream behavior1)
  (map (λ (e b) (if (not-empty-event? e) b e)) evt-stream (behavior-changes behavior1)))
;; onceE

;; skipFirstE

(define (delayE interval evt-stream)
 ; (append
   (for/list ([i (range (length evt-stream))])
    (if (> interval i)
        'no-evt
        (list-ref evt-stream (- i interval))))
         ; (list-tail evt-stream (- (length evt-stream) interval))))
  )

(define (blindE interval evt-stream)
  (letrec ([f (λ (evts wait-time)
                (cond [(empty? evts) '()]
                      [(and (>= 0 wait-time) (not-empty-event? (first evts))) (cons (first evts) (f (rest evts) interval))]
                      [else (cons 'no-evt (f (rest evts) (sub1 wait-time)))]))])
    (f evt-stream 0)))
#;(define (blindE interval evt-stream)
  (for/list ([i (range (length evt-stream))])
    (if (> interval (add1 i))
        (if (andmap empty-event? (take evt-stream i))
            (list-ref evt-stream i)
            'no-evt)
        (if (andmap empty-event? (take-right (take evt-stream i) interval))
            (list-ref evt-stream i)
            'no-evt))))
#;(define (blindE interval evt-stream)
  (for/list ([i (range (length evt-stream))])
    (let ([prior (if (<= (length (take evt-stream i)) interval)
                     (take evt-stream i)
                     (take-right (take evt-stream i) interval))])
      (if (ormap not-empty-event? prior)
          'no-evt
          (list-ref evt-stream i)))))

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

;; NB: passing an event stream to timerE is a bit of a hack
;; in Flapjax timerE's only arg is the interval
;; this could be fixed by specifying a size of the timeline
(define (guarded-modulo n m)
  (if (equal? m 0)
      'bad
      (modulo n m)))
#;(define (timerE interval evt-stream)
  (if (equal? interval 0)
      (map (λ (e) 'no-evt) evt-stream)
      (map (λ (n) (if (equal? 0 (guarded-modulo n interval))
                      #t
                      'no-evt))
           (range 1 (add1 (length evt-stream))))))
#;(define (timerE interval evt-stream)
  (define interval-val '(1 2 3 4 5 6 7 8 9 10))
  (map (λ (n) (if (equal? 0 (modulo n (list-ref interval-val (sub1 interval))))
                      #t
                      'no-evt))
           (range 1 (add1 (length evt-stream)))))
#;(define (timerE interval evt-stream)
  (let ([indices (range 1 (add1 (length evt-stream)))])
    (for/list ([i indices])
      (if (member  interval (take indices i))
          (if (equal? (modulo i interval) 0)
              #t
              'no-evt)
          'no-evt))))
;; despite best efforts, not sure how to use modulo in synthesis
;; without Rosette trying to set interval to 0?
(define timer-list (list '(no-evt no-evt no-evt no-evt no-evt no-evt no-evt no-evt no-evt no-evt)
                          '(#t #t #t #t #t #t #t #t #t #t)
                          '(no-evt #t no-evt #t no-evt #t no-evt #t no-evt #t)
                          '(no-evt no-evt #t no-evt no-evt #t no-evt no-evt #t no-evt)
                          '(no-evt no-evt no-evt #t no-evt no-evt no-evt #t no-evt no-evt)
                          '(no-evt no-evt no-evt no-evt #t no-evt no-evt no-evt no-evt #t)))
;; interval can only be 0-5, evt-stream can't be longer than 10
(define (timerE interval evt-stream)
  (take (list-ref timer-list interval) (length evt-stream)))

(define (startsWith init-value evt-stream)
  (behavior init-value (for/list ([i (range (length evt-stream))])
                         (findf (λ (e) (not (empty-event? e)))
                                (reverse (cons init-value (take evt-stream (add1 i))))))))

(define (changes behaviorB)
    (filterRepeatsE (behavior-changes behaviorB)))

(define (constantB const inputB)
  (behavior const (map (λ (e) const) (changes inputB))))

#;(define (delayB interval behavior1)
  (behavior (behavior-init behavior1) (delayE interval (behavior-changes behavior1))))

;; valueNow: since valueNow shouldn't be exposed to end users, it's in fjmodels.rkt

;; switchB
;; switchBB takes a behavior of behaviors: (behavior behavior1 (list behavior2 behavior3 behavior4)))
;; and returns a behavior: (behavior (behavior-init behavior1) (append (behavior-changes behavior1) (behavior-changes behavior2) ...)))

;; assumption is that all behaviors are complete for full timeline
(define (switchB inputBB)
  (let ([n (length (behavior-changes inputBB))])
    (behavior (behavior-init (behavior-init inputBB))
              (for/list ([i (range n)])
                (list-ref (behavior-changes (list-ref (behavior-changes inputBB) i)) i)))))

#;(define (andB behavior1 behavior2)
  (let* ([max-len (max (length (behavior-changes behavior1)) (length (behavior-changes behavior2)))]
         [padded-b1 (pad-behavior behavior1 max-len)]
         [padded-b2 (pad-behavior behavior2 max-len)])
  (behavior (and (behavior-init behavior1) (behavior-init behavior2))
            (map (λ (b1 b2) (and b1 b2)) (behavior-changes padded-b1) (behavior-changes padded-b2)))))

(define (andB behavior1 behavior2)
  (behavior (and (behavior-init behavior1) (behavior-init behavior2))
            (map (λ (b1 b2) (and b1 b2)) (behavior-changes behavior1) (behavior-changes behavior2))))

#;(define (orB behavior1 behavior2)
  (let* ([max-len (max (length (behavior-changes behavior1)) (length (behavior-changes behavior2)))]
         [padded-b1 (pad-behavior behavior1 max-len)]
         [padded-b2 (pad-behavior behavior2 max-len)])
    (behavior (or (behavior-init behavior1) (behavior-init behavior2))
              (map (λ (b1 b2) (or b1 b2)) (behavior-changes padded-b1) (behavior-changes padded-b2)))))

(define (orB behavior1 behavior2)
  (behavior (or (behavior-init behavior1) (behavior-init behavior2))
            (map (λ (b1 b2) (or b1 b2)) (behavior-changes behavior1) (behavior-changes behavior2))))

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

;; note: isn't condB equiv to nested ifBs?
#;(define (condB behaviorpairs [dummyinput 0])
  (let* ([all-behaviors (flatten behaviorpairs)]
         [max-len (apply max (map (λ (b) (length (behavior-changes b))) all-behaviors))])
    (behavior (second (findf (λ (bp) (first bp)) (map (λ (bp) (list (behavior-init (first bp)) (behavior-init (second bp)))) behaviorpairs)))
              (map (λ (y) (second (findf (λ (bp) (first bp)) y))) (apply (curry map list) (map (λ (x) (apply (curry map (λ (b1 b2) (list b1 b2))) x))
                                           (map (λ (bp) (list (pad-behavior-changes (first bp) max-len) (pad-behavior-changes (second bp) max-len))) behaviorpairs)))
              ))))

#;(define (ifB conditionB trueB falseB)
  (let ([max-len (max (length (behavior-changes conditionB)) (length (behavior-changes trueB)) (length (behavior-changes falseB)))])
  (behavior (if (behavior-init conditionB) (behavior-init trueB) (behavior-init falseB))
            (map (λ (cB tB fB) (if cB tB fB))
                 (pad-behavior-changes conditionB max-len)
                 (pad-behavior-changes trueB max-len)
                 (pad-behavior-changes falseB max-len)))))

(define (ifB conditionB trueB falseB)
  (behavior (if (behavior-init conditionB) (behavior-init trueB) (behavior-init falseB))
            (map (λ (cB tB fB) (if cB tB fB))
                 (behavior-changes conditionB)
                 (behavior-changes trueB)
                 (behavior-changes falseB))))

;; timerB

#;(define (blindB interval b)
  (behavior (behavior-init b) (blindE interval (changes b))))

#;(define (calmB interval b)
  (behavior (behavior-init b) (calmE interval (changes b))))

#;(define (collectE init proc evt-stream)
  (letrec ([collect (λ (x-lst prev)
                      (if (empty? x-lst)
                          '()
                          (let ([evt (first x-lst)])
                            (if (empty-event? evt)
                                (cons 'no-evt (collect (rest x-lst) prev))
                                (cons (proc evt prev) (collect (rest x-lst) (proc evt prev)))))))])
    (collect evt-stream init)))

;; this is an original operator!
(define (collectB init-val proc b1)
  (let ([b-init (proc init-val (behavior-init b1))])
         (letrec ([collect (λ (lst prev)
                       (if (empty? lst)
                           '()
                           (cons (proc (first lst) prev) (collect (rest lst) (proc (first lst) prev)))))])
           (behavior b-init (collect (behavior-changes b1) b-init)))))

#;(define (collectB init proc lst)
  (behavior init (for/list ([i (range (length lst))])
                   (foldl (λ (n m) (if (empty-event? n) m (proc n m))) init (take lst (add1 i))))))

