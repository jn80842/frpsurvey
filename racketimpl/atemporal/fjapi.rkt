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

(define (identityE evt)
  evt)

#;(define (oneE evt-stream)
  (unless (empty? evt-stream)
    (list (first evt-stream)))) ;; propagate only the first event

;; onceE can be in state of having fired in the past, or having never fired
(define (onceE state evt)
  (if state
      NOEVENT
      evt))

(define (zeroE)
  NOEVENT) ;; a stream that never fires

(define (mapE proc evt)
  (if (empty-event? evt) NOEVENT (proc evt)))

(define (mapE2 proc evt1 evt2)
  (if (or (empty-event? evt1) (empty-event? evt2)) NOEVENT (proc evt1 evt2)))

(define (mergeE evt1 evt2)
  (if (empty-event? evt2) evt1 evt2))

#;(define (switchE stream-of-streams)
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

(define (filterE pred evt)
  (if (and (not-empty-event? evt) (pred evt)) evt NOEVENT))

(define (ifE guard-evt true-evt false-evt)
  (if (empty-event? guard-evt) NOEVENT (if guard-evt true-evt false-evt)))

(define (constantE const evt)
  (if (empty-event? evt) NOEVENT const))

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

;; collectE has internal state: its previous value
(define (collectE state proc evt)
  (if (empty-event? evt)
      NOEVENT
      (proc state evt)))

(define (andE evt-1 evt-2)
  (if (and (not (empty-event? evt-1))
           (not (empty-event? evt-2))
           (and evt-1 evt-2))
      #t
      NOEVENT))

(define (orE evt-1 evt-2)
  (if (and (not (empty-event? evt-1))
           (not (empty-event? evt-2))
           (or evt-1 evt-2))
      #t
      NOEVENT))

(define (notE evt)
  (if (empty-event? evt) NOEVENT (not evt)))

(define (maskOnE mask-evt signal-evt)
  (if (empty-event? mask-evt) NOEVENT
      (if mask-evt signal-evt NOEVENT)))

(define (maskOffE mask-evt signal-evt)
  (if (and (not-empty-event? mask-evt) mask-evt) NOEVENT signal-evt))

;; filterRepeatsE: internal state is the last value it saw

(define (filterRepeatsE state evt)
  (if (or (empty-event? evt) (equal? state evt))
      NOEVENT
      evt))

;; send/receive

(define (snapshotE evt b)
  (if (not-empty-event? evt) b NOEVENT))
;; onceE

;; skipFirstE: internal state is the number of values it has seen
(define (skipFirstE state n evt)
  (if (or (empty-event? evt) (> n state))
      NOEVENT
      evt))

;; delayE: internal state is queued event & how long it's been queued for
(define (delayE queued-evt wait-length interval evt)
  (if (or (empty-event? queued-evt) (> interval wait-length))
      NOEVENT
      queued-evt))
 
;; punt on blindE and calmE

(define (startsWith prev-val evt)
  (if (empty-event? evt)
      prev-val
      evt))

(define (changes prev-val b)
  (filterRepeatsE prev-val b))

(define (constantB const b)
  const)

#;(define (delayB interval behavior1)
  (behavior (behavior-init behavior1) (delayE interval (behavior-changes behavior1))))

;; valueNow: since valueNow shouldn't be exposed to end users, it's in fjmodels.rkt

;; switchB
;; switchBB takes a behavior of behaviors: (behavior behavior1 (list behavior2 behavior3 behavior4)))
;; and returns a behavior: (behavior (behavior-init behavior1) (append (behavior-changes behavior1) (behavior-changes behavior2) ...)))

;; assumption is that all behaviors are complete for full timeline
#;(define (switchB inputBB)
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

(define (andB b1 b2)
  (and b1 b2))

(define (orB b1 b2)
  (or b1 b2))

(define (notB b)
  (not b))

#;(define (liftB proc . argBs)
  (let* ([unique-ts (sort (remove-duplicates (flatten (map (λ (b) (map get-timestamp (behavior-changes b))) argBs))) <)]
         [enhanced-argBs (map (λ (b) (project-values b unique-ts)) argBs)])
  (behavior (apply proc (map behavior-init argBs)) (apply (curry map (λ e (list (get-timestamp (first e)) (apply proc (map get-value e))))) enhanced-argBs))))

(define (liftB proc b)
  (proc b))

(define (liftB2 proc argB1 argB2)
  (proc argB1 argB2))

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
  (if conditionB trueB falseB))

;; timerB

#;(define (blindB interval b)
  (behavior (behavior-init b) (blindE interval (changes b))))

#;(define (calmB interval b)
  (behavior (behavior-init b) (calmE interval (changes b))))

(define (collectB state proc b)
  (proc state b))

