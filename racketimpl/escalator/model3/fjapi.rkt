#lang rosette
(provide (all-defined-out))

;; model methods here since we only need a few of them
(define (iff p q)
  (or (and p q) (and (not p) (not q))))

(define (get-sym-bool)
  (define-symbolic* b boolean?)
  b)

(define (get-sym-int)
  (define-symbolic* i integer?)
  i)

(define NOEVENT 'no-evt)

(define (empty-event? e)
  (eq? NOEVENT e))

(define (not-empty-event? e)
  (not (eq? NOEVENT e)))

(define (new-event-stream constructor)
  (if (get-sym-bool)
      (constructor)
      NOEVENT))

;;;;; flapjax API ;;;;;

(define (identityE evt)
  evt)

;; onceE w/o state is just identityE

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

;; collectE is just mapE with state, so omitted

(define (andE evt-1 evt-2)
  (if (and (not (empty-event? evt-1))
           (not (empty-event? evt-2))
           (and evt-1 evt-2))
      #t
      NOEVENT))

(define (orE evt-1 evt-2)
  (let ([e1 (if (empty-event? evt-1) #f evt-1)]
        [e2 (if (empty-event? evt-2) #f evt-2)])
    (or e1 e2)))

#;(define (orE evt-1 evt-2)
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

;; filterRepeatsE w/o state is just identityE

;; send/receive

;; snapshotE omitted since there are no behaviors

;; onceE

;; skipFirstE w/o state would be zeroE

;; delayE w/o state would be zeroE
 
;; all behavior operators omitted