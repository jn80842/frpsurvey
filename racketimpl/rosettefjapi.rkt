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

(define (valid-input-timestamps? input-stream)
  (and (apply distinct? (map get-timestamp input-stream))
       (andmap integer? (map get-timestamp input-stream))
       (andmap positive? (map get-timestamp input-stream))
       (timestamps-sorted? input-stream)))

(define (assert-valid-input-timestamps? input-stream)
  (begin (assert (apply distinct? (map get-timestamp input-stream)))
         (assert (andmap integer? (map get-timestamp input-stream)))
         (assert (andmap positive? (map get-timestamp input-stream)))
         (assert (timestamps-sorted? input-stream))))

(define (valid-output-timestamps? output-stream)
    (and (apply distinct? (map get-timestamp output-stream))
       (andmap integer? (map get-timestamp output-stream))
       ;; outputs can have 0 timestamp
       (andmap (λ (n) (not (negative? n))) (map get-timestamp output-stream))
       (timestamps-sorted? output-stream)))

(define (assert-valid-output-timestamps? output-stream)
  (begin (assert (apply distinct? (map get-timestamp output-stream)))
         (assert (andmap integer? (map get-timestamp output-stream)))
         ;; outputs can have 0 timestamp
         (assert (andmap (λ (n) (not (negative? n))) (map get-timestamp output-stream)))
         (assert (timestamps-sorted? output-stream))))

(define (timestamps-sorted? stream)
  (equal? (map get-timestamp stream) (sort (map get-timestamp stream) <)))

;;;; note for all behavior operators:                       ;;;;
;;;; behaviors *always* have a value (can never be 'no-evt) ;;;;

(struct behavior (init changes)
  #:transparent)

(define (project-values b ts)
  (map (λ (t) (list t (valueNow b t))) ts))

(define (get-missing-timestamps b1 b2)
  (filter (λ (t) (not (member t (map get-timestamp (behavior-changes b1)))))
          (map get-timestamp (behavior-changes b2))))

(define (valid-behavior? b)
  (and (behavior? b)
       (apply distinct? (map get-timestamp (behavior-changes b)))
       (andmap positive? (map get-timestamp (behavior-changes b)))
       (timestamps-sorted? (behavior-changes b))))

;;;;; inputs ;;;;;;;;;;
(define (boolean-event-stream concrete-list)
  (map (λ (c)
         (define-symbolic* timestamp integer?)
         (define-symbolic* value boolean?)
         (list timestamp value)) concrete-list))

(define (integer-event-stream concrete-list)
  (map (λ (c)
         (define-symbolic* timestamp integer?)
         (define-symbolic* value integer?)
         (list timestamp value)) concrete-list))

(define (boolean-behavior concrete-list)
  (define-symbolic* init-val boolean?)
  (behavior init-val (boolean-event-stream concrete-list)))

(define (integer-behavior concrete-list)
  (define-symbolic* init-val integer?)
  (behavior init-val (integer-event-stream concrete-list)))

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
      (sort (append evt-stream1 evt-stream2) (λ (x y) (< (get-timestamp x) (get-timestamp y)))))))

(define (switchE stream-of-streams-f)
  (λ ()
    (let ([stream-of-streams (map (λ (f) (apply (second f) '()))
                                  stream-of-streams-f)])
      (sort (flatten stream-of-streams) (λ (x y) (< (get-timestamp x) (get-timestamp y)))))))

;; condE

;; filterE

;; ifE

(define (constantE evt-stream-f const)
  (λ ()
    (let ([evt-stream (evt-stream-f)])
      (map (λ (s) (list (get-timestamp s) (if (equal? 'no-evt (get-value s)) 'no-evt const))) evt-stream))))

(define (collectE evt-stream-f init proc)
  (λ ()
    (let ([evt-stream (evt-stream-f)])
      (letrec ([collect (λ (x-lst prev)
                          (if (equal? (length x-lst) 0)
                              '()
                              (let* ([new-ts (get-timestamp (first x-lst))]
                                    [input-val (get-value (first x-lst))]
                                    [new-val (if (equal? input-val 'no-evt) prev (proc (get-value (first x-lst)) prev))])
                                (append (list (list new-ts new-val))
                                        (collect (cdr x-lst) new-val)))))])
        (collect evt-stream init)))))

;; andE

;; orE

;; notE

;; filterRepeatsE

;; send/receive

(define (snapshotE evt-stream-f behavior1)
  (λ ()
    (let ([real-evt-stream (filter (λ (e) (not (eq? (get-value e) 'no-evt))) (evt-stream-f))])
      (map (λ (t) (list (get-timestamp t) (valueNow behavior1 (get-timestamp t)))) real-evt-stream))))

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

#;(define (startsWith evt-stream-f init-value)
  (λ ()
    (let* ([evt-stream (evt-stream-f)])
      (append (list (list 0 init-value)) evt-stream))))

(define (startsWith evt-stream-f init-value)
  (behavior init-value (evt-stream-f)))

;; changes



(define (constantB const)
  (behavior const '()))

;; delayB

;; valueNow

(define (valueNow behavior1 ts) ;; altered a bit for our own use
  (let ([filtered-changes (filter (λ (t) (<= (get-timestamp t) ts)) (behavior-changes behavior1))])
    (if (empty? filtered-changes)
        (behavior-init behavior1)
        (get-value (last filtered-changes)))))

;; switchB

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

(define (liftB proc behavior1) ;; note: procedure can technically take multiple behaviors as args
  (behavior (proc (behavior-init behavior1)) (map (λ (b) (list (get-timestamp b) (proc (get-value b)))) (behavior-changes behavior1))))

;; condB

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

