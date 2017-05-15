#lang racket

;; inputs/outputs of operators are pairs of timestamps and values
(define (getClicksE id)
  (lambda (input) (cons (car input) (hash-ref (cdr input) id))))

(define (getConstantE const)
  (λ (p) (if (cdr p) (cons (car p) const) (cons (car p) #f))))

(define (getCollectE init-state func pipe)
  (λ (p) (let ([new-state (func init-state (cdr (pipe p)))])
           (set! init-state new-state)
           (cons (car p) new-state))))

(define (getMergeE pipe1 pipe2) ;; assume arity 2 for now, also that only 1 input can be received at each tick
  (λ (p) (let ([i1 (pipe1 p)]
        [i2 (pipe2 p)])
    (if (cdr i1) i1 i2))))

(define (getInsertDomE id)
  (λ (p) (cons (car p) (make-hash (list (cons id (cdr p)))))))


(define (chain f1 f2)
  (λ (p) (f2 (f1 p))))

;; note startsWith operator is missing
(define (construct-graph)
  (let* ([incBtnClickE (getClicksE 'inc)] ;; input -> event stream
         [incConstE (getConstantE 1)] ;; void -> event stream
         [decConstE (getConstantE -1)] ;; void -> event stream
         [incBtnConstE (chain incBtnClickE incConstE)] ;; event stream -> event stream -> event stream
         [decBtnConstE (chain (getClicksE 'dec) decConstE)] ;; event stream -> event stream -> event stream
         [mergedBtnsE (getMergeE incBtnConstE decBtnConstE)] ;; evtstrm -> evtstrm -> evtstrm
         [collectedBtnsE (getCollectE 0 (λ (s p) (if (number? p) (+ s p) s))  mergedBtnsE)] ;; evtstrm -> evtstrm
         [outputTextEltE (chain collectedBtnsE (getInsertDomE 'total))] ;; evtstrm -> output
         )
    (λ (input)
      (outputTextEltE input))))

(define (execute-graph input-list)
  (define graph (construct-graph))
  (for/list ([i input-list])
    (graph i)))

(define (make-inputs incBtnClick decBtnClick)
  (make-hash (list (cons 'inc incBtnClick) (cons 'dec decBtnClick))))

(define sample-input
  (list (cons 1 (make-inputs #t #f))
        (cons 2 (make-inputs #f #f))
        (cons 3 (make-inputs #f #f))
        (cons 4 (make-inputs #f #t))
        (cons 5 (make-inputs #f #t))
        (cons 6 (make-inputs #f #t))))

(display "Execute inc/dec buttons example on sample input \n")

(execute-graph sample-input)

;; behaviors are stateful
(struct behavior (val) #:transparent)

(define (getInputB id)
  (λ (p) (behavior (hash-ref (cdr p) id) (car p))))
(define (inputB id p)
  (behavior (hash-ref (cdr p) id)))

(define (getClockB)
  (λ (p) (behavior (car p) (car p))))
(define (clockB p)
  (behavior (car p)))

(define (getLiftB func b)
  (λ () (behavior (func (behavior-val b)))))
(define (liftB func b)
  (behavior (func (behavior-val b))))

(define (getAndB b1 b2)
  (λ () (behavior (and (behavior-val b1) (behavior-val b2)))))
(define (andB b1 b2)
  (behavior (and (behavior-val b1) (behavior-val b2))))

(define (getIfB b1 b2 b3)
  (λ () (if (behavior-val b1) b2 b3)))
(define (ifB b1 b2 b3)
  (if (behavior-val b1) b2 b3))

(define (getConstB const b)
  (behavior const))
(define (constB const)
  (behavior const))

(define (construct-heater-graph)
  (λ (p)
    (let* ([liftedClockB (liftB (λ (v) (or (> v 2100) (< v 800))) (clockB p))] ;; input -> B
           [liftedTempB (liftB (λ (v) (< v 60)) (inputB 'temp p))] ;; input -> B
           [conjClockTempB (andB liftedClockB liftedTempB)] ;; B -> B -> B
           [constOnB (constB "on")] ;; B
           [constOffB (constB "off")] ;; B
           [ifConjB (ifB conjClockTempB constOnB constOffB)] ;; B -> B -> B -> B
           [outputB (λ(p) (cons (car p) (make-hash (list (cons 'heater ifConjB)))))]) ;; B -> output
      (outputB p))))


;; input behaviors are lists of timestamps and values at that time
;; output behaviors are similarly list of timestamps and values at that time
(define (execute-iot-graph input-list)
  (define heater-graph (construct-heater-graph))
  (for/list ([i input-list])
    (heater-graph i)))

(display "Execute IoT thermostat example on sample input \n")

(define sample-iot-input
  (list (cons 2030 (make-hash (list (cons 'temp 60))))
        (cons 2045 (make-hash (list (cons 'temp 60))))
        (cons 2105 (make-hash (list (cons 'temp 60))))
        (cons 2107 (make-hash (list (cons 'temp 59))))))

(execute-iot-graph sample-iot-input)