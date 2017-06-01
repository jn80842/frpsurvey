#lang racket

(struct button (click) #:transparent)
(struct text-area (text) #:transparent)
(struct timer (value) #:transparent)
(struct inputs (button text-area timer) #:transparent)
(struct save-event (text) #:transparent)

(define sample-inputs
  (list
   (list 0 (inputs #f "" 0))
   (list 1 (inputs #f "hello" 1))
   (list 2 (inputs #f "hello world" 2))
   (list 3 (inputs #t "hello world" 3))
   (list 4 (inputs #f "hello world!" 4))
   (list 5 (inputs #f "hello world!" 5))))

(define expected-outputs
  (list
   (list 3 (save-event "hello world"))
   (list 5 (save-event "hello world!"))))


(define (handle-inputs input-pair)
  (let ([curr-inputs (second input-pair)])
    (cond
      [(inputs-button curr-inputs) (inputs-text-area curr-inputs)]
      [(and (equal? (modulo (inputs-timer curr-inputs) 5) 0) (not (equal? (inputs-timer curr-inputs) 0)))
       (inputs-text-area curr-inputs)]
      [else #f])))

;; output is series of save events
;; no need for an initial value
(define (save-draft input-list)
  (filter list?
  (for/list ([ip input-list])
    (let ([output-event (handle-inputs ip)])
      (if (string? output-event)
          (list (first ip) (save-event output-event))
          #f)))))

(define (checker inputs outputs)
  (let ([program-output (save-draft inputs)])
    (equal? program-output outputs)))

(checker sample-inputs expected-outputs)