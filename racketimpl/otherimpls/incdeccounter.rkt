#lang racket


(struct buttons (incbutton decbutton) #:transparent)
(struct number-elt (text) #:transparent)

(define sample-input
  (list (list 0 (buttons #f #f))
        (list 1 (buttons 'click #f))
        (list 2 (buttons #f #f))
        (list 3 (buttons 'click #f))
        (list 4 (buttons #f 'click))
        (list 5 (buttons #f 'click))
        (list 6 (buttons #f 'click))
        (list 7 (buttons #f #f))
        (list 8 (buttons #f #f))))

(define expected-output
  (list
   (list 0 (number-elt 0))
   (list 1 (number-elt 1))
   (list 2 (number-elt 1))
   (list 3 (number-elt 2))
   (list 4 (number-elt 1))
   (list 5 (number-elt 0))
   (list 6 (number-elt -1))
   (list 7 (number-elt -1))
   (list 8 (number-elt -1))))

(define (inc-dec-button-single input-pair current-state)
  (match (second input-pair)
    [(buttons 'click 'click) (error "can't click 2 buttons at the same time")]
    [(buttons 'click #f) (add1 current-state)]
    [(buttons #f 'click) (sub1 current-state)]
    [(buttons #f #f) current-state]))

(define (inc-dec-button input-pair-list init-val)
  (define new-state (number-elt init-val))
  (for/list ([ip input-pair-list])
    (set! new-state (number-elt (inc-dec-button-single ip (number-elt-text new-state))))
    (list (first ip) new-state)))

(define (checker input-list output-list)
  (let ([program-output (inc-dec-button input-list 0)])
    (equal? program-output output-list)))
    
(checker sample-input expected-output)
