#lang s-exp rosette/safe

(require "../rosettefjapi.rkt")

;;(current-bitwidth 16)

(define (clicksE concrete-list)
 ;(λ ()
    (map (λ (c)
           
         (define-symbolic* timestamp integer?)
         (define-symbolic* click-evt boolean?)
         (define click-union (if click-evt 'click 'no-evt))
         (list timestamp 'click)) concrete-list));)

(define (inc-dec-button-graph inc dec)
  (startsWith
   (collectE
   (mergeE (constantE inc 1) (constantE dec -1)) 0 +) 0))

(define concrete-inc-clicks (λ () (list (list 1 'click) (list 4 'click))))
(define concrete-dec-clicks (λ () (list (list 2 'no-evt) (list 3 'no-evt) (list 5 'click))))

(displayln ((inc-dec-button-graph concrete-inc-clicks concrete-dec-clicks)))

(define s-inc (clicksE (list 1 2 3 4 5)))
(define s-dec (clicksE (list 1 2 3 4 5)))


(define verified (verify
                  ;; assume that all inputs are valid
                  #:assume (assert (and
                                    ;; all timestamps are distinct
                                    ;;(apply distinct? (map get-timestamp (append s-dec s-inc)))
                                    (apply distinct? (map get-timestamp s-inc))
                                    (apply distinct? (map get-timestamp s-dec))
                                    ;; all timestamps are integers
                                    (andmap integer? (map get-timestamp (append s-dec s-inc)))
                                    ;; all timestamps are positive numbers
                                    (andmap positive? (map get-timestamp (append s-dec s-inc)))
                                    ;; lists of timestamps are sorted
                                    (equal? (map get-timestamp s-inc) (sort (map get-timestamp s-inc)))
                                    (equal? (map get-timestamp s-dec) (sort (map get-timestamp s-dec)))
                                    ;; input values are either 'click or 'no-evt
                                    (andmap (λ (e) (or (equal? 'no-evt (get-value e)) (equal? 'click (get-value e)))) (append s-inc s-dec))
                                    ))
                  #:guarantee (let ([output-evt-stream ((inc-dec-button-graph (λ () s-inc) (λ () s-dec)))])
                                (assert (and
                                         ;; first value at special timestamp 0 is 0
                                         (eq? (get-value (first output-evt-stream)) 0)
                                         (eq? (get-timestamp (first output-evt-stream)) 0)
                                         ;; all values are integers (i.e. all values are defined
                                         (andmap integer? (map get-value output-evt-stream))
                                         ;; the final value should be the number of inc button clicks - number of dec button clicks
                                         (eq? (get-value (last output-evt-stream)) (- (length (filter (λ (e) (equal? 'click (get-value e))) s-inc))
                                                                                      (length (filter (λ (e) (equal? 'click (get-value e))) s-dec))))
                                         )))
                                ))