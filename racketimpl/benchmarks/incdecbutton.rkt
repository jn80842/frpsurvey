#lang s-exp rosette/safe

(require "../rosettefjapi.rkt")

;;(current-bitwidth 16)

(define (clicksE concrete-list)
 ;(λ ()
    (map (λ (c)
           
         (define-symbolic* timestamp integer?)
         (define-symbolic* click-evt boolean?)
         (define click-union (if click-evt 'click 'no-evt))
         (list timestamp click-union)) concrete-list));)

(define (inc-dec-button-graph inc dec)
  (startsWith
   (collectE 
   (mergeE (constantE inc 1) (constantE dec -1)) 0 +) 0))

(define (small-graph inc dec)
  (collectE (mergeE inc dec) 0 +))

(define concrete-inc-clicks (λ () (list (list 1 'click) (list 4 'click))))
(define concrete-dec-clicks (λ () (list (list 2 'no-evt) (list 3 'no-evt) (list 5 'click))))

(displayln ((inc-dec-button-graph concrete-inc-clicks concrete-dec-clicks)))

(define s-inc (clicksE (list 1 2 3 4)))
(define s-dec (clicksE (list 1 2 3 4)))

(define (concrete-eval inc dec)
  ((inc-dec-button-graph (λ () inc) (λ () dec))))

(define (button-assumptions inc-stream dec-stream)
    (and (valid-input-timestamps? inc-stream)
         (valid-input-timestamps? dec-stream)
         (apply distinct? (map get-timestamp (append inc-stream dec-stream)))
         (andmap (λ (e) (or (equal? 'no-evt (get-value e)) (equal? 'click (get-value e)))) (append inc-stream dec-stream))))

(define (button-guarantees output-stream)
    (and (valid-output-timestamps? output-stream)
         (equal? (get-value (first output-stream)) 0)
         (equal? (get-timestamp (first output-stream)) 0)
         (andmap integer? (map get-value output-stream))
         (equal? (get-value (last output-stream)) (- (length (filter (λ (e) (equal? 'click (get-value e))) s-inc))
                                                                                      (length (filter (λ (e) (equal? 'click (get-value e))) s-dec))))
         ))

(define verified (verify
                  ;; assume that all inputs are valid
                  #:assume (assert (button-assumptions s-inc s-dec))
                  #:guarantee (assert (button-guarantees ((inc-dec-button-graph (λ () s-inc) (λ () s-dec)))))
                  ))

#;(define verified2 (verify
                   #:assume (begin ;;(assert
                           ;; (assert (apply distinct? (map get-timestamp (append s-dec s-inc))))
                           ;;(assert (apply distinct? (map get-timestamp s-inc)))
                           ;;(assert (apply distinct? (map get-timestamp s-dec)))
                           ;;   (assert (andmap (λ (e) (not (member (get-timestamp e) (map get-timestamp s-dec)))) s-inc))
                            (assert (andmap positive? (map get-timestamp s-inc)))
                            (assert (andmap positive? (map get-timestamp s-dec)))
                          ;; (assert (equal? (map get-timestamp s-inc) (sort (map get-timestamp s-inc) <)))
                          ;; (assert (equal? (map get-timestamp s-dec) (sort (map get-timestamp s-dec) <)))
                                 ;;  (assert (apply distinct? (map get-timestamp s-dec))))
                            )
                   #:guarantee ;;(assert (or (equal? 1 (get-value (first ((small-graph (λ () s-inc) (λ () s-dec))))))
                                 ;;      (equal? -1 (get-value (first ((small-graph (λ () s-inc) (λ () s-dec))))))))))
                   (assert (equal? (get-value (first ((inc-dec-button-graph (λ () s-inc) (λ () s-dec))))) 0))))


(define-symbolic x integer?)
(define v (verify
           #:assume (assert (equal? x 2))
           #:guarantee (assert (equal? 0 (/ x 0)))))