#lang s-exp rosette/safe

(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")

(current-bitwidth 5)

(define (clicksE concrete-list)
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

(displayln (inc-dec-button-graph concrete-inc-clicks concrete-dec-clicks))

(define s-inc (clicksE (list 1 2 3 4)))
(define s-dec (clicksE (list 1 2 3 4)))

(printf "current bitwidth: ~a~n" (current-bitwidth))
(printf "length of increase clicks ~a~n" (length s-inc))
(printf "length of decrease clicks ~a~n" (length s-dec))

(define (concrete-eval inc dec)
  (inc-dec-button-graph (λ () inc) (λ () dec)))

(define (button-assumptions inc-stream dec-stream)
    (and (valid-timestamps? inc-stream)
         (valid-timestamps? dec-stream)
         ;; TODO: figure out better solutions for simultaneous events
         (apply distinct? (map get-timestamp (append inc-stream dec-stream)))
         ))

#;(define (button-guarantees output-stream)
    (and (valid-timestamps? output-stream)
         ;; output should start with special timestamp 0
         (equal? (get-value (first output-stream)) 0)
         ;; value at special timestamp 0 should also be 0
         (equal? (get-timestamp (first output-stream)) 0)
         ;; values at each timestamp should be integers (i.e. should never be undefined)
         (andmap (λ (n) (<= n (expt 2 (current-bitwidth)))) (map get-value output-stream))
         ;; last value is the difference between the number of increase clicks and the number of decrease clicks
         (equal? (get-value (last output-stream)) (- (length (filter (λ (e) (equal? 'click (get-value e))) s-inc))
                                                                                      (length (filter (λ (e) (equal? 'click (get-value e))) s-dec))))
         ))

(define (button-guarantees output-behavior)
  (and (valid-behavior? output-behavior)
       (equal? 0 (behavior-init output-behavior))
       (andmap integer? (map get-value (behavior-changes output-behavior)))
       (equal? (get-value (last (behavior-changes output-behavior))) (- (length (filter (λ (e) (equal? 'click (get-value e))) s-inc))
                                                                                      (length (filter (λ (e) (equal? 'click (get-value e))) s-dec))))))

(define begin-time (current-seconds))
(define verified (verify
                  #:assume (assert (button-assumptions s-inc s-dec))
                  #:guarantee (assert (button-guarantees (inc-dec-button-graph (λ () s-inc) (λ () s-dec))))
                  ))
(define end-time (current-seconds))
(printf "time to verify: ~a seconds~n" (- end-time begin-time))
(if (unsat? verified)
    (displayln "Spec is verified.")
    (displayln "Model that violates spec is found: increase stream ~a, decrease stream ~a~n" (evaluate s-inc verified) (evaluate s-dec verified)))