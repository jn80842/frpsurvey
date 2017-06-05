#lang rosette/safe

(require "../rosettefjapi.rkt")

(define (clicksE concrete-list)
 ;(λ ()
    (map (λ (c)
         (define-symbolic* timestamp integer?)
         (list timestamp 'click)) concrete-list));)

(define (inc-dec-button-graph inc dec)
  (startsWith
   (collectE
   (mergeE (constantE inc 1) (constantE dec -1)) 0 +) 0))

(define concrete-inc-clicks (λ () (list (list 1 'click) (list 4 'click))))
(define concrete-dec-clicks (λ () (list (list 2 'click) (list 3 'click) (list 5 'click))))

(displayln ((inc-dec-button-graph concrete-inc-clicks concrete-dec-clicks)))

(define s-inc (clicksE (list 1)))
(define s-dec (clicksE (list 1)))

(define button-sol (solve
                    (begin
                      (assert (equal? (second (last ((inc-dec-button-graph (λ () s-inc) (λ () s-dec))))) 0))
                      ;; each sequence of events should have distinct timestamps
                      (assert (apply distinct? (map (λ (e) (first e)) s-inc)))
                      (assert (apply distinct? (map (λ (e) (first e)) s-dec)))
                      ;; all timestamps should be positive
                      (assert (andmap positive? (map (λ (e) (first e)) (append s-inc s-dec)))))))
