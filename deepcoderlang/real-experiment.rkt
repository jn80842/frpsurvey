#lang rosette

(current-bitwidth #f)

(define-symbolic* real-idx real?)
(define-symbolic* int-idx integer?)

(define-symbolic* r1 real?)
(define-symbolic* r2 real?)
(define-symbolic* r3 real?)

(define rlist (list r1 r2 r3))

(define functions-list (list (λ (i) (+ i 1))
                             (λ (i) (- i 1))
                             (λ (i) (* i i))
                             (λ (i) (* i 3))
                             ))

(define sk-phi (map (list-ref functions-list int-idx) rlist))
;(define f-phi (map (λ (i) (+ i 1)) rlist))
(define f-phi (map (λ (i) (* i i)) rlist))

(time (synthesize #:forall rlist
                    #:guarantee (assert (equal? sk-phi f-phi))))
