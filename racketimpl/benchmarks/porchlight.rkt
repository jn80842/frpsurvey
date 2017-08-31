#lang rosette/safe

(require "../fjmodels.rkt")
(require "../rosettefjapi.rkt")

(provide (all-defined-out))

;;;;; motion detector and porch light
(define delay-by 5)
(define calm-by 5)

(current-bitwidth 6)

(define stream-length 2)

(define (light-assumptions motion)
  (and (valid-timestamps? motion)
       ;; guard against overflow
       (andmap (λ (t) (> (max-for-current-bitwidth (current-bitwidth))
                         (+ delay-by t))) (map get-timestamp motion))
  ))