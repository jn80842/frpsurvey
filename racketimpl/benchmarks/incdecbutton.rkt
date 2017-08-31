#lang rosette/safe

(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")

(provide (all-defined-out))

(current-bitwidth 5)
(define stream-length 3)

(define (button-assumptions inc-stream dec-stream)
    (and (valid-timestamps? inc-stream)
         (valid-timestamps? dec-stream)
         (timestamps-below-max? (* 2 stream-length) inc-stream)
         (timestamps-below-max? (* 2 stream-length) dec-stream)
         ;; TODO: figure out better solutions for simultaneous events
         (apply distinct? (map get-timestamp (append inc-stream dec-stream)))
         ))