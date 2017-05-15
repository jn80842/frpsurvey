#lang racket

;; inputs inc button, dec button; output display elt

(define (fib k n)
  (if (< n 2)
      (k 1)
      (fib (λ (fib-of-n-1)
             (fib (λ (fib-of-n-2)
                    (k (+ fib-of-n-1 fib-of-n-2))) (- n 2))) (- n 1))))