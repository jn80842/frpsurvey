#lang rosette

(provide (all-defined-out))

(current-bitwidth #f)

;; rn null is 'null symbol
;; but could be racket null which is '()

(require (only-in racket [integer? concrete-integer?]))

(define (head-dc xs)
  (if (empty? xs)
      'null
      (first xs)))

(define (last-dc xs)
  (if (empty? xs)
      'null
      (last xs)))

(define (take-dc n xs)
  (if (negative? n)
      '()
      (if (> n (length xs))
          xs
          (take xs n))))

(define (drop-dc n xs)
  (if (negative? n)
      xs
      (if (>= n (length xs))
          '()
          (list-tail xs n))))

(define (access-dc n xs)
  (if (|| (negative? n) (>= n (length xs)))
      'null
      (list-ref xs n)))

(define (minimum-dc xs)
  (if (empty? xs)
      'null
      (apply min xs)))

(define (maximum-dc xs)
  (if (empty? xs)
      'null
      (apply max xs)))

(define (reverse-dc xs)
  (reverse xs))

(define (sort-dc xs)
  (sort xs <))

(define (sum-dc xs)
  (list (foldl + 0 xs)))

(define (map-dc f xs)
  (map f xs))

#;(define (filter-dc f xs)
  (filter f xs))
(define (filter-dc f xs)
  (letrec ([f (λ (xs)
                (cond [(not (list? xs)) '()]
                      [(empty? xs) '()]
                      [(f (first xs)) (append (list (first xs)) (f (cdr xs)))]
                      [else (f (cdr xs))]))])
        (f xs)))

(define (count-dc f xs)
  (count f xs))

(define (zipwith-dc f xs1 xs2)
  (let ([xs-min (min (length xs1) (length xs2))])
    (map f (take xs1 xs-min) (take xs2 xs-min))))

(define (scanl1-dc f xs)
  (if (empty? xs)
      '()
      (for/list ([i (range (length xs))])
        (foldl f (first xs) (take (drop xs 1) i)))))

(define int-to-int-funcs (list (λ (i) (+ i 1))
                               (λ (i) (- i 1))
                               (λ (i) (+ i i))
                               (λ (i) (+ i i i))
                               (λ (i) (+ i i i i))
                              ; (λ (i) (* i 2))
                              ; (λ (i) (quotient i 2))
                              ; (λ (i) (expt i 2))
                              ; (λ (i) (* i 3))
                              ; (λ (i) (quotient i 3))
                              ; (λ (i) (* i 4))
                              ; (λ (i) (quotient i 4))
                               ))
(define int-to-int-funcs-string (list "(λ (i) (+ i 1))"
                                      "(λ (i) (- i 1))"
                                      "(λ (i) (* i 2))"
                                      "(λ (i) (* i 3))"
                                      "(λ (i) (* i 4))"
                                     ; "(λ (i) (* i 2))"
                                     ; "(λ (i) (/ i 2))"
                                     ; "(λ (i) (expt i 2))"
                                     ; "(λ (i) (* i 3))"
                                     ; "(λ (i) (/ i 3))"
                                     ; "(λ (i) (* i 4))"
                                     ; "(λ (i) (/ i 4))"
                                      ))
(define int-to-bool-funcs (list ; even?
                                ; odd?
                                positive?
                                negative?
                                ))
(define int-to-bool-funcs-string (list ;"even?"
                                       ;"odd?"
                                       "positive?"
                                       "negative?"
                                       ))

(define-symbolic times (~> integer? integer? integer?))

(define (multiply x y)
  (cond [(and (concrete-integer? x) (concrete-integer? y)) (* x y)]
        [(= x 0) 0]
        [(= y 0) 0]
        [(= x 1) y]
        [(= y 1) x]
        [(= x -1) (- y)]
        [(= y -1) (- x)]
        [(= x 2) (+ y y)]
        [(= y 2) (+ x x)]
        [(= x 3) (+ y y y)]
        [(= y 3) (+ x x x)]
        [(= x -2) (- (+ y y))]
        [(= y -2) (- (+ x x))]
        [(= x -3) (- (+ y y y))]
        [(= y -3) (- (+ x x x))]
        [(<= x y) (times x y)]
        [else (times y x)]))

(define (get-lookup-mult magnitude)
  (displayln (format "Synthesizing lookup multiplication with range ~a-~a" (- magnitude) magnitude ))
  (define-symbolic i1 integer?)
  (define-symbolic i2 integer?)
  (clear-asserts!)
  (assert (<= i1 magnitude))
  (assert (<= i2 magnitude))
  (assert (>= i1 (- magnitude)))
  (assert (>= i2 (- magnitude)))
  (define b (time (synthesize #:forall (list i1 i2)
                              #:guarantee (assert (= (multiply i1 i2) (* i1 i2))))))
  (clear-asserts!)
  (let ([m (evaluate times b)])
    (λ (x y)
      (begin   (assert (<= x magnitude))
               (assert (<= y magnitude))
               (assert (>= x (- magnitude)))
               (assert (>= y (- magnitude)))
               (cond [(and (concrete-integer? x) (concrete-integer? y)) (* x y)]
                     [(= x 0) 0]
                     [(= y 0) 0]
                     [(= x 1) y]
                     [(= y 1) x]
                     [(= x -1) (- y)]
                     [(= y -1) (- x)]
                     [(= x 2) (+ y y)]
                     [(= y 2) (+ x x)]
                     [(= x 3) (+ y y y)]
                     [(= y 3) (+ x x x)]
                     [(= x -2) (- (+ y y))]
                     [(= y -2) (- (+ x x))]
                     [(= x -3) (- (+ y y y))]
                     [(= y -3) (- (+ x x x))]
                     [(<= x y) (m x y)]
                     [else (m y x)])))))

(define lut-mult6 (get-lookup-mult 6))

(define int-to-int-to-int-funcs (list +
                                      -
                                      lut-mult6 ;  *
                                      min
                                      max
                                      ))
(define int-to-int-to-int-funcs-string (list "+"
                                             "-"
                                             "*"
                                             "min"
                                             "max"
                                             ))