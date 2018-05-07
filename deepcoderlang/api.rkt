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
(define (filter-dc pred xs)
  (letrec ([f (λ (xs)
                (cond [(not (list? xs)) '()]
                      [(empty? xs) '()]
                      [(pred (first xs)) (append (list (first xs)) (f (cdr xs)))]
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

(define-symbolic expt-uf (~> integer? integer?))

(define (get-lookup-expt magnitude)
  (displayln (format "Synthesizing lookup expt 2 with range -~a-~a" magnitude magnitude))
  (clear-asserts!)
  (define-symbolic i integer?)
  (assert (<= i magnitude))
  (assert (>= i 0))
  (define b (time (synthesize #:forall (list i)
                              #:guarantee (assert (eq? (expt i 2) (expt-uf i))))))
  (clear-asserts!)
  (let ([e2 (evaluate expt-uf b)])
    (λ (x)
      (assert (<= (abs x) magnitude))
      (assert (>= (abs x) 0))
      (e2 (abs x)))))

(define lut-expt (get-lookup-expt 10))

(define-symbolic div2-uf (~> integer? integer?))

(define (signed-div-uf uf)
  (λ (x)
    (if (negative? x)
        (- (uf x))
        (uf x))))

(define (get-lookup-div2 magnitude)
  (displayln (format "Synthesizing lookup div2 with range -~a-~a" magnitude magnitude))
  (clear-asserts!)
  (define-symbolic i integer?)
  (assert (<= (abs i) magnitude))
  (assert (>= (abs i) 0))
  (define b (let ([signed-div2 (signed-div-uf div2-uf)])
              (time (synthesize #:forall (list i)
                                #:guarantee (assert (eq? (quotient i 2) (signed-div2 i)))))))
  (clear-asserts!)
  (let ([q2 (signed-div-uf (evaluate div2-uf b))])
    (λ (x)
      (assert (<= (abs x) magnitude))
      (assert (>= (abs x) 0))
      (q2 x))))

(define lut-div2 (get-lookup-div2 10))

(define-symbolic div3-uf (~> integer? integer?))

(define (get-lookup-div3 magnitude)
  (displayln (format "Synthesizing lookup div3 with range -~a-~a" magnitude magnitude))
  (clear-asserts!)
  (define-symbolic i integer?)
  (assert (<= (abs i) magnitude))
  (assert (>= (abs i) 0))
  (define b (let ([signed-div3 (signed-div-uf div3-uf)])
              (time (synthesize #:forall (list i)
                                #:guarantee (assert (eq? (quotient i 3) (signed-div3 i)))))))
  (clear-asserts!)
  (let ([q3 (signed-div-uf (evaluate div3-uf b))])
    (λ (x)
      (assert (<= (abs x) magnitude))
      (assert (>= (abs x) 0))
      (q3 x))))

(define lut-div3 (get-lookup-div3 10))

(define-symbolic div4-uf (~> integer? integer?))

(define (get-lookup-div4 magnitude)
  (displayln (format "Synthesizing lookup div4 with range -~a-~a" magnitude magnitude))
  (clear-asserts!)
  (define-symbolic i integer?)
  (assert (<= (abs i) magnitude))
  (assert (>= (abs i) 0))
  (define b (let ([signed-div4 (signed-div-uf div4-uf)])
              (time (synthesize #:forall (list i)
                                #:guarantee (assert (eq? (quotient i 4) (signed-div4 i)))))))
  (clear-asserts!)
  (let ([q4 (signed-div-uf (evaluate div4-uf b))])
    (λ (x)
      (assert (<= (abs x) magnitude))
      (assert (>= (abs x) 0))
      (q4 x))))

(define lut-div4 (get-lookup-div4 10))

(define int-to-int-funcs (list (λ (i) (+ i 1))
                               (λ (i) (- i 1))
                               (λ (i) (+ i i))
                               (λ (i) (+ i i i))
                               (λ (i) (+ i i i i))
                               lut-div2
                               lut-expt ;; note: (map expt2 input) is equiv to (zipwith * input input)
                               lut-div3
                               lut-div4
                               ))
(define int-to-int-funcs-string (list "(λ (i) (+ i 1))"
                                      "(λ (i) (- i 1))"
                                      "(λ (i) (* i 2))"
                                      "(λ (i) (* i 3))"
                                      "(λ (i) (* i 4))"
                                      "(λ (i) (/ i 2))"
                                      "(λ (i) (expt i 2))"
                                      "(λ (i) (/ i 3))"
                                      "(λ (i) (/ i 4))"
                                      ))

(define-symbolic odd-lut? (~> integer? boolean?))

(define (get-lookup-odd magnitude)
  (displayln (format "Synthesizing lookup odd? with range -~a-~a" magnitude magnitude))
  (clear-asserts!)
  (define-symbolic i integer?)
  (assert (<= (abs i) magnitude))
  (define b (time (synthesize #:forall (list i)
                              #:guarantee (assert (eq? (odd? i) (odd-lut? (abs i)))))))
  (clear-asserts!)
  (let ([o? (evaluate odd-lut? b)])
    (λ (x)
      (assert (<= (abs x) magnitude))
      (assert (>= (abs x) 0))
      (o? (abs x)))))

(define lut-odd10? (get-lookup-odd 10))

(define int-to-bool-funcs (list (λ (x) (not (lut-odd10? x))); even?
                                lut-odd10? ; odd?
                                positive?
                                negative?
                                ))
(define int-to-bool-funcs-string (list "even?"
                                       "odd?"
                                       "positive?"
                                       "negative?"
                                       ))

(define-symbolic times (~> integer? integer? integer?))

(define (signed-times uf)
  (λ (x y)
  (if (and (negative? x) (negative? y))
      (uf (abs x) (abs y))
        (if (and (negative? x) (positive? y))
            (- (uf (abs x) (abs y)))
        (if (and (positive? x) (negative? y))
            (- (uf (abs x) (abs y)))
            (uf x y))))))

(define (multiply-function times-f)
  (λ (x y)
    (let ([signed-uf (signed-times times-f)])
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
            [(<= x y) (signed-uf x y)]
            [else (signed-uf y x)]))))

(define (get-lookup-mult magnitude)
  (displayln (format "Synthesizing lookup multiplication with range ~a-~a" (- magnitude) magnitude ))
  (define-symbolic i1 integer?)
  (define-symbolic i2 integer?)
  (clear-asserts!)
  (assert (<= (abs i1) magnitude))
  (assert (<= (abs i2) magnitude))
  (define b (time (synthesize #:forall (list i1 i2)
                              #:guarantee (assert (= ((multiply-function times) i1 i2) (* i1 i2))))))
  (clear-asserts!)
  (let ([m (multiply-function (evaluate times b))])
    (λ (x y)
      (begin (assert (<= (abs x) magnitude))
             (assert (<= (abs y) magnitude))
             (m x y)))))

(define lut-mult10 (get-lookup-mult 10))

(define int-to-int-to-int-funcs (list +
                                      -
                                      lut-mult10
                                      min
                                      max
                                      ))
(define int-to-int-to-int-funcs-string (list "+"
                                             "-"
                                             "*"
                                             "min"
                                             "max"
                                             ))