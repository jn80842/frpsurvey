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

(define (get-lookup-expt magnitude)
  (let ([expt-table (map (λ (x) (expt x 2)) (range (add1 magnitude)))])
    (λ (i) (list-ref expt-table (abs i)))))

(define lut-expt (get-lookup-expt 10))

(define (get-lookup-div2 magnitude)
  (λ (x) (list-ref (map (λ (i) (quotient i 2)) (range (- magnitude) (add1 magnitude))) (+ x magnitude))))

(define lut-div2 (λ (i) (quotient i 2)));(get-lookup-div2 10))

(define (get-lookup-div3 magnitude)
  (let ([div4table (map (λ (i) (quotient i 3)) (range (- magnitude (add1 magnitude))))])
    (λ (x) (list-ref div4table (+ x magnitude)))))

(define lut-div3 (get-lookup-div3 10))

(define (get-lookup-div4 magnitude)
  (let ([div4table (map (λ (i) (quotient i 4)) (range (- magnitude (add1 magnitude))))])
    (λ (x) (list-ref div4table (+ x magnitude)))))

(define lut-div4 (get-lookup-div4 10))

(define int-to-int-funcs (list (λ (i) (+ i 1))
                               (λ (i) (- i 1))
                               (λ (i) (+ i i))
                               (λ (i) (+ i i i))
                               (λ (i) (+ i i i i))
                               lut-div2
                               (λ (i) (* i i)); lut-expt ;; note: (map expt2 input) is equiv to (zipwith * input input)
                               (λ (i) (/ i 3));lut-div3
                               (λ (i) (/ i 4));lut-div4
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

(define (get-lookup-odd magnitude)
  (let ([odd-table (map (λ (i) (odd? i)) (range (- magnitude) (add1 magnitude)))])
  (λ (x) (list-ref odd-table (+ x magnitude)))))

(define lut-odd10? (get-lookup-odd 10))

(define int-to-bool-funcs (list even?;(λ (x) (not (lut-odd10? x))); even?
                                odd?; lut-odd10? ; odd?
                                positive?
                                negative?
                                ))
(define int-to-bool-funcs-string (list "even?"
                                       "odd?"
                                       "positive?"
                                       "negative?"
                                       ))

(define (get-lookup-pos-mult magnitude)
  (let ([lut (for/list ([i (range (add1 magnitude))])
               (for/list ([j (range (add1 magnitude))])
                          (* i j)))])
    (λ (x y) (list-ref (list-ref lut x) y))))

(define (signed-times uf)
  (λ (x y)
  (if (and (negative? x) (negative? y))
      (uf (abs x) (abs y))
        (if (and (negative? x) (positive? y))
            (- (uf (abs x) (abs y)))
        (if (and (positive? x) (negative? y))
            (- (uf (abs x) (abs y)))
            (uf x y))))))

(define (get-lookup-mult magnitude)
  (signed-times (get-lookup-pos-mult magnitude)))

(define lut-mult10 (get-lookup-mult 10))

(define int-to-int-to-int-funcs (list +
                                      -
                                      *;lut-mult10
                                      min
                                      max
                                      ))
(define int-to-int-to-int-funcs-string (list "+"
                                             "-"
                                             "*"
                                             "min"
                                             "max"
                                             ))
