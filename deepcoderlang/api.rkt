#lang rosette

(provide (all-defined-out))

;; rn null is 'null symbol
;; but could be racket null which is '()


(define (head-dc xs)
  (if (empty? xs)
      'null
      (first xs)))

(define (last-dc xs)
  (if (empty? xs)
      'null
      (last xs)))

(define (take-dc n xs)
  (if (> n (length xs))
      xs
      (take xs n)))

(define (drop-dc n xs)
  (if (>= n (length xs))
      '()
      (drop xs n)))

(define (access-dc n xs)
  (if (> n (length xs))
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
  (foldl + 0 xs))

(define (map-dc f xs)
  (map f xs))

(define (filter-dc f xs)
  (filter f xs))

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