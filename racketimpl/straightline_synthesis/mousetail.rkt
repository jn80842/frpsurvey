#lang rosette
(require rosette/lib/synthax)

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../straightline.rkt")
(require "../benchmarks/mousetail.rkt")

(define (straightline-mousetail-y-graph y-stream)
  (define r1 y-stream)
  (define r2 ((curry delayE time-delay) r1))
  r2)

(define (straightline-mousetail-x-graph x-stream)
  (define r1 x-stream)
  (define r2 (delayE time-delay r1))
  (define r3 (mapE (λ (e) (+ e x-offset)) r2))
  r3)

(define (depth-2-sketch-graph stream1)
  (define r1 stream1)
  (define r2 ((choose identityE
                      oneE
                      zeroE
                      (curry mapE (λ (n) (+ n (??))))
                      (curry mergeE r1)
                      (curry filterE (λ (n) (eq? n (??))))
                      (curry constantE (??))
                      (curry collectE (??) + )
                      notE
                      (curry delayE 3)
                      (curry blindE (??))
                      (curry startsWith (??))) r1))
  r2)

(define (depth-3-sketch-graph stream1)
  (define r1 stream1)
  (define r2 ((choose identityE
                      oneE
                      zeroE
                      (curry mapE (λ (n) (+ n (??))))
                      (curry mergeE r1)
                      (curry filterE (λ (n) (eq? n (??))))
                      (curry constantE (??))
                      (curry collectE (??) + )
                      notE
                      (curry delayE 3)
                      (curry blindE (??))
                      (curry startsWith (??))) r1))
  (define r3 ((choose identityE
                      oneE
                      zeroE
                      (curry mapE (λ (n) (+ n (??))))
                      (curry mergeE (list-ref (list r1 r2) (choose 0 1)))
                      (curry filterE (λ (n) (eq? n (??))))
                      (curry constantE (??))
                      (curry collectE (??) + )
                      notE
                      (curry delayE 3)
                      (curry blindE (??))
                      (curry startsWith (??))) (list-ref (list r1 r2) (choose 0 1))))
  r3)
  
(assert (mousetail-assumptions s-mouse-x s-mouse-y))

(define binding
  (time (synthesize #:forall (append (harvest s-mouse-x) (harvest s-mouse-y))
                    #:guarantee (begin
                                  (assert (same mousetail-y-graph depth-2-sketch-graph s-mouse-y))
                                  (assert (same mousetail-x-graph depth-3-sketch-graph s-mouse-x))
                                  ))))


(print-forms binding)
;(function-printer binding)