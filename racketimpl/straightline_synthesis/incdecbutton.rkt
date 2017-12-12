#lang rosette
(require rosette/lib/synthax)

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../straightline.rkt")
(require "../benchmarks/incdecbutton.rkt")

(define (straightline-graph inc dec)
  (define r1 inc)
  (define r2 dec)
  (define r3 (constantE 1 r1))
  (define r4 (constantE -1 r2))
  (define r5 (mergeE r3 r4))
  (define r6 (collectE 0 + r5))
  (define r7 (startsWith 0 r6))
  r7)

(define (fully-expanded-sketch-graph inc dec)
  (define r1 inc)
  (define r2 dec)
  (define r3 ((choose identityE
                      oneE
                      zeroE
                      (curry mapE (λ (n) (+ n 1)))
                      (curry mergeE (list-ref (list r1 r2) (choose 0 1)))
                      (curry filterE (λ (n) (eq? n (??))))
                      (curry constantE (??))
                      (curry collectE (??) + )
                      notE
                      (curry delayE (??))
                      (curry blindE (??))
                     ;; (curry calmE (??))
                      (curry startsWith (??))) (list-ref (list r1 r2) (choose 0 1))))
  (define r4 ((choose identityE
                      oneE
                      zeroE
                      (curry mapE (λ (n) (+ n 1)))
                      (curry mergeE (list-ref (list r1 r2 r3) (choose 0 1 2)))
                      (curry filterE (λ (n) (eq? n (??))))
                      (curry constantE (??))
                      (curry collectE (??) + )
                      notE
                      (curry delayE (??))
                      (curry startsWith (??))) (list-ref (list r1 r2 r3) (choose 0 1 2))))
  (define r5 ((choose identityE
                      oneE
                      zeroE
                      (curry mapE (λ (n) (+ n 1)))
                      (curry mergeE (list-ref (list r1 r2 r3 r4) (choose 0 1 2 3)))
                      (curry filterE (λ (n) (eq? n (??))))
                      (curry constantE (??))
                      (curry collectE (??) + )
                      notE
                      (curry delayE (??))
                      (curry startsWith (??))) (list-ref (list r1 r2 r3 r4) (choose 0 1 2 3))))
  (define r6 ((choose identityE
                      oneE
                      zeroE
                      (curry mapE (λ (n) (+ n 1)))
                      (curry mergeE (list-ref (list r1 r2 r3 r4 r5) (choose 0 1 2 3 4)))
                      (curry filterE (λ (n) (eq? n (??))))
                      (curry constantE (??))
                      (curry collectE (??) + )
                      notE
                      (curry delayE (??))
                      (curry startsWith (??))) (list-ref (list r1 r2 r3 r4 r5) (choose 0 1 2 3 4))))
  (define r7 ((choose identityE
                      oneE
                      zeroE
                      (curry mapE (λ (n) (+ n 1)))
                      (curry mergeE (list-ref (list r1 r2 r3 r4 r5 r6) (choose 0 1 2 3 4 5)))
                      (curry filterE (λ (n) (eq? n (??))))
                      (curry constantE (??))
                      (curry collectE (??) + )
                      notE
                      (curry delayE (??))
                      (curry startsWith (??))) (list-ref (list r1 r2 r3 r4 r5 r6) (choose 0 1 2 3 4 5))))
  r7)


(define binding
  (time (synthesize #:forall (append (harvest s-inc) (harvest s-dec))
                    #:guarantee (assert (same inc-dec-button-graph
                                              fully-expanded-sketch-graph
                                              s-inc s-dec)))))

;;(print-forms binding)
(function-printer binding)