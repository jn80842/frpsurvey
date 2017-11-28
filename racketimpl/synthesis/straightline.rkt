#lang rosette
(require rosette/lib/angelic)
(require rosette/lib/synthax)

;;(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
;;(require "../benchmarks/incdecbutton.rkt")
;;(require "grammar.rkt")

(provide (all-defined-out))

#;(define (inc-dec-button-graph inc dec)
  (startsWith 0
   (collectE 0 +
   (mergeE (constantE 1 inc) (constantE -1 dec)))))

#;(define (straightline-graph inc dec)
  (define r1 inc)
  (define r2 dec)
  (define r3 (constantE 1 r1))
  (define r4 (constantE -1 r2))
  (define r5 (mergeE r3 r4))
  (define r6 (collectE 0 + r5))
  (define r7 (startsWith 0 r6))
  r7)

#;(define-syntax-rule (operator-choose regs)
  ((choose* (curry constantE (choose* -1 0 1 2 3 4))
                      (curry mergeE (list-ref regs (choose* 0 1 2 3 4)))
                      (curry collectE (choose* -1 0 1 2 3 4) + )
                      (curry startsWith (choose* -1 0 1 2 3 4))) (list-ref regs (choose* 0 1 2 3 4))))

(define (synth-insn regs)
  ((choose* (curry constantE (choose* -1 0 1 2 3 4))
            (curry mergeE (list-ref regs (choose* 0 1 2 3 4)))
            (curry collectE (choose* -1 0 1 2 3 4) +)
            (curry startsWith (choose* -1 0 1 2 3 4)))
   (list-ref regs (choose* 0 1 2 3 4))))

#;(define (one-arg-2-depth-graph arg1)
  (define r1 arg1)
  (define r2 ((choose (curry collectE (??) +)
                      (curry constantE (??))
                      (curry delayE (??))
                      (curry mapE (λ (e) (+ e (??))))
                      (curry startsWith (??))) r1))
  r2)

(define (one-arg-2-depth-graph arg1)
  (define r1 arg1)
  (define r2 (delayE (??) r1))
  r2)

(define (one-arg-3-depth-graph arg1)
  (define r1 arg1)
  (define r2 ((choose (curry collectE (??) +)
                      (curry constantE (??))
                      (curry delayE (??))
                      (curry mapE (λ (e) (+ e (??))))
                      (curry startsWith (??))) r1))
  (define r3 ((choose (curry collectE (??) +)
                      (curry constantE (??))
                      (curry delayE (??))
                      (curry mapE (λ (e) (+ e (??))))
                      (curry startsWith (??))) (list-ref (list r1 r2) (choose 0 1))))
  r3)

(define (fully-expanded-sketch-graph inc dec)
  (define r1 inc)
  (define r2 dec)
  (define r3 ((choose identityE
                      (curry constantE (??))
                      (curry delayE (??))
                      (curry mapE (λ (e) (+ e (??))))
                      (curry mergeE (list-ref (list r1 r2) (choose 0 1)))
                      notE
                      (curry collectE (??) + )
                      (curry startsWith (??))) (list-ref (list r1 r2) (choose 0 1))))
  (define r4 ((choose identityE
                      (curry constantE (??))
                      (curry delayE (??))
                      (curry mapE (λ (e) (+ e (??))))
                      (curry mergeE (list-ref (list r1 r2 r3) (choose 0 1 2)))
                      notE
                      (curry collectE (??) + )
                      (curry startsWith (??))) (list-ref (list r1 r2 r3) (choose 0 1 2))))
  (define r5 ((choose identityE
                      (curry constantE (??))
                      (curry delayE (??))
                      (curry mapE (λ (e) (+ e (??))))
                      (curry mergeE (list-ref (list r1 r2 r3 r4) (choose 0 1 2 3)))
                      notE
                      (curry collectE (??) + )
                      (curry startsWith (??))) (list-ref (list r1 r2 r3 r4) (choose 0 1 2 3))))
  (define r6 ((choose identityE
                      (curry constantE (??))
                      (curry delayE (??))
                      (curry mapE (λ (e) (+ e (??))))
                      (curry mergeE (list-ref (list r1 r2 r3 r4 r5) (choose 0 1 2 3 4)))
                      notE
                      (curry collectE (??) + )
                      (curry startsWith (??))) (list-ref (list r1 r2 r3 r4 r5) (choose 0 1 2 3 4))))
  (define r7 ((choose identityE
                      (curry constantE (??))
                      (curry delayE (??))
                      (curry mapE (λ (e) (+ e (??))))
                      (curry mergeE (list-ref (list r1 r2 r3 r4 r5 r6) (choose 0 1 2 3 4 5)))
                      notE
                      (curry collectE (??) + )
                      (curry startsWith (??))) (list-ref (list r1 r2 r3 r4 r5 r6) (choose 0 1 2 3 4 5))))
  r7)

(define-syntax (macro-graph stx)
  (syntax-case stx ()
    ([_ n arg1 arg2]
     (with-syntax ([choose-insn #'((choose (curry constantE (??))
                       (curry mergeE (list-ref (list r1 r2) (choose 0 1)))
                       (curry collectE (??) + )
                       (curry startsWith (??))) (list-ref (list r1 r2) (choose 0 1)))])
       #'(define (sketch-graph arg1 arg2)
           (define r1 arg1)
           (define r2 arg2)
           (for ([i (in-range 0 n)])
             (define r3 choose-insn)))))))

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))
;; top level definitions of SSA variables
;; hack to make eval of rewrites work
(define r1 'r1)
(define r2 'r2)
(define r3 'r3)
(define r4 'r4)
(define r5 'r5)
(define r6 'r6)

(define (insn-printer insn-stx)
  (eval (syntax-case insn-stx ()
          [(define r1 ((curry op ARG) INPUT-STREAM))
          #'(format "  (define ~a (~a ~a ~a))" 'r1 'op ARG INPUT-STREAM)]
          [(define r1 ((curry collectE int-arg op-arg) INPUT-STREAM))
          #'(format "  (define ~a (collectE ~a ~a ~a))" 'r1 'int-arg 'op-arg INPUT-STREAM)]
          [(define r1 v1) #'(format "  (define ~a ~a)" 'r1 'v1)]
          [retval #'(format "  ~a)" 'retval)]
          [_ #'(format "no match")]) ns))

(define (function-printer binding)
  (let ([syntax-lst (syntax-e (list-ref (generate-forms binding) 0))])
    (begin
      (displayln (format "(~a ~a" (syntax->datum (list-ref syntax-lst 0))
                         (syntax->datum (list-ref syntax-lst 1))))
      (for ([s (list-tail syntax-lst 2)])
        (displayln (insn-printer s))))))

(define (sketch-graph inc dec)
  (define r1 inc)
  (define r2 dec)
  (define r3 (synth-insn (list r1 r2)))
  (define r4 ((choose (curry constantE (choose* -1 0 1 2 3 4))
            (curry mergeE (list-ref (list r1 r2 r3) (choose 0 1 2 3 4)))
            (curry collectE (choose -1 0 1 2 3 4) +)
            (curry startsWith (choose -1 0 1 2 3 4)))
   (list-ref (list r1 r2 r3) (choose 0 1 2 3 4))))
  (define r5 (mergeE r3 r4))
  (define r6 (collectE 0 + r5))
  (define r7 (startsWith 0 r6))
  r7)


;;(define verified (verify #:guarantee (same straightline-graph inc-dec-button-graph s-inc s-dec)))

#;(define binding
  (time (synthesize #:forall (append (harvest s-inc) (harvest s-dec))
                    #:guarantee (assert (same inc-dec-button-graph
                                              fully-expanded-sketch-graph
                                              s-inc s-dec)))))

;;(print-forms binding)
#;(function-printer binding)