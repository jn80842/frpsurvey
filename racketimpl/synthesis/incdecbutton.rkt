#lang rosette/safe
(require rosette/lib/synthax)

(require "../fjmodels.rkt")
(require "../rosettefjapi.rkt")
(require "grammar.rkt")

(define (clicksE n)
  (let ([concrete-list (stream-size n)])
    (map (λ (c)
           (define-symbolic* timestamp integer?)
           (define-symbolic* click-evt boolean?)
           (define click-union (if click-evt 'click 'no-evt))
           (list timestamp click-union)) concrete-list)))

(define (inc-dec-button-graph inc dec)
 (startsWith (collectE 
   (mergeE (constantE inc 1) (constantE dec -1))
   0 +) 0))

(define (merge-constants-graph inc dec)
  (mergeE (constantE inc 1) (constantE dec -1)))
(define (synth-merge-constants-graph inc dec)
  (flapjax-grmr1 inc dec 2))

(define (collect-graph evt-stream)
  (startsWith (collectE evt-stream 0 +) 0))
(define (synth-collect-graph evt-stream)
  (flapjax-grmr evt-stream 3))

(define (outer-graph inc dec)
  (startsWith (collectE (mergeE (flapjax-grmr inc 2) (constantE dec -1)) 0 +) 0))

(define (synth-inc-dec-button-graph inc dec)
  (flapjax-grmr1 inc dec 4))

(define-symbolic* timestamp1 integer?)
(define-symbolic* bool1 boolean?)
(define-symbolic* timestamp2 integer?)
(define-symbolic* bool2 boolean?)
(define-symbolic* timestamp3 integer?)
(define-symbolic* bool3 boolean?)
(define-symbolic* timestamp4 integer?)
(define-symbolic* bool4 boolean?)
(define-symbolic* timestamp5 integer?)
(define-symbolic* bool5 boolean?)
(define-symbolic* timestamp6 integer?)
(define-symbolic* bool6 boolean?)

(assert (positive? timestamp1))
(assert (positive? timestamp2))
(assert (< timestamp1 timestamp2))
(assert (positive? timestamp3))
(assert (positive? timestamp4))
(assert (< timestamp3 timestamp4))
(assert (positive? timestamp5))
(assert (positive? timestamp6))

(displayln "Synthesize inc/dec button program:")

(displayln "Synthesize two merged streams with 1 and -1 constants")

(define begin-time (current-seconds))
(define binding
    (synthesize #:forall (list timestamp1 bool1
                               timestamp2 bool2
                              ; timestamp3 bool3
                               timestamp4 bool4
                               timestamp5 bool5
                              ; timestamp6 bool6
                               )
                #:guarantee (same2 merge-constants-graph synth-merge-constants-graph
                                  (list (list timestamp1 (if bool1 'click 'no-evt))
                                        (list timestamp2 (if bool2 'click 'no-evt))
                                      ;  (list timestamp3 (if bool3 'click 'no-evt))
                                        )
                                  (list (list timestamp4 (if bool4 'click 'no-evt))
                                        (list timestamp5 (if bool5 'click 'no-evt))
                                      ;  (list timestamp6 (if bool6 'click 'no-evt))
                                        ))
                                         
                              ))
(if (unsat? binding)
    (displayln "No binding was found.")
    (print-forms binding))
(define end-time (current-seconds))
(printf "Took ~a seconds~n" (- end-time begin-time))

(displayln "Synthesize startsWith and collectE portion")
(define collect-begin-time (current-seconds))
(define collect-binding
  (synthesize #:forall (list timestamp1 bool1
                               timestamp2 bool2
                               timestamp3 bool3
                               timestamp4 bool4
                               )
              #:guarantee (same collect-graph synth-collect-graph
                                (list (list timestamp1 1)
                                      (list timestamp2 -1)
                                      (list timestamp3 1))
                                        )
              ))
(define collect-end-time (current-seconds))
(if (unsat? collect-binding)
    (displayln "No binding was found.")
    (print-forms collect-binding))
(printf "Took ~a seconds~n" (- collect-end-time collect-begin-time))

(displayln "Synthesize full program")
(define full-begin-time (current-seconds))
(define full-binding
  (synthesize #:forall (list timestamp1 bool1
                             timestamp2 bool2
                             timestamp3 bool3
                             timestamp4 bool4)
              #:guarantee (same2 inc-dec-button-graph synth-inc-dec-button-graph
                                 (list (list timestamp1 bool1)
                                       (list timestamp2 bool2))
                                 (list (list timestamp3 bool3)
                                       (list timestamp4 bool4)))
              ))
(define full-end-time (current-seconds))
(if (unsat? full-binding)
    (displayln "No binding was found.")
    (print-forms full-binding))
(printf "Took ~a seconds~n" (- full-end-time full-begin-time))

