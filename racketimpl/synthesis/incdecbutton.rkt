#lang rosette/safe
(require rosette/lib/synthax)

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../benchmarks/incdecbutton.rkt")
(require "grammar.rkt")
(require "straightline.rkt")


;;         startsWith
;;          /       \
;;         0       collectE     
;;                  /  |  \
;;                λ    0  mergeE
;;                       /      \
;;                 constantE constantE
;;                  /     \    /     \
;;                 inc    1   dec    -1

(define-synthax (flapjax-grmr input ... depth)
  #:base (choose input ...)
  #:else (let ([recursive-call1 (flapjax-grmr input ... (sub1 depth))]
               [recursive-call2 (flapjax-grmr input ... (sub1 depth))]
               )
           (choose input ...
                   (startsWith (??) recursive-call1)
                   (constantE (??) recursive-call1)
                   (collectE (??) + recursive-call1)
                   (mergeE recursive-call1 recursive-call2)
                   )))

;; r1 - inc button
;; r2 - dec button

;; r3 := constantE 1 r1
;; r4 := constantE -1 r2
;; r5 := mergeE

(define (synth-inc-dec-button-graph inc dec)
  (flapjax-grmr inc dec 4))

(define (manual-synth-inc-dec-button-graph inc dec)
  (choose inc dec
          (startsWith 0 inc)
          (startsWith 0 dec)
          (constantE 1 inc)
          (constantE -1 inc)
          (constantE 1 dec)
          (constantE -1 dec)
          (collectE 0 + inc)
          (collectE 0 + dec)
          (mergeE inc dec)
          ))

(print-bitwidth-warning)

;(assert (button-assumptions s-inc s-dec))

(define (test-graph inc dec)
  (mergeE inc dec))
(define (synth-test-graph inc dec)
  (flapjax-grmr inc dec 4))

(assert (andmap (λ (i d) (not (and (equal? i 'click) (equal? d 'click)))) s-inc s-dec))

(displayln "Synthesize inc/dec button program:")

(displayln "Synthesize full program")
(define begin-time (current-seconds))
;; synthesize program that matches benchmark program
#;(define binding
  (time (synthesize #:forall (append (harvest s-inc) (harvest s-dec))
                    #:guarantee (assert (same inc-dec-button-graph
                                              fully-expanded-sketch-graph
                                             ;; synth-inc-dec-button-graph
                                              s-inc s-dec)))))
;; synthesize program that matches spec
#;(define binding
  (time (synthesize #:forall (append (harvest s-inc) (harvest s-dec))
              #:guarantee (assert (button-guarantees (fully-expanded-sketch-graph s-inc s-dec))))))
;; synthesize program that matches spec but is not equivalent to benchmark program
#;(define binding
  (synthesize #:forall (append (harvest s-inc) (harvest s-dec))
              #:guarantee (let ([synth-graph (synth-inc-dec-button-graph s-inc s-dec)])
                            (begin
                              (assert (button-guarantees (synth-inc-dec-button-graph s-inc s-dec)))
                              (assert (not (same inc-dec-button-graph
                                               synth-inc-dec-button-graph exist-s-inc exist-s-dec)))))))

;; synthesize program that matches input/output pair
(define binding
  (synthesize #:forall (append (harvest s-inc) (harvest s-dec))
              #:guarantee (assert (equal? (fully-expanded-sketch-graph concrete-inc-clicks concrete-dec-clicks)
                                          concrete-counter))))

;; synthesize program that matches input/output pair but is not equivalent to benchmark program
#;(define binding
  (synthesize #:forall (append (harvest s-inc) (harvest s-dec))
              #:guarantee (begin 
                            (assert (equal? (synth-inc-dec-button-graph concrete-inc-clicks concrete-dec-clicks)
                                            concrete-counter))
                            #;(assert (equal? (synth-inc-dec-button-graph2 concrete-inc-clicks concrete-dec-clicks)
                                            concrete-counter))
                            (assert (same synth-inc-dec-button-graph
                                         synth-inc-dec-button-graph2 s-inc s-dec)))))

(define end-time (current-seconds))
(if (unsat? binding)
    (displayln "No binding was found.")
    (function-printer binding))
(printf "Took ~a seconds~n" (- end-time begin-time))
