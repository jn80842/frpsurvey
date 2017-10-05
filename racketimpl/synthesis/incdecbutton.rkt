#lang rosette/safe
(require rosette/lib/synthax)

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")
(require "../benchmarks/incdecbutton.rkt")
(require "grammar.rkt")


;;         startsWith
;;          /       \
;;         0       collectE     
;;                  /  |  \
;;                λ    0  mergeE
;;                       /      \
;;                 constantE constantE
;;                  /     \    /     \
;;                 inc    1   dec    -1


(define-synthax (event-stream-grmr input ... depth)
  #:base (choose input ...)
  #:else (choose input ...
                   (constantE-grmr input ... (sub1 depth))
                   (collectE-grmr input ... (sub1 depth))
                   (mergeE-grmr input ... (sub1 depth))))

(define-synthax (constantE-grmr input ... depth)
  #:base (choose input ...)
  #:else (choose input ...
                 (constantE (??)
                            (choose input ...
                                    (mergeE-grmr input ... (sub1 depth))))))

(define-synthax (collectE-grmr input ... depth)
  #:base (choose input ...)
  #:else (choose input ...
                 (collectE (??) + (choose input ...
                                       (constantE-grmr input ... (sub1 depth))
                                       (mergeE-grmr input ... (sub1 depth))))))

(define-synthax (mergeE-grmr input ... depth)
  #:base (choose input ...)
  #:else (choose input ...
                 (mergeE (choose input ...
                                 (constantE-grmr input ... (sub1 depth))
                                 (collectE-grmr input ... (sub1 depth)))
                         (choose input ...
                                 (constantE-grmr input ... (sub1 depth))
                                 (collectE-grmr input ... (sub1 depth)))
                         )))

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

#;(define-synthax (flapjax-grmr input ... depth)
  #:base (choose input ... )
  #:else (choose input ...
                 (startsWith 0 (flapjax-grmr input ... (sub1 depth)))
                 (constantE (choose 1 -1) (flapjax-grmr input ... (sub1 depth)))
                 (collectE 0 + (flapjax-grmr input ... (sub1 depth)))
                 (mergeE (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)))))

(define (synth-inc-dec-button-graph inc dec)
  (flapjax-grmr inc dec 4))
  ;(startsWith 0 (event-stream-grmr inc dec 4)))
  ;(startsWith 0 (flapjax-grmr inc dec 3)))

(define (synth-inc-dec-button-graph2 inc dec)
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
                                              synth-inc-dec-button-graph
                                              s-inc s-dec)))))
;; synthesize program that matches spec
#;(define binding
  (time (synthesize #:forall (append (harvest s-inc) (harvest s-dec))
              #:guarantee (assert (button-guarantees (synth-inc-dec-button-graph s-inc s-dec))))))
;; synthesize program that matches spec but is not equivalent to benchmark program
#;(define binding
  (synthesize #:forall (append (harvest s-inc) (harvest s-dec))
              #:guarantee (begin
                            (assert (button-guarantees (synth-inc-dec-button-graph s-inc s-dec)))
                            (assert (not (same inc-dec-button-graph
                                               synth-inc-dec-button-graph s-inc s-dec))))))

;; synthesize program that matches input/output pair
#;(define binding
  (synthesize #:forall '() ; (append (harvest s-inc) (harvest s-dec))
              #:guarantee (assert (equal? (synth-inc-dec-button-graph concrete-inc-clicks concrete-dec-clicks)
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
    (print-forms binding))
(printf "Took ~a seconds~n" (- end-time begin-time))
