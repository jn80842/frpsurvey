#lang rosette/safe
(require rosette/lib/synthax)

(require "../fjmodels.rkt")
(require "../rosettefjapi.rkt")
(require "../benchmarks/incdecbutton.rkt")
(require "grammar.rkt")


;;         startsWith
;;          /       \
;;         0       collectE     
;;                  /   \
;;                λ     mergeE
;;                     /      \
;;                constantE constantE
;;                 /     \    /     \
;;                inc    1   dec    -1

(define-synthax (flapjax-grmr input ... depth)
  #:base (choose input ...)
  #:else (let ([recursive-call1 (flapjax-grmr input ... (sub1 depth))]
               [recursive-call2 (flapjax-grmr input ... (sub1 depth))]
               ;[recursive-call3 (flapjax-grmr input ... (sub1 depth))]
               )
           (choose input ...
                  ; (zeroE)
                  ; (constantB (choose 'on 'off (??)))
                  ; (oneE recursive-call1)
                  ; (switchE recursive-call1)
                  ; (notE recursive-call1)
                  ; (changes recursive-call1)
                   (startsWith 0 recursive-call1)
                   (constantE (choose 1 -1) recursive-call1)
                  ; (delayE (??) recursive-call1)
                  ; (blindE (??) recursive-call1)
                  ; (calmE (??) recursive-call1)
                  ; (mapE (choose (λ (e) (list (get-timestamp e) (+ (get-value e) (??))))
                  ;               (λ (e) (list (get-timestamp e)
                  ;                            (startBehaviorAtTimestamp (get-timestamp e) (choose input ...))))
                  ;               (λ (e) (list (get-timestamp e) (zeroE)))) recursive-call1)
                  ; (filterE (choose (λ (t) (<= t (??)))
                  ;                  (λ (c) (or (>= (vector-ref c 0) (??)) (>= (??) (vector-ref c 0))))) recursive-call1)
                  ; (liftB (λ (e) (if e 'on 'off)) recursive-call1)
                   (collectE 0 + recursive-call1)
                  ; (andB recursive-call1 recursive-call2)
                   (mergeE recursive-call1 recursive-call2)
                  ; (snapshotE recursive-call1 recursive-call2)
                  ; (liftB (choose (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))
                  ;                (λ (clock location) (if (or (>= (time-vec->integer clock) 2130) (< (time-vec->integer clock) 800))
                  ;                                        'night
                  ;                                        (if (equal? location 'home)
                  ;                                            'home
                  ;                                            'away)))) recursive-call1 recursive-call2)
                  ; (ifE recursive-call1 recursive-call2 recursive-call3)
                  ; (ifB recursive-call1 recursive-call2 recursive-call3)
                   )))

(define (synth-inc-dec-button-graph inc dec)
  (flapjax-grmr inc dec 4))

(print-bitwidth-warning)

(assert (button-assumptions s-inc s-dec))

(displayln "Synthesize inc/dec button program:")

;; synthesized in 359 seconds
(displayln "Synthesize full program")
(define begin-time (current-seconds))
;; synthesize program that matches benchmark program
(define binding
  (time (synthesize #:forall (append (harvest s-inc) (harvest s-dec))
              #:guarantee (assert (same inc-dec-button-graph synth-inc-dec-button-graph
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
  (synthesize #:forall (append (harvest s-inc) (harvest s-dec))
              #:guarantee (assert (equal? (synth-inc-dec-button-graph concrete-inc-clicks concrete-dec-clicks)
                                          concrete-counter))))
;; synthesize program that matches input/output pair but is not equivalent to benchmark program
#;(define binding
  (synthesize #:forall (append (harvest s-inc) (harvest s-dec))
              #:guarantee (begin
                            (assert (equal? (synth-inc-dec-button-graph concrete-inc-clicks concrete-dec-clicks)
                                            concrete-counter))
                            (assert (not same inc-dec-button-graph
                                         synth-inc-dec-button-graph s-inc s-dec)))))
(define end-time (current-seconds))
(if (unsat? binding)
    (displayln "No binding was found.")
    (print-forms binding))
(printf "Took ~a seconds~n" (- end-time begin-time))

