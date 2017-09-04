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
  #:else (let ([possible-ints (choose -1 0 1 2 3 4 5)])
           (choose input ...
                 ;; zero arity
                 ;(zeroE)
                 ;(constantB (choose 'on 'off (??)))
                 ;; E ::= arity-1-op E
                 ;((choose oneE
                  ; switchE
                  ; notE
                  ; changes
                  ; ) (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= arity-1-op val E
                 ((choose
                   startsWith
                   constantE
                  ; delayE
                  ; blindE
                  ; calmE
                  ; mapE
                  ; filterE
                  ; liftB
                   )
                  (choose possible-ints
                         ; (λ (e) (if e 'on 'off))
                         ; (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))
                         ; (λ (t) (<= t (??)))
                         ; (λ (c) (or (>= (vector-ref c 0) (??)) (>= (??) (vector-ref c 0))))
                         ; (λ (e) (list (get-timestamp e) (+ (get-value e) (??))))
                          )
                  (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= collectE init λ E
                 (collectE possible-ints + (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= arity-2-op E E
                 (;(choose ;andB
                          mergeE
                          ;snapshotE
                          ;)
                          (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= liftB λ E E
                 ;(liftB (choose (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))
                 ;               (λ (clock location) (if (or (>= (time-vec->integer clock) 2130) (< (time-vec->integer clock) 800))
                 ;                'night
                 ;                (if (equal? location 'home)
                 ;                    'home
                 ;                    'away))))
                 ;       (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= arity-3-op E E E               
                 ;((choose ifE
                 ;         ifB
                 ;         ) (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)))          
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
#;(define binding
  (synthesize #:forall (append (harvest-events s-inc) (harvest-events s-dec))
              #:guarantee (assert (same inc-dec-button-graph synth-inc-dec-button-graph
                                 s-inc s-dec))))
;; synthesize program that matches spec
(define binding
  (synthesize #:forall (append (harvest s-inc) (harvest s-dec))
              #:guarantee (assert (button-guarantees (synth-inc-dec-button-graph s-inc s-dec)))))
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

