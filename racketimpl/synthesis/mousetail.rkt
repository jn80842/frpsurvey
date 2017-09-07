#lang rosette

(require rosette/lib/synthax)

(require "../fjmodels.rkt")
(require "../rosettefjapi.rkt")
(require "../benchmarks/mousetail.rkt")
(require "grammar.rkt")

;;      mapE                    delayE
;;   /      \                   /    \
;; λ        delayE             3     mouse-y
;;         /      \
;;        3     mouse-x

#;(define-synthax (flapjax-grmr input ... depth)
  #:base (choose input ...)
  #:else (let ([recursive-call1 (flapjax-grmr input ... (sub1 depth))])
           (choose input ...
                   (delayE 3 recursive-call1)
                   (mapE (λ (e) (list (get-timestamp e) (+ (get-value e) 5))) recursive-call1))))

(define-synthax (flapjax-grmr input ... depth)
  #:base (choose input ...)
  #:else (let ([recursive-call1 (flapjax-grmr input ... (sub1 depth))]
               ;[recursive-call2 (flapjax-grmr input ... (sub1 depth))]
               ;[recursive-call3 (flapjax-grmr input ... (sub1 depth))]
               ;[possible-ints 3]
               )
           (choose input ...
                   ;; zero arity
                   ;(zeroE)
                   ;(constantB (choose 'on 'off (??)))
                   ;; E ::= arity-1-op E
                   ;((choose oneE
                   ;         switchE
                   ;         notE
                   ;         changes
                   ;) recursive-call1)
                 ;; E ::= arity-1-op val E
                 ((choose
                   ;startsWith
                   ;constantE
                   delayE
                   ;blindE
                   ;calmE
                   mapE
                   ;filterE
                   ;liftB
                   )
                  (choose 3
                          ;'on 'off
                          ;(λ (e) (if e 'on 'off))
                          ;(λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))
                          ;(λ (t) (<= t possible-ints))
                          ;(λ (c) (or (>= (vector-ref c 0) possible-ints) (>= possible-ints (vector-ref c 0))))
                          (λ (e) (list (get-timestamp e) (+ (get-value e) 5)))
                          ;(λ (e) (list (get-timestamp e)
                          ; (startBehaviorAtTimestamp (get-timestamp e) (choose input ...))))
                          ;(λ (e) (list (get-timestamp e) (zeroE)))
                          )
                  recursive-call1)
                 ;; E ::= collectE init λ E
                 ;(collectE possible-ints + recursive-call1)
                 ;; E ::= arity-2-op E E
                 ;((choose andB
                 ;         mergeE
                 ;         snapshotE
                 ;         ) recursive-call1 recursive-call2)
                 ;; E ::= liftB λ E E
                 ;(liftB (choose (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))
                 ;               (λ (clock location) (if (or (>= (time-vec->integer clock) 2130) (< (time-vec->integer clock) 800))
                 ;                'night
                 ;                (if (equal? location 'home)
                 ;                    'home
                 ;                    'away))))
                 ;       recursive-call1 recursive-call2)
                 ;; E ::= arity-3-op E E E
                 ;((choose ifE
                 ;         ifB
                 ;         ) recursive-call1 recursive-call2 recursive-call3)
                 )))

(define (synth-mousetail-x-graph x-evt-stream)
  (flapjax-grmr x-evt-stream 3))

(define (synth-mousetail-y-graph y-evt-stream)
  (flapjax-grmr y-evt-stream 1))

(assert (mousetail-assumptions s-mouse-x s-mouse-y))

(print-bitwidth-warning)

(displayln "Synthesize mousetail program:")

(define begin-time (current-seconds))
;; synthesize a program that is equivalent to benchmark program
(define binding
    (time (synthesize #:forall (append (harvest s-mouse-x)
                                       (harvest s-mouse-y))
                #:guarantee (time (begin (assert (same mousetail-x-graph
                                         synth-mousetail-x-graph
                                         s-mouse-x))
                                  (assert (same mousetail-y-graph
                                       synth-mousetail-y-graph
                                         s-mouse-y)))))))
;; synthesize program that matches spec
#;(define binding
  (time (synthesize #:forall (append (harvest s-mouse-x) (harvest s-mouse-y))
                    #:guarantee (assert (mousetail-guarantees s-mouse-x s-mouse-y
                                                              (synth-mousetail-x-graph s-mouse-x)
                                                              (synth-mousetail-y-graph s-mouse-y))))))
;; try to synthesize program that matches spec but is not equivalent to benchmark program
#;(define binding
  (synthesize #:forall (append (harvest s-mouse-x) (harvest s-mouse-y))
              #:guarantee (let ([x-graph (synth-mousetail-x-graph s-mouse-x)]
                                [y-graph (synth-mousetail-y-graph s-mouse-y)])
                            (begin (assert (mousetail-guarantees s-mouse-x s-mouse-y
                                                               x-graph y-graph))
                                 (assert (or (not (equal? (spec-mousetail-x-graph s-mouse-x)
                                                         x-graph))
                                             (not (equal? (spec-mousetail-y-graph s-mouse-y)
                                                          y-graph))))))))
;; synthesize program that matches input/output pairs
#;(define binding
  (synthesize #:forall (append (harvest s-mouse-x) (harvest s-mouse-y))
              #:guarantee (begin (assert (equal? (synth-mousetail-x-graph concrete-mouse-x-input)
                                                 concrete-mouse-x-output))
                                 (assert (equal? (synth-mousetail-y-graph concrete-mouse-y-input)
                                                 concrete-mouse-y-output)))))
;; synthesize program that matches input/output pairs but is not equivalent to benchmark program
#;(define binding
  (synthesize #:forall (append (harvest s-mouse-x) (harvest s-mouse-y))
              #:guarantee (begin (assert (equal? (synth-mousetail-x-graph concrete-mouse-x-input)
                                                 concrete-mouse-x-output))
                                   (assert (equal? (synth-mousetail-y-graph concrete-mouse-y-input)
                                                 concrete-mouse-y-output))
                                   (assert (or (not (equal? (mousetail-x-graph s-mouse-x)
                                                            (synth-mouse-tail-x-graph s-mouse-x)))
                                               (not (equal? (mousetail-y-graph s-mouse-y)
                                                            (synth-mouse-tail-y-graph s-mouse-y))))))))
(if (unsat? binding)
    (displayln "No binding was found.")
    (print-forms binding))
(define end-time (current-seconds))
(printf "Took ~a seconds~n" (- end-time begin-time))
