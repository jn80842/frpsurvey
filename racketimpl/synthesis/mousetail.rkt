#lang rosette/safe

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

(define-synthax (flapjax-grmr input ... depth)
  #:base (choose input ...)
  #:else (let ([possible-ints (choose -1 0 1 2 3 4 5)])
           (choose input ...
                 ;; zero arity
                 ;(zeroE)
                 ;(constantB (choose 'on 'off (??)))
                 ;; E ::= arity-1-op E
                 ;((choose oneE
                 ;  switchE
                 ;  notE
                 ;  changes
                 ;  ) (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= arity-1-op val E
                 ((choose
                  ; startsWith
                  ; constantE
                   delayE
                  ; blindE
                  ; calmE
                   mapE
                  ; filterE
                  ; liftB
                   )
                  (choose possible-ints
                          ;'on 'off
                          ;(λ (e) (if e 'on 'off))
                          ;(λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))
                          ;(λ (t) (<= t (??)))
                          ;(λ (c) (or (>= (vector-ref c 0) (??)) (>= (??) (vector-ref c 0))))
                          (λ (e) (list (get-timestamp e) (+ (get-value e) possible-ints)))
                          ;(λ (e) (list (get-timestamp e)
                          ; (startBehaviorAtTimestamp (get-timestamp e) (choose input ...))))
                          ;(λ (e) (list (get-timestamp e) (zeroE)))
                          )
                  (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= collectE init λ E
                 ;(collectE (??) + (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= arity-2-op E E
                 ;((choose andB
                 ;         mergeE
                 ;         snapshotE
                 ;         ) (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)))
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

(define (spec-mouse-tail-x-graph x-evt-stream)
  (mapE (λ (e) (list (get-timestamp e) (+ (get-value e) x-offset))) (delayE time-delay x-evt-stream)))

(define (spec-mouse-tail-y-graph y-evt-stream)
  (delayE time-delay y-evt-stream))

(define (synth-mouse-tail-x-graph x-evt-stream)
  (flapjax-grmr x-evt-stream 3))

(define (synth-mouse-tail-y-graph y-evt-stream)
  (flapjax-grmr y-evt-stream 3))

(define s-mouse-x (new-event-stream sym-integer 3))
(define s-mouse-y (new-event-stream sym-integer 3))

(assert (mousetail-x-assumptions s-mouse-x))
(assert (mousetail-y-assumptions s-mouse-y))

(print-bitwidth-warning)

(displayln "Synthesize mousetail program:")

(define begin-time (current-seconds))
;; synthesize a program that is equivalent to benchmark program
#;(define binding
    (time (synthesize #:forall (append (harvest-events s-mouse-x)
                                 (harvest-events s-mouse-y))
                #:guarantee (time (begin (assert (same spec-mouse-tail-x-graph
                                         synth-mouse-tail-x-graph
                                         s-mouse-x))
                                  (assert (same spec-mouse-tail-y-graph
                                       synth-mouse-tail-y-graph
                                         s-mouse-y)))))))
;; synthesize program that matches spec
#;(define binding
  (time (synthesize #:forall (append (harvest s-mouse-x) (harvest s-mouse-y))
                    #:guarantee (assert (mousetail-guarantees s-mouse-x s-mouse-y (synth-mouse-tail-x-graph s-mouse-x)
                                                              (synth-mouse-tail-y-graph s-mouse-y))))))
;; try to synthesize program that matches spec but is not equivalent to benchmark program
#;(define binding
  (synthesize #:forall (append (harvest s-mouse-x) (harvest s-mouse-y))
              #:guarantee (let ([x-graph (synth-mouse-tail-x-graph s-mouse-x)]
                                [y-graph (synth-mouse-tail-y-graph s-mouse-y)])
                            (begin (assert (mousetail-guarantees s-mouse-x s-mouse-y
                                                               x-graph y-graph))
                                 (assert (or (not (equal? (spec-mouse-tail-x-graph s-mouse-x)
                                                         x-graph))
                                             (not (equal? (spec-mouse-tail-y-graph s-mouse-y)
                                                          y-graph))))))))
;; synthesize program that matches input/output pairs
#;(define binding
  (synthesize #:forall (append (harvest s-mouse-x) (harvest s-mouse-y))
              #:guarantee (begin (assert (equal? (synth-mouse-tail-x-graph concrete-mouse-x-input)
                                                 concrete-mouse-x-output))
                                 (assert (equal? (synth-mouse-tail-y-graph concrete-mouse-y-input)
                                                 concrete-mouse-y-output)))))
;; synthesize program that matches input/output pairs but is not equivalent to benchmark program
(define binding
  (synthesize #:forall (append (harvest s-mouse-x) (harvest s-mouse-y))
              #:guarantee (begin (assert (equal? (synth-mouse-tail-x-graph concrete-mouse-x-input)
                                                 concrete-mouse-x-output))
                                   (assert (equal? (synth-mouse-tail-y-graph concrete-mouse-y-input)
                                                 concrete-mouse-y-output))
                                   (assert (or (not (equal? (spec-mouse-tail-x-graph s-mouse-x)
                                                            (synth-mouse-tail-x-graph s-mouse-x)))
                                               (not (equal? (spec-mouse-tail-y-graph s-mouse-y)
                                                            (synth-mouse-tail-y-graph s-mouse-y))))))))
(if (unsat? binding)
    (displayln "No binding was found.")
    (print-forms binding))
(define end-time (current-seconds))
(printf "Took ~a seconds~n" (- end-time begin-time))
