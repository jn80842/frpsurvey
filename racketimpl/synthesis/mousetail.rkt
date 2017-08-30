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
  #:else (choose input ...
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
                  (choose (??)
                          ;'on 'off
                          ;(λ (e) (if e 'on 'off))
                          ;(λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))
                          ;(λ (t) (<= t (??)))
                          ;(λ (c) (or (>= (vector-ref c 0) (??)) (>= (??) (vector-ref c 0))))
                          (λ (e) (list (get-timestamp e) (+ (get-value e) (??))))
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
                 )) 

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

(displayln "Synthesize mousetail program:")

(define begin-time (current-seconds))
(define binding
    (time (synthesize #:forall (append (harvest-events s-mouse-x)
                                 (harvest-events s-mouse-y))
                #:guarantee (time (begin (assert (same spec-mouse-tail-x-graph
                                         synth-mouse-tail-x-graph
                                         s-mouse-x))
                                  (assert (same spec-mouse-tail-y-graph
                                       synth-mouse-tail-y-graph
                                         s-mouse-y)))))))
(if (unsat? binding)
    (displayln "No binding was found.")
    (print-forms binding))
(define end-time (current-seconds))
(printf "Took ~a seconds~n" (- end-time begin-time))
