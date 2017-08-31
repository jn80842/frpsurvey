#lang rosette/safe
(require rosette/lib/synthax)

(require "../fjmodels.rkt")
(require "../rosettefjapi.rkt")
(require "../benchmarks/porchlight.rkt")
;(require "grammar.rkt")

;;                    mergeE
;;               /            \
;;            blindE          calmE
;;          /      \          /    \
;;         int   constantE   int   constantE
;;               /      \           /       \
;;              'on   md-events   'off     md-events

(define-synthax (flapjax-grmr input ... depth)
  #:base (choose input ...)
  #:else (choose input ...
                 ;; zero arity
                ; (zeroE)
                ; (constantB (choose 'on 'off (??)))
                 ;; E ::= arity-1-op E
                ; ((choose oneE
                ;   switchE
                ;   notE
                ;   changes
                ;   ) (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= arity-1-op val E
                 ((choose
                 ;  startsWith
                   constantE
                 ;  delayE
                   blindE
                   calmE
                 ;  mapE
                 ;  filterE
                 ;  liftB
                   )
                  (choose
                   2
                   'on 'off
                   ;       (λ (e) (if e 'on 'off))
                   ;       (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))
                   ;       (λ (t) (<= t (??)))
                   ;       (λ (c) (or (>= (vector-ref c 0) (??)) (>= (??) (vector-ref c 0))))
                   ;       (λ (e) (list (get-timestamp e) (+ (get-value e) (??))))
                          )
                  (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= collectE init λ E
                 ;(collectE (??) + (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= arity-2-op E E
                 (;(choose ;andB
                          mergeE
                          ;snapshotE
                   ;       )
                  (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= liftB λ E E
                ; (liftB (choose (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))
                ;                (λ (clock location) (if (or (>= (time-vec->integer clock) 2130) (< (time-vec->integer clock) 800))
                ;                 'night
                ;                 (if (equal? location 'home)
                ;                     'home
                ;                     'away))))
                 ;       (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= arity-3-op E E E               
                ; ((choose ifE
                ;          ifB
                ;          ) (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)))          
                 ))

(printf "Current bitwidth: ~a~n" (current-bitwidth))

(define s-motion (new-event-stream (sym-union-constructor 'motion 'no-evt) stream-length))

(define (light-graph md-events)
  (mergeE (blindE delay-by (constantE 'on md-events)) (calmE calm-by (constantE 'off md-events))))

(define (synth-light-graph md-events)
  (flapjax-grmr md-events 2))

(assert (light-assumptions s-motion))

(displayln "Synthesize porchlight program:")

(define begin-time (current-seconds))
(define binding
    (synthesize #:forall (harvest-events s-motion)
                #:guarantee (assert (same light-graph synth-light-graph s-motion))))
(if (unsat? binding)
    (displayln "No binding was found.")
    (print-forms binding))
(define end-time (current-seconds))
(printf "Took ~a seconds~n" (- end-time begin-time))
