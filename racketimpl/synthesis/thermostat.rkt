#lang rosette/safe
(require rosette/lib/synthax)

(require "../fjmodels.rkt")
(require "../rosettefjapi.rkt")
(require "../benchmarks/thermostat.rkt")
;(require "grammar.rkt")

;;                    ifB
;;            /        |         \
;;          andB    constantB    constantB
;;        /    \        |            |
;;    liftB    liftB   'on          'off
;;   /    \     /   \
;;  λ   tempB  λ   clockB

(define-synthax (flapjax-grmr input ... depth)
  #:base (choose input ...)
  #:else (choose input ...
                 ;; zero arity
                 ;(zeroE)
                 (constantB (choose 'on 'off (??)))
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
                  ; delayE
                  ; blindE
                  ; calmE
                  ; mapE
                  ; filterE
                   liftB
                   )
                  (choose ;(??)
                         ; (λ (e) (if e 'on 'off))
                         ; (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))
                          (λ (t) (<= t (??)))
                          (λ (c) (or (>= (vector-ref c 0) (??)) (>= (??) (vector-ref c 0))))
                         ; (λ (e) (list (get-timestamp e) (+ (get-value e) (??))))
                          )
                  (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= collectE init λ E
                 ;(collectE (??) + (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= arity-2-op E E
                 ((choose andB
                          ;mergeE
                          ;snapshotE
                          ) (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= liftB λ E E
                 ;(liftB (choose (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))
                 ;               (λ (clock location) (if (or (>= (time-vec->integer clock) 2130) (< (time-vec->integer clock) 800))
                 ;                'night
                 ;                (if (equal? location 'home)
                 ;                    'home
                 ;                    'away))))
                 ;       (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= arity-3-op E E E               
                 ((choose ;ifE
                          ifB
                          ) (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)))          
                 ))

(unless (>= (current-bitwidth) 6)
  (displayln "bitwidth too low for time vec!!!!"))

(define (thermostat-graph tempB clockB)
  (ifB (andB (liftB (λ (t) (<= t temp-floor)) tempB)
             (liftB (λ (c) (or (>= (vector-ref c 0) hour-begin) (>= hour-end (vector-ref c 0)))) clockB))
       (constantB 'on)
       (constantB 'off)))

(define (synth-thermostat-graph tempB clockB)
  (flapjax-grmr clockB tempB 3))

(define s-tempB (new-behavior sym-integer 2))
(define s-clockB (new-behavior sym-time-vec 2))

(check-existence-of-solution thermostat-assumptions s-tempB s-clockB)

(assert (thermostat-assumptions s-tempB s-clockB))

(define begin-time (current-seconds))
(define binding (synthesize #:forall (append (harvest-behavior s-tempB) (harvest-behavior s-clockB))
                            #:guarantee (assert (same thermostat-graph synth-thermostat-graph
                                                      s-tempB s-clockB))
                            ))
(define end-time (current-seconds))
(if (unsat? binding)
    (displayln "No binding was found.")
    (print-forms binding))
(printf "Took ~a seconds~n" (- end-time begin-time))




