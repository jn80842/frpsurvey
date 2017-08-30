#lang rosette/safe
(require rosette/lib/synthax)

(require "../fjmodels.rkt")
(require "../rosettefjapi.rkt")
(require "../benchmarks/kitchenlight.rkt")
;(require "grammar.rkt")

(current-bitwidth 6)

;;            liftB
;;         /    |       \
;;       λ    liftB         liftB
;;        /     |          /  |   \
;;      λ  motionSensorB  λ clockB locationB

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
                ; ((choose
                 ;  startsWith
                 ;  constantE
                 ;  delayE
                 ;  blindE
                 ;  calmE
                 ;  mapE
                 ;  filterE
                 ;  liftB
                 ;  )
                 ; (choose ;(??)
                 ;         (λ (e) (if e 'on 'off))
                 ;         (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))
                         ; (λ (t) (<= t (??)))
                 ;         (λ (c) (or (>= (vector-ref c 0) (??)) (>= (??) (vector-ref c 0))))
                         ; (λ (e) (list (get-timestamp e) (+ (get-value e) (??))))
                 ;         )
                 ; (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= collectE init λ E
                ; (collectE (??) + (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= arity-2-op E E
                ; ((choose andB
                ;          mergeE
                ;          snapshotE
                ;          ) (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= liftB λ E E
                 (liftB ;(choose (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))
                                (λ (clock location) (if (or (>= (vector-ref clock 0) 21) (< (vector-ref clock 0) 8))
                                 'night
                                 (if (equal? location 'home)
                                     'home
                                     'away))) ;)
                        (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= arity-3-op E E E
                 ;((choose ifE
                  ;        ifB
                   ;       ) (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)))
                 ))

;; between 21:30 and 8:00 inclusive, mode is night
;; otherwise, if user is at home, home, else away
(define (mode-graph clockB userLocationB) ;motionSensorB)
  (liftB (λ (clock location) (if (or (>= (time-vec->integer clock) 2130) (< (time-vec->integer clock) 800))
                                 'night
                                 (if (equal? location 'home)
                                     'home
                                     'away)))
         clockB userLocationB))

;; when motion sensor senses motion, light turns on
(define (kitchen-light-status-graph clockB userLocationB motionSensorB)
  (liftB (λ (e) (if e 'on 'off)) motionSensorB))

;; when light turns on, if mode is night, light is orange, else white
(define (kitchen-light-color-graph kitchenLightB modeB)
  (liftB (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none)) kitchenLightB modeB))

;; full graph
(define (kitchen-light-graph clockB userLocationB motionSensorB)
  (liftB (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))
         (liftB (λ (e) (if e 'on 'off)) motionSensorB)
         (liftB (λ (clock location) (if (or (>= (time-vec->integer clock) 2130) (< (time-vec->integer clock) 800))
                                 'night
                                 (if (equal? location 'home)
                                     'home
                                     'away)))
         clockB userLocationB)))

(define (synth-graph clockB userLocationB) ; motionSensorB)
  (flapjax-grmr clockB userLocationB 3))

(define stream-length 2)

(define s-clockB (new-behavior sym-time-vec stream-length))
(define s-locationB (new-behavior (sym-union-constructor 'home 'away) stream-length))
(define s-motionSensorB (new-behavior sym-boolean stream-length))

(assert (light-color-assumptions s-clockB s-locationB s-motionSensorB))
;; limit range of timestamps, lock down minutes
(assert (and (andmap (λ (ts) (>= (* 4 stream-length) ts)) (append (map get-timestamp (behavior-changes s-clockB))
                                                                  (map get-timestamp (behavior-changes s-locationB))
                                                                  (map get-timestamp (behavior-changes s-motionSensorB))))
             (behavior-check (λ (v) (and (= (vector-ref v 1) 0) (= (vector-ref v 2) 0))) s-clockB)))

(displayln "Synthesize kitchen light program")

(define begin-time (current-seconds))
(define binding
  (synthesize #:forall (append
                        (harvest-behavior s-clockB)
                               (harvest-behavior s-locationB)
                               (harvest-behavior s-motionSensorB))
              #:guarantee (assert ;(same kitchen-light-graph synth-graph s-clockB s-locationB s-motionSensorB)
                           (same mode-graph synth-graph s-clockB s-locationB) ;s-motionSensorB)
                                  )))
(define end-time (current-seconds))
(if (unsat? binding)
    (displayln "No binding was found.")
    (print-forms binding))
(printf "Took ~a seconds~n" (- end-time begin-time))
