#lang rosette/safe
(require rosette/lib/synthax)

(require "../fjmodels.rkt")
(require "../rosettefjapi.rkt")
(require "../benchmarks/kitchenlight.rkt")
(require "grammar.rkt")

(current-bitwidth 6)

(define-synthax (grmr input1 input2 input3 depth)
  #:base (choose input1 input2 input3)
  #:else (choose input1 input2 input3
                  (liftB (choose (λ (e) (if e 'on 'off))
                                 (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none)))
                         (grmr input1 input2 input3 (sub1 depth)))
                  (liftB (λ (clock location) (if (or (>= (time-vec->integer clock) 2130) (< (time-vec->integer clock) 800))
                                 'night
                                 (if (equal? location 'home)
                                     'home
                                     'away)))
                         (grmr input1 input2 input3 (sub1 depth)) (grmr input1 input2 input3 (sub1 depth)))
                         ))

;; between 21:30 and 8:00 inclusive, mode is night
;; otherwise, if user is at home, home, else away
(define (mode-graph clockB userLocationB motionSensorB)
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

(define (synth-graph clockB userLocationB motionSensorB)
  (flapjax-grmr clockB userLocationB motionSensorB 3))

(define stream-length 2)

(define s-clockB (new-behavior sym-time-vec stream-length))
(define s-locationB (new-behavior (sym-union-constructor 'home 'not-at-home) stream-length))
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
                           (same mode-graph synth-graph s-clockB s-locationB s-motionSensorB)
                                  )))
(define end-time (current-seconds))
(if (unsat? binding)
    (displayln "No binding was found.")
    (print-forms binding))
(printf "Took ~a seconds~n" (- end-time begin-time))
