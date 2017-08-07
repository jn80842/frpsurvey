#lang rosette/safe
(require rosette/lib/synthax)

(require "../fjmodels.rkt")
(require "../rosettefjapi.rkt")
(require "grammar.rkt")

(current-bitwidth 6)

;; between 21:30 and 8:00 inclusive, mode is night
;; otherwise, if user is at home, home, else away
(define (mode-graph clockB userLocationB)
  (liftB (λ (clock location) (if (or (>= (time-vec->integer clock) 2130) (< (time-vec->integer clock) 800))
                                 'night
                                 (if (equal? location 'home)
                                     'home
                                     'away)))
         clockB userLocationB))
(define (synth-mode-graph clockB userLocationB)
  (flapjax-grmrB2 clockB userLocationB 3))

;; when motion sensor senses motion, light turns on
(define (kitchen-light-status-graph motionSensorB)
  (liftB (λ (e) (if e 'on 'off)) motionSensorB))
(define (synth-kitchen-light-status-graph motionSensorB)
  (flapjax-grmrB motionSensorB 3))

;; when light turns on, if mode is night, light is orange, else white
(define (kitchen-light-color-graph kitchenLightB modeB)
  (liftB (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none)) kitchenLightB modeB))
(define (synth-kitchen-light-color-graph kitchenLightB modeB)
  (flapjax-grmrB3 kitchenLightB modeB '() 3))

(define (full-kitchen-light-color-graph clockB userLocationB motionSensorB)
  (liftB (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))
         (liftB (λ (e) (if e 'on 'off)) motionSensorB)
         (liftB (λ (clock location) (if (or (>= (time-vec->integer clock) 2130) (< (time-vec->integer clock) 800))
                                 'night
                                 (if (equal? location 'home)
                                     'home
                                     'away)))
         clockB userLocationB)))
(define (synth-full-kitchen-light-color-graph clockB userLocationB motionSensorB)
  (flapjax-grmrB3 clockB userLocationB motionSensorB 4))

;; location behavior symbolic variables
(define-symbolic* init-bool boolean?)
(define-symbolic* timestamp1 integer?)
(define-symbolic* bool1 boolean?)
(define-symbolic* timestamp2 integer?)
(define-symbolic* bool2 boolean?)
(define-symbolic* timestamp3 integer?)
(define-symbolic* bool3 boolean?)

(assert valid-behavior? (behavior init-bool (list (list timestamp1 bool1)
                                                  (list timestamp2 bool2)
                                                  (list timestamp3 bool3))))

;; clock behavior symbolic variables
(define-symbolic* init-hour integer?)
(define-symbolic* init-ten integer?)
(define-symbolic* init-minute integer?)
(define-symbolic* timestamp4 integer?)
(define-symbolic* hour1 integer?)
(define-symbolic* ten1 integer?)
(define-symbolic* minute1 integer?)
(define-symbolic* timestamp5 integer?)
(define-symbolic* hour2 integer?)
(define-symbolic* ten2 integer?)
(define-symbolic* minute2 integer?)
(define-symbolic* timestamp6 integer?)
(define-symbolic* hour3 integer?)
(define-symbolic* ten3 integer?)
(define-symbolic* minute3 integer?)

(assert (valid-time-vec-behavior?
         (behavior (vector init-hour init-ten init-minute)
                   (list (list timestamp4 (vector hour1 ten1 minute1))
                         (list timestamp5 (vector hour2 ten2 minute2))
                         (list timestamp6 (vector hour3 ten3 minute3))))))

;; location behavior symbolic variables
(define-symbolic* init-location boolean?)
(define-symbolic* timestamp7 integer?)
(define-symbolic* location1 boolean?)
(define-symbolic* timestamp8 integer?)
(define-symbolic* location2 boolean?)
(define-symbolic* timestamp9 integer?)
(define-symbolic* location3 boolean?)

(assert (valid-behavior?
         (behavior (if init-location 'home 'not-at-home)
                   (list (list timestamp7 (if location1 'home 'not-at-home))
                         (list timestamp8 (if location2 'home 'not-at-home))
                         (list timestamp9 (if location3 'home 'not-at-home))))))

;; mode behavior symbolic variables
(define-symbolic* init-mode-night boolean?)
(define-symbolic* init-mode-home boolean?)
(define-symbolic* timestamp10 integer?)
(define-symbolic* mode-night1 boolean?)
(define-symbolic* mode-home1 boolean?)
(define-symbolic* timestamp11 integer?)
(define-symbolic* mode-night2 boolean?)
(define-symbolic* mode-home2 boolean?)
(define-symbolic* timestamp12 integer?)
(define-symbolic* mode-night3 boolean?)
(define-symbolic* mode-home3 boolean?)

(assert (valid-behavior?
         (behavior (if init-mode-night 'night (if init-mode-home 'home 'away))
                   (list (list timestamp10 (if mode-night1 'night (if mode-home1 'home 'away)))
                         (list timestamp11 (if mode-night2 'night (if mode-home1 'home 'away)))
                         (list timestamp12 (if mode-night3 'night (if mode-home1 'home 'away)))))))

;; kitchen light symbolic variables
(define-symbolic* init-light boolean?)
(define-symbolic* timestamp13 integer?)
(define-symbolic* light1 boolean?)
(define-symbolic* timestamp14 integer?)
(define-symbolic* light2 boolean?)
(define-symbolic* timestamp15 integer?)
(define-symbolic* light3 boolean?)

(assert (valid-behavior?
         (behavior (if init-light 'on 'off)
                   (list (list timestamp13 (if light1 'on 'off))
                         (list timestamp14 (if light2 'on 'off))
                         (list timestamp15 (if light3 'on 'off))))))

(displayln "Synthesize mode graph")
(define mode-begin-time (current-seconds))
(define mode-binding
  (synthesize #:forall (list timestamp4 hour1 ten1 minute1
                             timestamp5 hour2 ten2 minute2
                             timestamp6 hour3 ten3 minute3
                             timestamp7 location1
                             timestamp8 location2
                             timestamp9 location3
                             init-hour init-ten init-minute
                             init-location
                             )
              #:guarantee (same2 mode-graph synth-mode-graph
                                 (behavior (vector init-hour init-ten init-minute)
                                           (list (list timestamp4 (vector hour1 ten1 minute1))
                                                 (list timestamp5 (vector hour2 ten2 minute2))
                                                 (list timestamp6 (vector hour3 ten3 minute3))))
                                 (behavior (if init-location 'home 'not-at-home)
                                           (list (list timestamp7 (if location1 'home 'not-at-home))
                                                 (list timestamp8 (if location2 'home 'not-at-home))
                                                 (list timestamp9 (if location3 'home 'not-at-home))))
                                 )))
(define mode-end-time (current-seconds))
(if (unsat? mode-binding)
    (displayln "No binding was found.")
    (print-forms mode-binding))
(printf "Took ~a seconds~n" (- mode-end-time mode-begin-time))

(displayln "Synthesize light status graph")

(define light-status-begin-time (current-seconds))
(define light-status-binding
  (synthesize #:forall (list timestamp1 bool1
                             timestamp2 bool2
                             timestamp3 bool3
                             init-bool)
              #:guarantee (same kitchen-light-status-graph synth-kitchen-light-status-graph
                           (behavior init-bool (list (list timestamp1 bool1)
                                                     (list timestamp2 bool2)
                                                     (list timestamp3 bool3)))
              )))
(define light-status-end-time (current-seconds))
(if (unsat? light-status-binding)
    (displayln "No binding was found.")
    (print-forms light-status-binding))
(printf "Took ~a seconds~n" (- light-status-end-time light-status-begin-time))

(displayln "Synthesize partial kitchen light color graph")
(define light-color-partial-begin-time (current-seconds))
(define light-color-partial-binding
  (synthesize #:forall (list ;timestamp4 hour1 ten1 minute1
                             ;timestamp5 hour2 ten2 minute2
                             ;timestamp6 hour3 ten3 minute3
                             ;timestamp7 location1
                             ;timestamp8 location2
                             ;timestamp9 location3
                             ;init-hour init-ten init-minute
                             ;init-location
                             ;timestamp1 bool1
                             ;timestamp2 bool2
                             ;timestamp3 bool3
                             ;init-bool
                        init-mode-night init-mode-home
                        timestamp10 mode-night1 mode-home1
                        timestamp11 mode-night2 mode-home2
                        timestamp12 mode-night3 mode-home3
                        init-light
                        timestamp13 light1
                        timestamp14 light2
                        timestamp15 light3
                             )
              #:guarantee (same2 kitchen-light-color-graph synth-kitchen-light-color-graph
                                 #;(mode-graph (behavior (vector init-hour init-ten init-minute)
                                           (list (list timestamp4 (vector hour1 ten1 minute1))
                                                 (list timestamp5 (vector hour2 ten2 minute2))
                                                 (list timestamp6 (vector hour3 ten3 minute3))))
                                 (behavior (if init-location 'home 'not-at-home)
                                           (list (list timestamp7 (if location1 'home 'not-at-home))
                                                 (list timestamp8 (if location2 'home 'not-at-home))
                                                 (list timestamp9 (if location3 'home 'not-at-home)))))
                                 (behavior (if init-mode-night 'night (if init-mode-home 'home 'away))
                                           (list (list timestamp10 (if mode-night1 'night (if mode-home1 'home 'away)))
                                                 (list timestamp11 (if mode-night2 'night (if mode-home1 'home 'away)))
                                                 (list timestamp12 (if mode-night3 'night (if mode-home1 'home 'away)))))
                                 #;(kitchen-light-status-graph
                                  (behavior init-bool (list (list timestamp1 bool1)
                                                            (list timestamp2 bool2)
                                                            (list timestamp3 bool3))))
                                 (behavior (if init-light 'on 'off)
                                           (list (list timestamp13 (if light1 'on 'off))
                                                 (list timestamp14 (if light2 'on 'off))
                                                 (list timestamp15 (if light3 'on 'off)))))))
(define light-color-partial-end-time (current-seconds))
(if (unsat? light-color-partial-binding)
    (displayln "No binding was found.")
    (print-forms light-color-partial-binding))
(printf "Took ~a seconds~n" (- light-color-partial-end-time light-color-partial-begin-time))

(displayln "Synthesize kitchen light color graph")
(define light-color-begin-time (current-seconds))
(define light-color-binding
  (synthesize #:forall (list timestamp4 hour1 ten1 minute1
                             timestamp5 hour2 ten2 minute2
                             timestamp6 hour3 ten3 minute3
                             timestamp7 location1
                             timestamp8 location2
                             timestamp9 location3
                             init-hour init-ten init-minute
                             init-location
                             timestamp1 bool1
                             timestamp2 bool2
                             timestamp3 bool3
                             init-bool
                             )
              #:guarantee (same3 full-kitchen-light-color-graph synth-full-kitchen-light-color-graph
                                 (behavior (vector init-hour init-ten init-minute)
                                                       (list (list timestamp4 (vector hour1 ten1 minute1))
                                                             (list timestamp5 (vector hour2 ten2 minute2))
                                                             (list timestamp6 (vector hour3 ten3 minute3))))
                                 (behavior (if init-location 'home 'not-at-home)
                                           (list (list timestamp7 (if location1 'home 'not-at-home))
                                                 (list timestamp8 (if location2 'home 'not-at-home))
                                                 (list timestamp9 (if location3 'home 'not-at-home))))
                                 (behavior init-bool (list (list timestamp1 bool1)
                                                           (list timestamp2 bool2)
                                                           (list timestamp3 bool3))))))
(define light-color-end-time (current-seconds))
(if (unsat? light-color-binding)
    (displayln "No binding was found.")
    (print-forms light-color-binding))
(printf "Took ~a seconds~n" (- light-color-end-time light-color-begin-time))
                                             
