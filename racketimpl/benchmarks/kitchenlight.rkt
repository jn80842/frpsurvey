#lang rosette/safe

(require "../rosettefjapi.rkt")

(current-bitwidth 5)

(define b-clock (behavior 0 (list (list 1 2000) (list 2 2021) (list 3 2025) (list 4 2027) (list 5 2032) (list 6 2045) (list 7 2130) (list 8 2345) (list 9 2350) (list 10 805)
                          (list 11 2000) (list 12 2129) (list 13 2130) (list 14 2202) (list 15 2215) (list 16 2221) (list 17 2226))))
(define b-location (behavior 'not-at-home (list (list 1 'not-at-home) (list 2 'home) (list 3 'home) (list 4 'home) (list 5 'home) (list 6 'home) (list 7 'home) (list 8 'home) (list 9 'home)
                             (list 10 'not-at-home) (list 11 'not-at-home) (list 12 'not-at-home) (list 13 'not-at-home) (list 14 'home) (list 15 'home) (list 16 'home) (list 17 'home))))
(define b-motion-sensor (behavior #f (list (list 3 #t) (list 5 #f) (list 8 #t) (list 9 #f) (list 15 #t) (list 17 #f))))

(define b-mode (behavior 'night (list (list 1 'away) (list 2 'home) (list 3 'home) (list 4 'home) (list 5 'home) (list 6 'home) (list 7 'night) (list 8 'night) (list 9 'night)
                                      (list 10 'away) (list 11 'away) (list 12 'away) (list 13 'night) (list 14 'night) (list 15 'night) (list 16 'night) (list 17 'night))))
(define b-light-status (behavior 'off (list (list 3 'on) (list 5 'off) (list 8 'on) (list 9 'off) (list 15 'on) (list 17 'off))))
(define b-light-color (behavior 'none (list (list 3 'white) (list 5 'none) (list 8 'orange) (list 9 'none) (list 15 'orange) (list 17 'none))))

;; between 21:30 and 8:00 inclusive, mode is night
;; otherwise, if user is at home, home, else away
(define (mode-graph clockB userLocationB)
  (liftB (λ (clock location) (if (or (>= clock 2130) (< clock 800))
                                 'night
                                 (if (equal? location 'home)
                                     'home
                                     'away)))
         clockB userLocationB))

;; when motion sensor senses motion, light turns on
(define (kitchen-light-status-graph motionSensorB)
  (liftB (λ (e) (if e 'on 'off)) motionSensorB))

;; when light turns on, if mode is night, light is orange, else white
(define (kitchen-light-color-graph kitchenLightB modeB)
  (liftB (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none)) kitchenLightB modeB))

;; check input/output pairs
(printf "checking graphs: mode ~a, light status ~a, light color ~a~n"
 (equal-behaviors? (mode-graph b-clock b-location) b-mode)
 (equal-behaviors? (kitchen-light-status-graph b-motion-sensor) b-light-status)
 (equal-behaviors? (kitchen-light-color-graph (kitchen-light-status-graph b-motion-sensor) (mode-graph b-clock b-location)) b-light-color))