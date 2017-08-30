#lang rosette/safe

(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")
(require "../benchmarks/kitchenlight.rkt")

(define b-clock (behavior (integer->time-vec 0) (list (list 1 (integer->time-vec 2000)) (list 2 (integer->time-vec 2021)) (list 3 (integer->time-vec 2025)) (list 4 (integer->time-vec 2027))
                                  (list 5 (integer->time-vec 2032)) (list 6 (integer->time-vec 2045)) (list 7 (integer->time-vec 2130)) (list 8 (integer->time-vec 2345))
                                  (list 9 (integer->time-vec 2350)) (list 10 (integer->time-vec 805)) (list 11 (integer->time-vec 2000)) (list 12 (integer->time-vec 2129))
                                  (list 13 (integer->time-vec 2130)) (list 14 (integer->time-vec 2202)) (list 15 (integer->time-vec 2215)) (list 16 (integer->time-vec 2221))
                                  (list 17 (integer->time-vec 2226)))))
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
  (liftB (λ (clock location) (if (or (>= (time-vec->integer clock) 2130) (< (time-vec->integer clock) 800))
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

(define (sym-location)
  (define-symbolic* b boolean?)
  (if b 'home 'not-at-home))

(define stream-length 6)

(define s-motion-sensor (new-behavior sym-boolean stream-length))
(define s-location (new-behavior sym-location stream-length))
(define s-clock (new-behavior sym-time-vec stream-length))

(printf "current bitwidth ~a, maximum possible value is ~a~n"
        (current-bitwidth) (max-for-current-bitwidth (current-bitwidth)))
(printf "length of motion sensor changes ~a~n" (length (changes s-motion-sensor)))
(printf "length of location changes ~a~n" (length (changes s-location)))
(printf "length of clock changes ~a~n" (length (changes s-clock)))

(check-existence-of-solution light-color-assumptions s-clock s-location s-motion-sensor)

(define (if-home-then-home-or-night loc mode)
  (or (not (eq? 'home loc)) (or (eq? mode 'home) (eq? mode 'night))))
(define (if-night-then-night clock mode)
  (or (not (or (>= (time-vec->integer clock) 2130) (< (time-vec->integer clock) 800))) (eq? mode 'night)))

(define (mode-guarantees clockB locationB)
  (let* ([modeB (mode-graph clockB locationB)]
         [unique-ts (all-unique-timestamps clockB locationB modeB)]
         [enhanced-clockB (projected-behavior clockB unique-ts)]
         [enhanced-locationB (projected-behavior locationB unique-ts)]
         [enhanced-modeB (projected-behavior modeB unique-ts)])
    (and (if-home-then-home-or-night (behavior-init locationB) (behavior-init modeB))
         (andmap if-home-then-home-or-night enhanced-locationB enhanced-modeB)
         (if-night-then-night (behavior-init clockB) (behavior-init modeB))
         (andmap if-night-then-night enhanced-clockB enhanced-modeB)
         )))

(define (light-status-guarantees motionSensorB)
  (let ([lightStatusB (kitchen-light-status-graph motionSensorB)])
    (and (valid-behavior? lightStatusB)
         (behavior-check (λ (v) (or (eq? 'on v) (eq? 'off v))) lightStatusB)
         (eq? (length (filter (λ (v) (get-value v)) (changes motionSensorB)))
              (length (filter (λ (v) (eq? 'on (get-value v))) (changes lightStatusB)))))))

(define (light-color-guarantees clockB locationB motionSensorB)
  (let* ([colorB (kitchen-light-color-graph (kitchen-light-status-graph motionSensorB)
                                            (mode-graph clockB locationB))]
         [unique-ts (all-unique-timestamps clockB locationB motionSensorB colorB)]
         [enhanced-clockB (projected-behavior clockB unique-ts)]
         [enhanced-locationB (projected-behavior locationB unique-ts)]
         [enhanced-motionSensorB (projected-behavior motionSensorB unique-ts)]
         [enhanced-colorB (projected-behavior colorB unique-ts)])
    (and (valid-behavior? colorB)
         (behavior-check (λ (clock color) (or (not (or (>= (time-vec->integer clock) 2130) (< (time-vec->integer clock) 800)))
                                             (or (eq? color 'none) (eq? color 'orange)))) enhanced-clockB enhanced-colorB)
         (behavior-check (λ (clock color) (or (or (>= (time-vec->integer clock) 2130) (< (time-vec->integer clock) 800))
                                             (or (eq? color 'none) (eq? color 'white)))) enhanced-clockB enhanced-colorB)
         (behavior-check (λ (motion color) (or (not motion) (eq? color 'none))) motionSensorB colorB)
         )))

(displayln "Verify mode spec")
(define begin-time (current-seconds))

(define verified (verify
                  #:assume (assert (mode-assumptions s-clock s-location))
                  #:guarantee (assert (mode-guarantees s-clock s-location (mode-graph s-clock s-location)))))

(define end-time (current-seconds))
(printf "Verification took ~a seconds~n" (- end-time begin-time))