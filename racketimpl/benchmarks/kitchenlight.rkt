#lang rosette/safe

(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")

(current-bitwidth 6)

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

(define (location-behavior concrete-list)
  (define-symbolic* init-is-home? boolean?)
  (behavior (if init-is-home? 'home 'not-at-home) (map (λ (x)
                                                         (define-symbolic* timestamp integer?)
                                                         (define-symbolic* is-home? boolean?)
                                                         (list timestamp (if is-home? 'home 'not-at-home))) concrete-list)))

(define s-motion-sensor (boolean-behavior (list 1)))
(define s-location (location-behavior (list 1)))
(define s-clock (time-vec-behavior (list 1)))
       
(define (mode-assumptions clockB locationB)
  (and (valid-behavior? clockB)
       (valid-time-vec? (behavior-init clockB))
       (andmap (λ (e) (valid-time-vec? (get-value e))) (behavior-changes clockB))
       (valid-behavior? locationB)
       ))

(define solved (solve (assert (mode-assumptions s-clock s-location))))

(if (unsat? solved)
    (displayln "no solution for assumptions")
    (begin
      (displayln "sample solution for assumptions:")
      (displayln (evaluate s-clock solved))
      (displayln (evaluate s-location solved))))

(define (if-home-then-home-or-night loc mode)
  (or (not (eq? 'home loc)) (or (eq? mode 'home) (eq? mode 'night))))

(define (mode-guarantees clockB locationB modeB)
  (let ([unique-ts (all-unique-timestamps clockB locationB modeB)])
  (and (if-home-then-home-or-night (behavior-init locationB) (behavior-init modeB))
     ;  (andmap if-home-then-home-or-night (project-values locationB unique-ts) (project-values modeB unique-ts))
       )))

(displayln "Verify mode spec")
(define begin-time (current-seconds))

(define verified (verify
                  #:assume (assert (mode-assumptions s-clock s-location))
                  #:guarantee (assert (mode-guarantees s-clock s-location (mode-graph s-clock s-location)))))

(define end-time (current-seconds))
(printf "Verification took ~a seconds~n" (- end-time begin-time))