#lang racket

;; Example Task 1

;; The user wants to write a program that will turn on a light at
;; 6am, then turn it off after sunrise.

;; They write the following program that takes an input an event stream
;; representing minute ticks of a clock and an event stream that sends a signal
;; at sunrise each day, and outputs an event stream of commands to issue to
;; the light.

(define (early-morning-lightE clockE sunriseE)
  (mergeE (constantE 'off sunriseE)
          (constantE 'on (filterE (λ (c) (is-six-am? c)) clockE))))

;; Several months later, the user comes home and discovers that the light
;; has been on all day.

;; Instead, imagine the user has specified their desired program by
;; giving the following input and output pair (assume the clock event stream
;; need not specify every possible time for any given interval):

'(5:59   6:00   6:30   7:21    8:00)
'(no-evt no-evt no-evt sunrise no-evt)
'(no-evt on     no-evt off     no-evt)

;; There are many possible programs that can produce that output, so the synthesizer
;; finds a disambiguating input and asks the user what the outcome should be.

'(5:30   5:49    6:00   6:30)
'(no-evt sunrise no-evt no-evt)

;; In this case, in which sunrise occurs before 6am, the user doesn't want
;; the light to turn on at all.

'(no-evt no-evt no-evt no-evt)

;; Through these types of interactions, the synthesizer eventually produces
;; a correct program:

(define (early-morning-lightE clockE sunriseE)
  (let ([seen-todays-sunriseB (startsWith #f (mergeE (constantE #f (filter (λ (c) (is-midnight? c)) clockE))
                                                     (constantE #t sunriseE)))])
    (filterRepeatsE (mergeE (constantE 'off sunriseE)
                            (constantE 'on (filterE (notE (snapshotE seen-todays-sunriseB
                                                                     (filterE (λ (c) (is-six-am? c))
                                                                              clockE)))))))))

;; Example Task 2

;; User wants to write a program to control an outdoor light with a motion sensor.
;; When the motion sensor detects motion, the light should turn on and turn off again
;; three minutes later

;; Here is an attempt to write that program:
(define (outdoor-light-graph motionSensorE)
  (mergeE (constantE 'on motionSensorE) (constantE 'off (delayE 3 motionSensorE))))

;; This program has undesired behavior: if one person moves past the sensor, the light will
;; turn on; if another person moves past the sensor 2 minutes later, the light will turn off
;; 3 minutes after the original motion sensor signal, potentially leaving the second person in
;; the dark.

;; In this case, it may be easier for the user to state properties the program should have than
;; to write the program itself. Here we state the English-language spec above in LTL formulas.

;(assert (globally (implies (motion-sensor-on? ms) (next (outdoor-light-on? ol)))))
(assert (globally (implies (and (not (motion-sensor-on? ms))
                                (not (motion-sensor-on? (next ms)))
                                (not (motion-sensor-on? (next (next ms)))))
                           (next (next (next (not (outdoor-light-on? ol))))))))

