#lang racket

;; Example Task 1

;; User wants to write a program to control the porchlight.
;; When they enter their house (as determined by a location service
;; on their phone), they want the porchlight to turn on, and when
;; they leave their garage, they want their porchlight to turn on.

;; The input to the program is a behavior holding the user location state.
;; The output of the program is an event stream that will control the porchlight

;; Here is an attempt to write that program
(define (porchlight-graph userLocationB)
  (mergeE
   (changes (collectB (λ (prev current) (if (and (eq? current 'house)
                                                     (not (eq? current 'house))) 'on 'off)) userLocationB))
   (changes (collectB (λ (prev current) (if (and (not (eq? current 'garage))
                                                     (eq? current 'garage))) 'off 'on) userLocationB))))

;; However, this program has a bug: if the user leaves their garage and directly enters their home,
;; the porchlight will be turned on and off at the same timestep.

;; Instead, the user provides two sample input/output pairs for the desired program:
'(house house away away work)
'(no-evt no-evt off no-evt no-evt)

'(house house garage away away)
'(no-evt no-evt no-evt off no-evt)

;; Given this spec, the synthesizer can produce more than one unique program, so it
;; gives the user an input that can disambiguate the candidate programs

'(work away away garage house)

;; The user supplies the desired output

'(no-evt no-evt no-evt no-evt on)

;; And the new input/output pair is added to the spec. 

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

(assert (globally (implies (motion-sensor-on? ms) (outdoor-light-on? ol))))
(assert (globally (implies (and (not (motion-sensor-on? ms))
                                (not (motion-sensor-on? (next ms)))
                                (not (motion-sensor-on? (next (next ms)))))
                           (not (outdoor-light-on? ol)))))

