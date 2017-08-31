#lang rosette/safe

(require rosette/lib/synthax)
(require "../fjmodels.rkt")
(require "../rosettefjapi.rkt")
(require "../benchmarks/draganddrop.rkt")
(require "grammar.rkt")

;;                startsWith
;;                /         \
;;             init-pos    switchE
;;                           |
;;                         mergeE
;;                      /          \
;;                 mapE               mapE
;;             /       \            /       \
;; (λ) (mouse-pos) mouse-down   (λ) (zeroE) mouse-up

(printf "Current bitwidth: ~a~n" (current-bitwidth))

(define-synthax (flapjax-grmr input ... depth)
  #:base (choose input ...)
  #:else (choose input ...
                 ;; zero arity
                 (zeroE)
                 ;(constantB (choose 'on 'off (??)))
                 ;; E ::= arity-1-op E
                 (;(choose oneE
                   switchE
                  ; notE
                   ;changes
                   ;)
                 (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= arity-1-op val E
                 ((choose
                   startsWith
                  ; constantE
                  ; delayE
                  ; blindE
                  ; calmE
                   mapE
                  ; filterE
                  ; liftB
                   )
                  (choose ;(??)
                          ;'on 'off
                          ;(λ (e) (if e 'on 'off))
                          ;(λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))
                          ;(λ (t) (<= t (??)))
                          ;(λ (c) (or (>= (vector-ref c 0) (??)) (>= (??) (vector-ref c 0))))
                          ;(λ (e) (list (get-timestamp e) (+ (get-value e) (??))))
                   (λ (e) 
                     (list (get-timestamp e)
                           (startBehaviorAtTimestamp (get-timestamp e) (choose input ...))))
                   (λ (e)
                     (list (get-timestamp e) (zeroE)))
                          )
                  (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= collectE init λ E
                 (collectE (??) + (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= arity-2-op E E
                 (
                  ;(choose andB
                          mergeE
                   ;       snapshotE
                   ;       )
                 (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= liftB λ E E
                 ;(liftB (choose (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))
                 ;               (λ (clock location) (if (or (>= (time-vec->integer clock) 2130) (< (time-vec->integer clock) 800))
                 ;                'night
                 ;                (if (equal? location 'home)
                 ;                    'home
                 ;                    'away))))
                 ;       (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= arity-3-op E E E               
                 ;((choose ifE
                 ;         ifB
                 ;         ) (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)))          
                 ))                 

(define (moveEe mouse-downE mouse-posB)
  (mapE (λ (e) 
          (list (get-timestamp e)
                (startBehaviorAtTimestamp (get-timestamp e) mouse-posB)))
        mouse-downE))

(define (dropEe upE)
  (mapE (λ (e) (list (get-timestamp e) (zeroE))) upE))
(define (dragE moveEe dropEe)
  (switchE (mergeE moveEe dropEe)))

(define (elt-positionB mouse-up mouse-down mouse-pos init-pos)
  (startsWith (dragE (moveEe mouse-down mouse-pos) (dropEe mouse-up)) init-pos))

(define (synth-graph mouse-up mouse-down mouse-pos init-pos)
  (flapjax-grmr mouse-up mouse-down mouse-pos init-pos 3))

(define s-mouse-up (symbolic-click-event-stream 'up stream-length))
(define s-mouse-down (symbolic-click-event-stream 'down stream-length))
(define s-mouse-pos (new-behavior sym-coords stream-length))
(define s-init-elt-pos (sym-coords))

(assert (drag-and-drop-assumptions s-mouse-up s-mouse-down s-mouse-pos s-init-elt-pos))

(define begin-time (current-seconds))
(define binding
    (synthesize #:forall (append (harvest-events s-mouse-up) (harvest-events s-mouse-down) (harvest-behavior s-mouse-pos) (harvest-term s-init-elt-pos))
                #:guarantee (assert (same elt-positionB synth-graph s-mouse-up s-mouse-down s-mouse-pos s-init-elt-pos))))
(if (unsat? binding)
    (displayln "No binding was found.")
    (print-forms binding))
(define end-time (current-seconds))
(printf "Took ~a seconds~n" (- end-time begin-time))
