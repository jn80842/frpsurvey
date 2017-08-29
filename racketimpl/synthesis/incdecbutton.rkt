#lang rosette/safe
(require rosette/lib/synthax)

(require "../fjmodels.rkt")
(require "../rosettefjapi.rkt")
(require "../benchmarks/incdecbutton.rkt")
(require "grammar.rkt")


;;         startsWith
;;          /       \
;;      collectE     0
;;         /   \
;;      mergeE  λ
;;     /      \
;;  constantE constantE
;;   /     \    /     \
;;  inc    1   dec    -1

(define-synthax (flapjax-grmr input ... depth)
  #:base (choose input ...)
  #:else (choose input ...
                 ((choose startsWith constantE) (??) (flapjax-grmr input ... (sub1 depth)))
                 (collectE (??) + (flapjax-grmr input ... (sub1 depth)))
                 (mergeE (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)))))

#;(define-synthax (flapjax-grmr input ... depth)
  #:base (choose input ...)
  #:else (choose input ...
                 ;; zero arity
                 (zeroE)
                 (constantB (choose 'on 'off (??)))
                 ;; E ::= arity-1-op val E
                 ((choose startsWith constantE delayE blindE calmE mapE)
                  (choose (??)
                          (λ (e) (if e 'on 'off))
                          (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))
                          (λ (t) (<= t (??)))
                          (λ (c) (or (>= (vector-ref c 0) (??)) (>= (??) (vector-ref c 0))))
                          (λ (e) (list (get-timestamp e) (+ (get-value e) (??)))))
                  (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= arity-2-op E E
                 (mergeE (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)))
              ;   (liftB (choose (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))
              ;                  (λ (clock location) (if (or (>= (time-vec->integer clock) 2130) (< (time-vec->integer clock) 800))
              ;                   'night
              ;                   (if (equal? location 'home)
              ;                       'home
              ;                       'away))))
              ;          (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)))
                                
                                
                 ))

(define (inc-dec-button-graph inc dec)
 (startsWith 0 (collectE 0 +
   (mergeE (constantE 1 inc) (constantE -1 dec)))))

(define (merge-constants-graph inc dec)
  (mergeE (constantE 1 inc) (constantE -1 dec)))
#;(define (synth-merge-constants-graph inc dec)
  (flapjaxE-grmr2 inc dec 2))

(define (collect-graph evt-stream)
  (startsWith 0 (collectE 0 + evt-stream)))
(define (synth-collect-graph evt-stream)
  (flapjax-grmr evt-stream 3))

(define (outer-graph inc dec)
  (startsWith 0 (collectE 0 + (mergeE (flapjax-grmr inc 2) (constantE -1 dec)))))

(define (synth-inc-dec-button-graph inc dec)
  (flapjax-grmr inc dec 4))

(define stream-length 3)

(define s-inc (new-event-stream (sym-union-constructor 'click 'no-evt) stream-length))
(define s-dec (new-event-stream (sym-union-constructor 'click 'no-evt) stream-length))

(assert (button-assumptions s-inc s-dec))
(assert (andmap (λ (e) (>= (* 2 stream-length) (get-timestamp e))) s-inc))
(assert (andmap (λ (e) (>= (* 2 stream-length) (get-timestamp e))) s-dec))

(displayln "Synthesize inc/dec button program:")

(displayln "Synthesize two merged streams with 1 and -1 constants")

(define begin-time (current-seconds))
#;(define binding
    (synthesize #:forall (append (harvest-events s-inc) (harvest-events s-dec))
                #:guarantee (assert (same merge-constants-graph synth-merge-constants-graph
                                  s-inc s-dec))))  
#;(if (unsat? binding)
    (displayln "No binding was found.")
    (print-forms binding))
(define end-time (current-seconds))
(printf "Took ~a seconds~n" (- end-time begin-time))

(define-symbolic* timestamp1 integer?)
(define-symbolic* timestamp2 integer?)
(define-symbolic* timestamp3 integer?)
(assert (valid-timestamps? (list (list timestamp1 'a) (list timestamp2 'b) (list timestamp3 'c))))

(displayln "Synthesize startsWith and collectE portion")
(define collect-begin-time (current-seconds))
#;(define collect-binding
  (synthesize #:forall (list timestamp1 timestamp2 timestamp3)
              #:guarantee (assert (same collect-graph synth-collect-graph
                                (list (list timestamp1 1)
                                      (list timestamp2 -1)
                                      (list timestamp3 1))
                                        ))
              ))
(define collect-end-time (current-seconds))
#;(if (unsat? collect-binding)
    (displayln "No binding was found.")
    (print-forms collect-binding))
(printf "Took ~a seconds~n" (- collect-end-time collect-begin-time))

(displayln "Synthesize full program")
(define full-begin-time (current-seconds))
(define full-binding
  (synthesize #:forall (append (harvest-events s-inc) (harvest-events s-dec))
              #:guarantee (assert (same inc-dec-button-graph synth-inc-dec-button-graph
                                 s-inc s-dec))))
(define full-end-time (current-seconds))
(if (unsat? full-binding)
    (displayln "No binding was found.")
    (print-forms full-binding))
(printf "Took ~a seconds~n" (- full-end-time full-begin-time))

