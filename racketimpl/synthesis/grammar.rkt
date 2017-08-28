#lang rosette/safe

(require rosette/lib/synthax)
 (require rosette/lib/angelic)
(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")

(provide (all-defined-out))

(define  (same program1 program2 . inputs)
  (equal? (apply program1 inputs)
                  (apply program2 inputs)))

(define (harvest-term v)
  (cond [(vector? v) (vector->list v)]
        [(and (union? v) (eq? 2 (length (union-contents v)))) (car (first (union-contents v)))]
        [(term? v) v]))

(define (harvest-events evt-stream)
  (flatten
  (append (map get-timestamp evt-stream)
          (map harvest-term (map get-value evt-stream)))))

(define (harvest-behavior b)
  (flatten (append (list (harvest-term (behavior-init b))) (harvest-events (behavior-changes b)))))

(define-synthax (flapjax-grmr input ... depth)
  #:base (choose input ...)
  #:else (choose input ...
                 ;; zero arity
                 (zeroE)
                 (constantB (choose 'on 'off (??)))
                 ;; E ::= arity-1-op E
                 ((choose oneE switchE notE notB changes) (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= arity-1-op val E
                 ((choose startsWith constantE delayE blindE calmE mapE filterE liftB)
                  (choose (??)
                          (λ (e) (if e 'on 'off))
                          (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))
                          (λ (t) (<= t (??)))
                          (λ (c) (or (>= (vector-ref c 0) (??)) (>= (??) (vector-ref c 0))))
                          (λ (e) (list (get-timestamp e) (+ (get-value e) (??)))))
                  (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= collectE init λ E
                 (collectE (??) + (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= arity-2-op E E
                 ((choose andB mergeE snapshotE) (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= liftB λ E E
                 (liftB (choose (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))
                                (λ (clock location) (if (or (>= (time-vec->integer clock) 2130) (< (time-vec->integer clock) 800))
                                 'night
                                 (if (equal? location 'home)
                                     'home
                                     'away))))
                        (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= arity-3-op E E E               
                 ((choose ifE ifB) (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)))          
                 )) 

#;(define-synthax (flapjaxE-grmr input-stream ... depth)
  #:base (choose input-stream  ... )
  #:else (choose input-stream  ...
                (startsWith (flapjaxE-grmr input-stream ... (sub1 depth)) (??))
                 (collectE (flapjaxE-grmr input-stream ... (sub1 depth)) (??) +)
                 (mapE (λ (e) (list (get-timestamp e) (+ (get-value e) (??))))
                       (flapjaxE-grmr input-stream ... (sub1 depth)))
                 (constantE (flapjaxE-grmr input-stream ... (sub1 depth)) (??))
                 (delayE (flapjaxE-grmr input-stream ... (sub1 depth)) (??))
                 (mergeE (flapjaxE-grmr input-stream ... (sub1 depth)) (flapjaxE-grmr input-stream ... (sub1 depth)))))
