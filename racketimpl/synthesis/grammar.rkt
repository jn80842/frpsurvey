#lang rosette/safe

(require rosette/lib/synthax)
 (require rosette/lib/angelic)
(require "../rosettefjapi.rkt")
(require "../fjmodels.rkt")

(provide (all-defined-out))

(define-synthax (flapjax-grmr input ... depth)
  #:base (choose input ...)
  #:else (let ([recursive-call1 (flapjax-grmr input ... (sub1 depth))]
               [recursive-call2 (flapjax-grmr input ... (sub1 depth))]
               [recursive-call3 (flapjax-grmr input ... (sub1 depth))])
           (choose input ...
                   (zeroE)
                   (constantB (choose 'on 'off (??)))
                   (oneE recursive-call1)
                   (switchE recursive-call1)
                   (notE recursive-call1)
                   (changes recursive-call1)
                   (startsWith (??) recursive-call1)
                   (constantE (??) recursive-call1)
                   (delayE (??) recursive-call1)
                   (blindE (??) recursive-call1)
                   (calmE (??) recursive-call1)
                   (mapE (choose (λ (e) (list (get-timestamp e) (+ (get-value e) (??))))
                                 (λ (e) (list (get-timestamp e)
                                              (startBehaviorAtTimestamp (get-timestamp e) (choose input ...))))
                                 (λ (e) (list (get-timestamp e) (zeroE)))) recursive-call1)
                   (filterE (choose (λ (t) (<= t (??)))
                                    (λ (c) (or (>= (vector-ref c 0) (??)) (>= (??) (vector-ref c 0))))) recursive-call1)
                   (liftB (λ (e) (if e 'on 'off)) recursive-call1)
                   (collectE (??) + recursive-call1)
                   (andB recursive-call1 recursive-call2)
                   (mergeE recursive-call1 recursive-call2)
                   (snapshotE recursive-call1 recursive-call2)
                   (liftB (choose (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))
                                  (λ (clock location) (if (or (>= (time-vec->integer clock) 2130) (< (time-vec->integer clock) 800))
                                                          'night
                                                          (if (equal? location 'home)
                                                              'home
                                                              'away)))) recursive-call1 recursive-call2)
                   (ifE recursive-call1 recursive-call2 recursive-call3)
                   (ifB recursive-call1 recursive-call2 recursive-call3))))

#;(define-synthax (flapjax-grmr input ... depth)
  #:base (choose input ...)
  #:else (choose input ...
                 ;; zero arity
                 (zeroE)
                 (constantB (choose 'on 'off (??)))
                 ;; E ::= arity-1-op E
                 ((choose oneE
                   switchE
                   notE
                   changes
                   ) (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= arity-1-op val E
                 ((choose
                   startsWith
                   constantE
                   delayE
                   blindE
                   calmE
                   mapE
                   filterE
                   liftB
                   )
                  (choose (??)
                          'on 'off
                          (λ (e) (if e 'on 'off))
                          (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))
                          (λ (t) (<= t (??)))
                          (λ (c) (or (>= (vector-ref c 0) (??)) (>= (??) (vector-ref c 0))))
                          (λ (e) (list (get-timestamp e) (+ (get-value e) (??))))
                          (λ (e) (list (get-timestamp e)
                           (startBehaviorAtTimestamp (get-timestamp e) (choose input ...))))
                          (λ (e) (list (get-timestamp e) (zeroE)))
                          )
                  (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= collectE init λ E
                 (collectE (??) + (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= arity-2-op E E
                 ((choose andB
                          mergeE
                          snapshotE
                          ) (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= liftB λ E E
                 (liftB (choose (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))
                                (λ (clock location) (if (or (>= (time-vec->integer clock) 2130) (< (time-vec->integer clock) 800))
                                 'night
                                 (if (equal? location 'home)
                                     'home
                                     'away))))
                        (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)))
                 ;; E ::= arity-3-op E E E               
                 ((choose ifE
                          ifB
                          ) (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)) (flapjax-grmr input ... (sub1 depth)))          
                 )) 


#;(define-synthax (flapjax-grmr input ... depth)
  #:base (choose input ...)
  #:else (let ([recursive-call1 (flapjax-grmr input ... (sub1 depth))]
               [recursive-call2 (flapjax-grmr input ... (sub1 depth))]
               [recursive-call3 (flapjax-grmr input ... (sub1 depth))])
           (choose input ...
                   ;; zero arity
                   (zeroE)
                   (constantB (choose 'on 'off (??)))
                   ;; E ::= arity-1-op E
                   ((choose oneE
                            switchE
                            notE
                            changes
                   ) recursive-call1)
                 ;; E ::= arity-1-op val E
                 ((choose
                   startsWith
                   constantE
                   delayE
                   blindE
                   calmE
                   mapE
                   filterE
                   liftB
                   )
                  (choose (??)
                          'on 'off
                          (λ (e) (if e 'on 'off))
                          (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))
                          (λ (t) (<= t (??)))
                          (λ (c) (or (>= (vector-ref c 0) (??)) (>= (??) (vector-ref c 0))))
                          (λ (e) (list (get-timestamp e) (+ (get-value e) (??))))
                          (λ (e) (list (get-timestamp e)
                           (startBehaviorAtTimestamp (get-timestamp e) (choose input ...))))
                          (λ (e) (list (get-timestamp e) (zeroE)))
                          )
                  recursive-call1)
                 ;; E ::= collectE init λ E
                 (collectE (??) + recursive-call1)
                 ;; E ::= arity-2-op E E
                 ((choose andB
                          mergeE
                          snapshotE
                          ) recursive-call1 recursive-call2)
                 ;; E ::= liftB λ E E
                 (liftB (choose (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))
                                (λ (clock location) (if (or (>= (time-vec->integer clock) 2130) (< (time-vec->integer clock) 800))
                                 'night
                                 (if (equal? location 'home)
                                     'home
                                     'away))))
                        recursive-call1 recursive-call2)
                 ;; E ::= arity-3-op E E E
                 ((choose ifE
                          ifB
                          ) recursive-call1 recursive-call2 recursive-call3)
                 )))
