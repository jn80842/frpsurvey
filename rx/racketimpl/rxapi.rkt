#lang rosette

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   Helper methods
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (persistent-stream stream)
  (for/list ([i (range (length stream))])
    (foldl (λ (n m) (if (empty? n) m n)) '() (take stream (add1 i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   Observable constructors
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; waits for all observables to emit at least once
;; thereafter, when any observable emits, send it with the most recent values of the others
(define (combineLatestO . streams)
  (let ([expanded-streams (map persistent-stream streams)])
    (for/list ([i (range (length (first streams)))])
      (let ([inputs (map (λ (e) (list-ref e i)) streams)]
            [expanded (map (λ (e) (list-ref e i)) expanded-streams)])
        (if (or (ormap empty? expanded)
                (andmap empty? inputs))
            '()
            (flatten expanded))))))

(define (fromEventO events)
  events)

(define (intervalO n lst-len)
  (let* ([add-bet-val (for/list ([i (range (sub1 n))]) '())]
         [steps (range n lst-len n)]
         [suffix (for/list ([i (range (- lst-len (* n (length steps))))]) '())])
    (append add-bet-val
            (add-between (for/list ([i (range 1 (add1 (length steps)))]) (list i))
                         add-bet-val #:splice? #t)
            suffix)))

;; merge can take a variable number of streams
;; merge takes an optional arg to limit how many streams to feed from concurrently
(define (mergeO stream1 stream2)
  (map (λ (e1 e2) (if (not (empty? e1)) e1 e2)) stream1 stream2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   Observable methods
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; dispose

(define (distinctUntilChangedO evt-stream)
  (letrec ([f (λ (evt rest)
                (cond [(empty? rest) evt]
                      [(equal? evt (first rest)) '()]
                      [(not (empty? (first rest))) evt]
                      [else (f evt (cdr rest))]))])
    (for/list ([i (range 1 (add1 (length evt-stream)))])
      (let ([lst (take evt-stream i)])
        (if (empty? (last lst))
            '()
            (f (last lst) (cdr (reverse lst))))))))

(define (mapO proc . streams)
  (apply (curry map (λ es (let ([inputs (flatten es)])
                            (list (apply proc inputs))))) streams))

(define (sampleO interval stream)
  (let ([steps (range interval (length stream) interval)])
    (for/list ([i (range (length stream))])
      (if (member (add1 i) steps)
          (list-ref stream i)
          '()))))

(define (scanO-no-seed proc stream)
  (let ([init-val (first stream)]
        [rest-val (cdr stream)])
    (append (list init-val)
            (for/list ([i (range (length rest-val))])
              (if (empty? (list-ref rest-val i))
                  '()
                  (list (foldl (λ (n m) (if (empty? n) n
                                            (apply proc (append (flatten n) (flatten m)))))
                               init-val (take rest-val (add1 i)))))))))
(define (scanO-seed proc seed stream)
  (for/list ([i (range (length stream))])
    (if (empty? (list-ref stream i))
        '()
        (list (foldl (λ (n m) (if (empty? n) n
                                  (apply proc (append (flatten n) (flatten m)))))
                     seed (take stream (add1 i)))))))

;; subscribe

;; after all observables have emitted at least one event
;; when the source observable emits, send it with the most recent value from the other input observables
;; can take optional function with which to combine the observable values; other send as arrays
(define (withLatestFromO source-stream input-stream)
  (let ([expanded-input (persistent-stream input-stream)])
    (map (λ (e1 e2) (if (or (empty? e1) (empty? e2))
                        '()
                        (append e1 e2))) source-stream expanded-input)))


