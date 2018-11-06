#lang rosette
(provide (all-defined-out))

(struct sym-input (name input) #:transparent)

(define (get-inputs inputs)
  (map (λ (i) (sym-input-input i)) inputs))

(struct io-specs (inputs outputs) #:transparent)

(define (io-specs-assertion ios sketch-program)
  (assert (io-specs-formula ios sketch-program)))
(define (io-specs-formula ios sketch-program)
  (equal? (apply sketch-program (io-specs-inputs ios))
          (io-specs-outputs ios)))

(struct neg-io-specs (inputs outputs) #:transparent)

(define (neg-io-specs-assertion ios sketch-program)
  (assert (neg-io-specs-formula ios sketch-program)))
(define (neg-io-specs-formula ios sketch-program)
  (not (equal? (apply sketch-program (neg-io-specs-inputs ios))
               (neg-io-specs-outputs ios))))

(define (spec-assertions specs sketch-program)
  (for-each (λ (s) (cond [(io-specs? s) (io-specs-assertion s sketch-program)]
                         [(input-invariant? s) (input-invariant-assertion s)]
                         [(output-invariant? s) (output-invariant-assertion s sketch-program)]
                         [(input-output-invariant? s) (input-output-invariant-assertion s sketch-program)]
                         [else (void)])) specs))

(define (spec-formulas specs sketch-program)
  (andmap (λ (s) (cond [(io-specs? s) (io-specs-formula s sketch-program)]
                         [(input-invariant? s) (input-invariant-formula s)]
                         [(output-invariant? s) (output-invariant-formula s sketch-program)]
                         [(input-output-invariant? s) (input-output-invariant-formula s sketch-program)]
                         [else (void)])) specs))

;; given list of inputs in order of signature of graph
;; apply a boolean function that states invariant
;; assert that function is true
(struct input-invariant (inputs assertion-function) #:transparent)

(define (input-invariant-assertion invariant)
  (assert (input-invariant-formula invariant)))
(define (input-invariant-formula invariant)
  (apply (input-invariant-assertion-function invariant) (get-inputs (input-invariant-inputs invariant))))

(struct output-invariant (inputs assertion-function))

(define (output-invariant-assertion invariant sketch-program)
  (assert (output-invariant-formula invariant sketch-program)))
(define (output-invariant-formula invariant sketch-program)
  ((output-invariant-assertion-function invariant)
           (apply sketch-program (get-inputs (output-invariant-inputs invariant)))))

(struct input-output-invariant (inputs assertion-function))

(define (input-output-invariant-assertion invariant sketch-program)
  (input-output-invariant-formula invariant sketch-program))
(define (input-output-invariant-formula invariant sketch-program)
    (let* ([inputs-list (get-inputs (input-output-invariant-inputs invariant))]
         [inputs-and-outputs (append inputs-list (list (apply sketch-program inputs-list)))])
      (apply (input-output-invariant-assertion-function invariant) inputs-and-outputs)))