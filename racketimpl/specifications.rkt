#lang rosette
(provide (all-defined-out))

(struct sym-input (name input) #:transparent)

(define (get-inputs inputs)
  (map (λ (i) (sym-input-input i)) inputs))

(struct io-specs (inputs outputs) #:transparent)

(define (io-specs-assertion ios sketch-program)
  (assert (equal? (apply sketch-program (io-specs-inputs ios))
                  (io-specs-outputs ios))))

(struct neg-io-specs (inputs outputs) #:transparent)

(define (neg-io-specs-assertion ios sketch-program)
  (assert (not (equal? (apply sketch-program (neg-io-specs-inputs ios))
                       (neg-io-specs-outputs ios)))))

(define (spec-assertions specs sketch-program)
  (for-each (λ (s) (cond [(io-specs? s) (io-specs-assertion s sketch-program)]
                         [(input-invariant? s) (input-invariant-assertion s)]
                         [(output-invariant? s) (output-invariant-assertion s sketch-program)]
                         [(input-output-invariant? s) (input-output-invariant-assertion s sketch-program)]
                         [else (void)])) specs))

;; given list of inputs in order of signature of graph
;; apply a boolean function that states invariant
;; assert that function is true
(struct input-invariant (inputs assertion-function) #:transparent)

(define (input-invariant-assertion invariant)
  (assert (apply (input-invariant-assertion-function invariant) (get-inputs (input-invariant-inputs invariant)))))

(struct output-invariant (inputs assertion-function))

(define (output-invariant-assertion invariant sketch-program)
  (assert ((output-invariant-assertion-function invariant)
           (apply sketch-program (get-inputs (output-invariant-inputs invariant))))))

(struct input-output-invariant (inputs assertion-function))

(define (input-output-invariant-assertion invariant sketch-program)
  (let* ([inputs-list (get-inputs (input-output-invariant-inputs invariant))]
         [inputs-and-outputs (append inputs-list (list (apply sketch-program inputs-list)))])
    (assert (apply (input-output-invariant-assertion-function invariant) inputs-and-outputs))))
