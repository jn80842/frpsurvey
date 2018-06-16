#lang rosette

(require racket/engine)

(require "../../../api.rkt")
(require "../../../sketch.rkt")
(require "../../../specifications.rkt")
(require "../../../random.rkt")
(require "../../../benchmark_random.rkt")

(define e (engine (λ (_) (benchmark-from-file 1 1 ""))))
(with-handlers ([exn:fail? (λ (exn) (begin (displayln "failed program")
                                           'fail))])
  (engine-run 3600000 e))
