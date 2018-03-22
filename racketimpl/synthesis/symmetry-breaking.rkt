#lang rosette/safe
(require rosette/lib/synthax)

(require "grammar.rkt")

(define-synthax (event-stream-grmr input ... depth)
  #:base (choose input ...)
  #:else (choose input ...
                   (constantE-grmr input ... (sub1 depth))
                   (collectE-grmr input ... (sub1 depth))
                   (mergeE-grmr input ... (sub1 depth))))

(define-synthax (constantE-grmr input ... depth)
  #:base (choose input ...)
  #:else (choose input ...
                 (constantE (??)
                            (choose input ...
                                    (mergeE-grmr input ... (sub1 depth))))))

(define-synthax (collectE-grmr input ... depth)
  #:base (choose input ...)
  #:else (choose input ...
                 (collectE (??) + (choose input ...
                                       (constantE-grmr input ... (sub1 depth))
                                       (mergeE-grmr input ... (sub1 depth))))))

(define-synthax (mergeE-grmr input ... depth)
  #:base (choose input ...)
  #:else (choose input ...
                 (mergeE (choose input ...
                                 (constantE-grmr input ... (sub1 depth))
                                 (collectE-grmr input ... (sub1 depth)))
                         (choose input ...
                                 (constantE-grmr input ... (sub1 depth))
                                 (collectE-grmr input ... (sub1 depth)))
                         )))
