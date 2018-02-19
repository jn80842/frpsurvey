#lang rosette/safe

(require "../dense-fjmodels.rkt")
(require "../densefjapi.rkt")

(provide (all-defined-out))

;;          mergeE
;;        /       \
;;   saveButtonE  andE
;;               /     \
;;           timerE    snapshotE
;;          /      \     /      \
;;    saveButtonE   5  timerE    collectB
;;                    /     \     /   |    \
;;              saveButtonE  5   λ    #f  textChangedE

;;              filterE
;;            /        \
;;          λ       filterRepeatsE
;;                      |
;;                    mergeE
;;                   /       \
;;          constantE         mergeE
;;          /       \       /        \
;;       #t  textChangedE constantE    constantE
;;                        /      \      /      \
;;                     #f  saveButtonE #f    timerE
;;                                             |
;;                                             5


;;(define (saveDraft-graph textChangedE saveButtonE)
;;  (mergeE saveButtonE
;;          (andE (timerE 5 saveButtonE)
;;                (snapshotE (timerE 5 saveButtonE)
;;                           (collectB (λ (prev curr)))))

(define s-saveButtonE (new-event-stream (λ () 'save) 5))
(define s-textChangedE (new-event-stream (λ () 'change) 5))

(define concrete-save-buttonE '(no-evt no-evt no-evt save no-evt no-evt no-evt no-evt no-evt no-evt))
(define concrete-text-changedE '(no-evt change change no-evt no-evt change change no-evt no-evt no-evt))

(define (saveCommand-graph textChangedE saveButtonE)
  (filterE (λ (e) e) (filterRepeatsE (mergeE saveButtonE
                                            (mergeE (timerE 5 textChangedE)
                                                    (constantE #f textChangedE))))))
