#lang rosette

(require "api.rkt")

(provide (all-defined-out))

(define (random-list-member xs)
  (list-ref xs (random (length xs))))

;; Deepcoder DSL grammar
;; S -> R | S R
;; R -> NR | LR
;; LR -> list | take NR LR | drop NR LR | reverse LR | sort LR 
;;        | map IntPred LR | filter BoolPred LR
;;        | zipwith Int2Pred LR LR | scanl1 Int2Pred LR
;; NR -> num | head LR | last LR | access NR LR | minimum LR
;;        | maximum LR | sum LR | count BoolPred LR
;; IntPred -> +1 | -1 | *2 | /2 | * -1 | **2 | *3 | /3 | *4 | /4
;; BoolPred -> positive? | negative? | odd? | even?
;; Int2Pred -> + | - | * | min | max

(struct tdc-insn (op-index
                 int-idx
                 list-idx
                 list2-idx
                 λi2i-idx
                 λi2b-idx
                 λi2i2i-idx) #:transparent)

;; typed operators maintains 2 lists of registers
;; separating int type and list type

(define (get-tdc-insn-holes)
  (define-symbolic* op integer?)
  (define-symbolic* int-idx integer?)
  (define-symbolic* list-idx integer?)
  (define-symbolic* list2-idx integer?)
  (define-symbolic* λi2i-idx integer?)
  (define-symbolic* λi2b-idx integer?)
  (define-symbolic* λi2i2i-idx integer?)
  (tdc-insn op int-idx list-idx list2-idx λi2i-idx λi2b-idx λi2i2i-idx))

(define (get-dc-holes-list count)
  (for/list ([i (range count)]) (get-tdc-insn-holes)))

(struct tyoperator (name call print random-insn) #:transparent)

(define (get-tyop insn)
  (list-ref tyoperator-list (tdc-insn-op-index insn)))

(define (call-tdc-insn insn int-vars list-vars)
  ((tyoperator-call (get-tyop insn)) insn int-vars list-vars))

(define (print-tdc-insn insn varname int-vars list-vars)
  (let ([op (get-tyop insn)])
    (format "  (define ~a (~a ~a))" varname (tyoperator-name op)
          ((tyoperator-print op) insn int-vars list-vars))))

(define (get-int-input insn int-vars)
  (list-ref int-vars (tdc-insn-int-idx insn)))

(define (get-list-input insn list-vars)
  (list-ref list-vars (tdc-insn-list-idx insn)))

(define (get-list2-input insn list-vars)
  (list-ref list-vars (tdc-insn-list2-idx insn)))

(define head-tyop
  (tyoperator "head"
            (λ (insn int-vars list-vars) (head-dc (get-list-input insn list-vars)))
            (λ (insn int-vars list-vars) (format "~a" (get-list-input insn list-vars)))
            (λ (int-count list-count) (tdc-insn (get-operator-idx "head") 0 (random list-count) 0 0 0 0))))
            ;(λ (int-idxs list-idxs) (tdc-insn (get-operator-idx "head") 0 (random-list-member list-idxs) 0 0 0 0))))
(define last-tyop
  (tyoperator "last"
            (λ (insn int-vars list-vars) (last-dc (get-list-input insn list-vars)))
            (λ (insn int-vars list-vars) (format "~a" (get-list-input insn list-vars)))
            (λ (int-count list-count) (tdc-insn (get-operator-idx "last") 0 (random list-count) 0 0 0 0))))
           ;(λ (int-idxs list-idxs) (tdc-insn (get-operator-idx "last") 0 (random-list-member list-idxs) 0 0 0 0))))
(define take-tyop
  (tyoperator "take"
            (λ (insn int-vars list-vars) (take-dc (get-int-input insn int-vars) (get-list-input insn list-vars)))
            (λ (insn int-vars list-vars) (format "~a ~a" (get-int-input insn int-vars)
                                                 (get-list-input insn list-vars)))
            (λ (int-count list-count) (tdc-insn (get-operator-idx "take") (random int-count) (random list-count) 0 0 0 0))))
            ;(λ (int-idxs list-idxs) (tdc-insn (get-operator-idx "take") (random-list-member int-idxs)
             ;                                         (random-list-member list-idxs) 0 0 0 0))))
(define drop-tyop
  (tyoperator "drop"
            (λ (insn int-vars list-vars) (drop-dc (get-int-input insn int-vars) (get-list-input insn list-vars)))
            (λ (insn int-vars list-vars) (format "~a ~a" (get-int-input insn int-vars)
                                                 (get-list-input insn list-vars)))
            (λ (int-count list-count) (tdc-insn (get-operator-idx "drop") (random int-count) (random list-count) 0 0 0 0))))
            ;(λ (int-idxs list-idxs) (tdc-insn (get-operator-idx "drop") (random-list-member int-idxs)
            ;                                  (random-list-member list-idxs) 0 0 0 0))))
(define access-tyop
  (tyoperator "access"
            (λ (insn int-vars list-vars) (access-dc (get-int-input insn int-vars) (get-list-input insn list-vars)))
            (λ (insn int-vars list-vars) (format "~a ~a" (get-int-input insn int-vars)
                                                 (get-list-input insn list-vars)))
            (λ (int-count list-count) (tdc-insn (get-operator-idx "access") (random int-count) (random list-count) 0 0 0 0))))
            ;(λ (int-idxs list-idxs) (tdc-insn (get-operator-idx "access") (random-list-member int-idxs)
            ;                                  (random-list-member list-idxs) 0 0 0 0))))
(define minimum-tyop
  (tyoperator "minimum"
            (λ (insn int-vars list-vars) (minimum-dc (get-list-input insn list-vars)))
            (λ (insn int-vars list-vars) (format "~a" (get-list-input insn list-vars)))
            (λ (int-count list-count) (tdc-insn (get-operator-idx "minimum") 0 (random list-count) 0 0 0 0))))
            ;(λ (int-idxs list-idxs) (tdc-insn (get-operator-idx "minimum") 0 (random-list-member list-idxs) 0 0 0 0))))
(define maximum-tyop
  (tyoperator "maximum"
            (λ (insn int-vars list-vars) (maximum-dc (get-list-input insn list-vars)))
            (λ (insn int-vars list-vars) (format "~a" (get-list-input insn list-vars)))
            (λ (int-count list-count) (tdc-insn (get-operator-idx "maximum") 0 (random list-count) 0 0 0 0))))
            ;(λ (int-idxs list-idxs) (tdc-insn (get-operator-idx "maximum") 0 (random-list-member list-idxs) 0 0 0 0))))
(define reverse-tyop
  (tyoperator "reverse"
            (λ (insn int-vars list-vars) (reverse-dc (get-list-input insn list-vars)))
            (λ (insn int-vars list-vars) (format "~a" (get-list-input insn list-vars)))
            (λ (int-count list-count) (tdc-insn (get-operator-idx "reverse") 0 (random list-count) 0 0 0 0))))
           ; (λ (int-idxs list-idxs) (tdc-insn (get-operator-idx "reverse") 0 (random-list-member list-idxs) 0 0 0 0))))
(define sort-tyop
  (tyoperator "sort"
            (λ (insn int-vars list-vars) (sort-dc (get-list-input insn list-vars)))
            (λ (insn int-vars list-vars) (format "~a" (get-list-input insn list-vars)))
            (λ (int-count list-count) (tdc-insn (get-operator-idx "sort") 0 (random list-count) 0 0 0 0))))
            ;(λ (int-idxs list-idxs) (tdc-insn (get-operator-idx "sort") 0 (random-list-member list-idxs) 0 0 0 0))))
(define sum-tyop
  (tyoperator "sum"
            (λ (insn int-vars list-vars) (sum-dc (get-list-input insn list-vars)))
            (λ (insn int-vars list-vars) (format "~a" (get-list-input insn list-vars)))
            (λ (int-count list-count) (tdc-insn (get-operator-idx "sum") 0 (random list-count) 0 0 0 0))))
            ;(λ (int-idxs list-idxs) (tdc-insn (get-operator-idx "sum") 0 (random-list-member list-idxs) 0 0 0 0))))
(define map-tyop
  (tyoperator "map"
            (λ (insn int-vars list-vars) (map-dc (list-ref int-to-int-funcs (tdc-insn-λi2i-idx insn))
                                   (get-list-input insn list-vars)))
            (λ (insn int-vars list-vars) (format "~a ~a" (list-ref int-to-int-funcs-string (tdc-insn-λi2i-idx insn))
                                   (get-list-input insn list-vars)))
            (λ (int-count list-count) (tdc-insn (get-operator-idx "map") 0 (random list-count) 0 (random (length int-to-int-funcs)) 0 0))))
            ;(λ (int-idxs list-idxs) (tdc-insn (get-operator-idx "map") 0 (random-list-member list-idxs) 0
            ;                                  (random (length int-to-int-funcs)) 0 0))))
(define filter-tyop
  (tyoperator "filter"
            (λ (insn int-vars list-vars) (filter-dc (list-ref int-to-bool-funcs (tdc-insn-λi2b-idx insn))
                                      (get-list-input insn list-vars)))
            (λ (insn int-vars list-vars) (format "~a ~a" (list-ref int-to-bool-funcs-string (tdc-insn-λi2b-idx insn))
                                   (get-list-input insn list-vars)))
            (λ (int-count list-count) (tdc-insn (get-operator-idx "filter") 0 (random list-count) 0 0 (random (length int-to-bool-funcs)) 0))))
            ;(λ (int-idxs list-idxs) (tdc-insn (get-operator-idx "filter") 0 (random-list-member list-idxs) 0 0
            ;                                  (random (length int-to-bool-funcs)) 0))))
(define count-tyop
  (tyoperator "count"
            (λ (insn int-vars list-vars) (count-dc (list-ref int-to-bool-funcs (tdc-insn-λi2b-idx insn))
                                     (get-list-input insn list-vars)))
            (λ (insn int-vars list-vars) (format "~a ~a" (list-ref int-to-bool-funcs-string (tdc-insn-λi2b-idx insn))
                                   (get-list-input insn list-vars)))
            (λ (int-count list-count) (tdc-insn (get-operator-idx "count") 0 (random list-count) 0 0 (random (length int-to-bool-funcs)) 0))))
            ;(λ (int-idxs list-idxs) (tdc-insn (get-operator-idx "count") 0 (random-list-member list-idxs) 0 0
            ;                                  (random (length int-to-bool-funcs)) 0))))
(define zipwith-tyop
  (tyoperator "zipwith"
            (λ (insn int-vars list-vars) (zipwith-dc (list-ref int-to-int-to-int-funcs (tdc-insn-λi2i2i-idx insn))
                                       (get-list-input insn list-vars) (get-list2-input insn list-vars)))
            (λ (insn int-vars list-vars) (format "~a ~a ~a"
                                                 (list-ref int-to-int-to-int-funcs-string (tdc-insn-λi2i2i-idx insn))
                                                 (get-list-input insn list-vars) (get-list2-input insn list-vars)))
            (λ (int-count list-count) (tdc-insn (get-operator-idx "zipwith") 0 (random list-count) (random list-count) 0 0 (random (length int-to-int-to-int-funcs))))))
            ;(λ (int-idxs list-idxs) (tdc-insn (get-operator-idx "zipwith") 0 (random-list-member list-idxs)
             ; (random-list-member list-idxs) 0 0 (random (length int-to-int-to-int-funcs))))))
(define scanl1-tyop
  (tyoperator "scanl1"
            (λ (insn int-vars list-vars) (scanl1-dc (list-ref int-to-int-to-int-funcs (tdc-insn-λi2i2i-idx insn))
                                      (get-list-input insn list-vars)))
            (λ (insn int-vars list-vars) (format "~a ~a"
                                                 (list-ref int-to-int-to-int-funcs-string (tdc-insn-λi2i2i-idx insn))
                                                 (get-list-input insn list-vars)))
            (λ (int-count list-count) (tdc-insn (get-operator-idx "scanl1") 0 (random list-count) 0 0 0 (random (length int-to-int-to-int-funcs))))))
            ;(λ (int-idxs list-idxs) (tdc-insn (get-operator-idx "scanl1") 0 (random-list-member list-idxs) 0
            ;                                  0 0 (random (length int-to-int-to-int-funcs))))))

(define int-tyoperator-list (list head-tyop
                                  last-tyop
                                  access-tyop
                                  minimum-tyop
                                  maximum-tyop))
(define list-tyoperator-list (list take-tyop
                                   drop-tyop
                                   reverse-tyop
                                   sort-tyop
                                   sum-tyop
                                   map-tyop
                                   filter-tyop
                                   count-tyop
                                   zipwith-tyop
                                   scanl1-tyop))

(define tyoperator-list (append int-tyoperator-list list-tyoperator-list))

(define tyoperator-lookup (make-hash (for/list ([i (range (length tyoperator-list))])
                                     (cons (tyoperator-name (list-ref tyoperator-list i)) i))))

(define (get-operator-idx op-string)
  (hash-ref tyoperator-lookup op-string))

(define (is-int-insn? insn)
  (< (tdc-insn-op-index insn) (length int-tyoperator-list)))