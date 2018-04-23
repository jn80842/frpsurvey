#lang rosette

(require "typed-operators.rkt")

(provide (all-defined-out))

(define (random-number magnitude)
  (- (random (* magnitude 2)) magnitude))

(define (get-random-list size magnitude)
  (for/list ([i (range size)])
    (random-number magnitude)))

(define (get-random-typed-insn int-idxs list-idxs)
  (let ([random-op (if (equal? 0 int-idxs)
                       (list-ref does-not-require-int-input-idxs (random (length does-not-require-int-input-idxs)))
                       (random (length tyoperator-list)))])
    ((tyoperator-random-insn (list-ref tyoperator-list random-op)) int-idxs list-idxs)))

;; NB: when picking input indexes we might want to bias toward higher indexes
;; maybe just insert later indexes multiply times?
#;(define (get-random-program reg-count int-count list-count)
  (letrec ([f (λ (insn int-idxs list-idxs i)
                (cond [(equal? reg-count i) insn]
                      [else (let ([next-insn (get-random-typed-insn int-idxs list-idxs)])
                              (if (is-int-insn? next-insn)
                                  (f (append insn (list next-insn)) (append int-idxs (list i)) list-idxs (add1 i))
                                  (f (append insn (list next-insn)) int-idxs (append list-idxs (list i)) (add1 i))))]))])
    (f '() (range int-count) (range int-count (+ int-count list-count)) 0)))
(define (get-random-program reg-count int-count list-count)
  (letrec ([f (λ (insns int-count list-count i)
                (cond [(equal? reg-count i) insns]
                      [else (let ([next-insn (get-random-typed-insn int-count list-count)])
                              (if (is-int-insn? next-insn)
                                  (f (append insns (list next-insn)) (add1 int-count) list-count (add1 i))
                                  (f (append insns (list next-insn)) int-count (add1 list-count) (add1 i))))]))])
    (f '() int-count list-count 0)))

(define (get-random-program-function insns)
  (letrec ([f (λ (calculated-ints calculated-lists i)
                (cond [(equal? (sub1 (length insns)) i)
                       (call-tdc-insn (last insns) calculated-ints calculated-lists)]
                      [else (let* ([next-insn (list-ref insns i)]
                                   [next-val (call-tdc-insn next-insn calculated-ints calculated-lists)])
                              (if (is-int-insn? next-insn)
                                  (f (append calculated-ints (list next-val)) calculated-lists (add1 i))
                                  (f calculated-ints (append calculated-lists (list next-val)) (add1 i))))]))])
    (λ (int-inputs list-inputs) (f int-inputs list-inputs 0))))

(define (register-names insns ints lists)
  (letrec ([f (λ (insns int-count list-count)
                (cond [(empty? insns) '()]
                      [(is-int-insn? (first insns))
                       (append (list (format "ri~a" int-count)) (f (cdr insns) (add1 int-count) list-count))]
                      [else (append (list (format "rl~a" list-count)) (f (cdr insns) int-count (add1 list-count)))]))])
    (f insns (add1 ints) (add1 lists))))

(define (string-from-random-program insns int-count list-count funcname)
  (let* ([arg-list (for/list ([i (range (+ int-count list-count))])
                    (format "input~a" (add1 i)))]
         [input-stmt-list (append (for/list ([i (range int-count)])
                                    (format "  (define ri~a input~a)" (add1 i) (add1 i)))
                                  (for/list ([i (range int-count (+ int-count list-count))])
                            (format "  (define rl~a input~a)" (add1 (- i int-count)) (add1 i))))]
         [int-insn-count (count is-int-insn? insns)]
         [int-varlist (for/list ([i (range (+ int-count int-insn-count))])
                       (format "ri~a" (add1 i)))]
         [list-varlist (for/list ([i (range (+ list-count (- (length insns) int-insn-count)))])
                        (format "rl~a" (add1 i)))]
         [reg-list (register-names insns int-count list-count)]
         [stmt-list (for/list ([i (range (length insns))])
                      (print-tdc-insn (list-ref insns i)
                                      (list-ref reg-list i)
                                      int-varlist
                                      list-varlist))]
         [return-stmt (format "  ~a)" (last reg-list))])
    (string-append (format "(define (~a ~a)\n" funcname (string-join arg-list))
                   (string-join input-stmt-list "\n")
                   "\n"
                   (string-join stmt-list "\n")
                   "\n"
                   return-stmt)))
                                      

(define (print-from-random-program insn int-count list-count [funcname "random-function"])
  (displayln (string-from-random-program insn int-count list-count funcname)))



                                                                                 
        
