#lang rosette

(provide (all-defined-out))

(require "fjapi.rkt")
(require "fjmodel.rkt")

(struct operator
  (name call print) #:transparent)

(define zeroE-op
  (operator "zeroE"
            (λ (i reg) (zeroE (list-ref reg (insn-idx1 i))))
            (λ (i reg) (list-ref reg (insn-idx1 i)))))
(define mapE-op
  (operator "mapE"
            (λ (i reg) (mapE (list-ref function-list (insn-idx2 i))
                             (list-ref reg (insn-idx1 i))))
            (λ (i reg) (format "~a ~a" (list-ref function-list-string (insn-idx2 i))
                               (list-ref reg (insn-idx1 i))))))
(define mapE2-op
  (operator "mapE"
            (λ (i reg) (mapE2 (list-ref function-2arg-list (insn-idx2 i))
                              (get-input-stream i reg) (list-ref reg (insn-idx3 i))))
            (λ (i reg) (format "~a ~a ~a" (list-ref function-2arg-list (insn-idx2 i))
                               (get-input-stream i reg) (list-ref reg (insn-idx3 i))))))
(define collectE-op
  (operator "collectE"
            (λ (i reg) (collectE (list-ref constant-list (insn-idx2 i))
                                 (list-ref function-2arg-list (insn-idx3 i))
                                 (get-input-stream i reg)))
            (λ (i reg) (format "~a ~a ~a" (list-ref constant-list (insn-idx2 i))
                               (list-ref function-2arg-list-string (insn-idx3 i))
                               (get-input-stream i reg)))))
(define collectE-imm-op
  (operator "collectE"
            (λ (i reg) (collectE (insn-int i) (list-ref function-2arg-list (insn-idx3 i))
                                 (get-input-stream i reg)))
            (λ (i reg) (format "~a ~a ~a" (insn-int i) (list-ref function-2arg-list-string (insn-idx3 i))
                              (get-input-stream i reg)))))
(define mergeE-op
  (operator "mergeE"
            (λ (i reg) (mergeE (list-ref reg (insn-idx1 i)) (list-ref reg (insn-idx2 i))))
            (λ (i reg) (format "~a ~a" (list-ref reg (insn-idx1 i)) (list-ref reg (insn-idx2 i))))))
(define filterE-op
  (operator "filterE"
            (λ (i reg) (filterE (list-ref function-list (insn-idx2 i))
                                (list-ref reg (insn-idx1 i))))
            (λ (i reg) (format "~a ~a" (list-ref function-list-string (insn-idx2 i))
                               (list-ref reg (insn-idx1 i))))))
(define constantE-op
  (operator "constantE"
            (λ (i reg) (constantE (list-ref constant-list (insn-idx2 i))
                                  (list-ref reg (insn-idx1 i))))
            (λ (i reg) (format "~a ~a" (list-ref constant-list (insn-idx2 i))
                               (list-ref reg (insn-idx1 i))))))
(define constantE-imm-op
  (operator "constantE"
            (λ (i reg) (constantE (insn-int i) (list-ref reg (insn-idx1 i))))
            (λ (i reg) (format "~a ~a" (insn-int i) (list-ref reg (insn-idx1 i))))))
(define filterRepeatsE-op
  (operator "filterRepeatsE"
            (λ (i reg) (filterRepeatsE (get-input-stream i reg)))
            (λ (i reg) (format "~a" (get-input-stream i reg)))))
(define startsWith-op
  (operator "startsWith"
            (λ (i reg) (startsWith (list-ref constant-list (insn-idx2 i))
                                   (get-input-stream i reg)))
            (λ (i reg) (format "~a ~a" (list-ref constant-list (insn-idx2 i))
                               (get-input-stream i reg)))))
(define startsWith-imm-op
  (operator "startsWith"
            (λ (i reg) (startsWith (insn-int i) (get-input-stream i reg)))
            (λ (i reg) (format "~a ~a" (insn-int i) (get-input-stream i reg)))))
(define constantB-op
  (operator "constantB"
            (λ (i reg) (constantB (list-ref constant-list (insn-idx2 i))
                                  (list-ref reg (insn-idx1 i))))
            (λ (i reg) (format "~a ~a" (list-ref constant-list (insn-idx2 i))
                               (list-ref reg (insn-idx1 i))))))
(define constantB-imm-op
  (operator "constantB"
            (λ (i reg) (constantB (insn-int i) (get-input-stream i reg)))
            (λ (i reg) (format "~a ~a" (insn-int i) (get-input-stream i reg)))))
(define andB-op
  (operator "andB"
            (λ (i reg) (andB (list-ref reg (insn-idx1 i))
                             (list-ref reg (insn-idx2 i))))
            (λ (i reg) (format "~a ~a" (list-ref reg (insn-idx1 i))
                               (list-ref reg (insn-idx2 i))))))
(define orB-op
  (operator "orB"
            (λ (i reg) (orB (get-input-stream i reg)
                            (list-ref reg (insn-idx2 i))))
            (λ (i reg) (format "~a ~a" (get-input-stream i reg)
                               (list-ref reg (insn-idx2 i))))))
(define notB-op
  (operator "notB"
            (λ (i reg) (notB (get-input-stream i reg)))
            (λ (i reg) (format "~a" (get-input-stream i reg)))))
(define liftB-op
  (operator "liftB"
            (λ (i reg) (liftB1 (list-ref function-list (insn-idx2 i))
                              (list-ref reg (insn-idx1 i))))
            (λ (i reg) (format "~a ~a" (list-ref function-list-string (insn-idx2 i))
                               (list-ref reg (insn-idx1 i))))))
(define liftB2-op
  (operator "liftB"
            (λ (i reg) (liftB2 (list-ref function-2arg-list (insn-idx2 i))
                               (get-input-stream i reg) (list-ref reg (insn-idx3 i))))
            (λ (i reg) (format "~a ~a ~a" (list-ref function-2arg-list (insn-idx2 i))
                               (get-input-stream i reg) (list-ref reg (insn-idx3 i))))))
(define ifB-op
  (operator "ifB"
            (λ (i reg) (ifB (list-ref reg (insn-idx1 i))
                            (list-ref reg (insn-idx2 i))
                            (list-ref reg (insn-idx3 i))))
            (λ (i reg) (format "~a ~a ~a" (list-ref reg (insn-idx1 i))
                               (list-ref reg (insn-idx2 i))
                               (list-ref reg (insn-idx3 i))))))
(define collectB-op
  (operator "collectB"
            (λ (i reg) (collectB (list-ref constant-list (insn-idx2 i))
                                 (list-ref function-2arg-list (insn-idx3 i))
                                 (get-input-stream i reg)))
            (λ (i reg) (format "~a ~a ~a" (list-ref constant-list (insn-idx2 i))
                               (list-ref function-2arg-list-string (insn-idx3 i))
                               (get-input-stream i reg)))))
(define collectB-imm-op
  (operator "collectB"
            (λ (i reg) (collectB (insn-int i) (list-ref function-2arg-list (insn-idx3 i))
                                 (get-input-stream i reg)))
            (λ (i reg) (format "~a ~a ~a" (insn-int i) (list-ref function-2arg-list-string (insn-idx3 i))
                              (get-input-stream i reg)))))

(define event-operator-list
  (list mergeE-op
        constantE-imm-op
        constantE-op
        mapE-op
        mapE2-op
        filterE-op
        collectE-op
        collectE-imm-op
        filterRepeatsE-op
        filterE-op))
(define behavior-operator-list
  (list constantB-imm-op
        constantB-op
        andB-op
        orB-op
        notB-op
        ifB-op
        liftB-op
        liftB2-op
        collectB-op
        collectB-imm-op
        startsWith-op
        startsWith-imm-op
        ))

(define function-list (list (λ (e) (+ e 5))
                            (λ (t) (<= t 2))
                            (λ (c) (or (>= c 4) (>= 2 c)))
                            (λ (e) (if e 'on 'off))
                            ))
(define function-list-string (list "(λ (e) (+ e 5))"
                                   "(λ (t) (<= t 2))"
                                   "(λ (c) (or (>= c 4) (>= 2 c)))"
                                   "(λ (e) (if e 'on 'off))"
                                   ))
(define function-2arg-list (list (λ (clock location) (if (or (>= clock 4) (< clock 2))
                                 'night
                                 (if (equal? location 'home)
                                     'home
                                     'away)))
                                 (λ (elt1 elt2) (+ elt1 elt2))
                                 (λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))
                               ;  (λ (rain clock) (if (is-midnight? clock) 'midnight rain))
                                 (λ (r prev) (if (eq? r 'midnight) #f
                                                 (if r #t prev)))
                                 #;(λ (rain clock) (and (not rain)
                                                  (eq? (time-vec-hour clock) 18)
                                                  (< (time-vec-min1 clock) 1)))
                                 (λ (x y) (if x y 'no-evt))
                                 (λ (x y) (if x y x))
                                 ))
(define function-2arg-list-string (list "(λ (clock location) (if (or (>= clock hour-begin) (< clock hour-end))
                                 'night
                                 (if (equal? location 'home)
                                     'home
                                     'away)))"
                                        "(λ (elt1 elt2) (+ elt1 elt2))"
                                        "(λ (light mode) (if (equal? light 'on) (if (equal? mode 'night) 'orange 'white) 'none))"
                                      ;  "(λ (rain clock) (if (is-midnight? clock) 'midnight rain))"
                                        "(λ (r prev) (if (eq? r 'midnight) #f
                                                     (if r #t prev)))"
                                      ;  "(λ (rain clock) (and (not rain)
                                      ;            (eq? (time-vec-hour clock) 18)
                                      ;            (< (time-vec-min1 clock) 1)))"
                                        "(λ (x y) (if x y 'no-evt))"
                                        "(λ (x y) (if x y x))"
                                        ))
(define constant-list (list 'on 'off #t #f 'test))

(struct insn
  (e-or-b op-index idx1 idx2 idx3 int) #:transparent)

(define (get-input-stream insn past-vars)
  (list-ref past-vars (insn-idx1 insn)))

(define (get-insn-holes)
  (define-symbolic* e? boolean?)
  (define-symbolic* op integer?)
  (define-symbolic* idx1 integer?)
  (define-symbolic* idx2 integer?)
  (define-symbolic* idx3 integer?)
  (define-symbolic* int integer?)
  (insn e? op idx1 idx2 idx3 int))

(define (truncate-histories registers timestep)
  (map (λ (e) (e timestep)) (map (curry take) registers)))
(define constant-insn (insn #t 0 0 0 0 1))

(define (call-insn i reg)
    (if (insn-e-or-b i)
        (let ([firstinput (get-input-stream i reg)])
          (for/list ([timestep (range 1 (add1 (length firstinput)))])
            (if (empty-event? (list-ref firstinput (sub1 timestep)))
                (if (equal? 0 (insn-op-index i))
                    ((operator-call mergeE-op) i (truncate-histories reg timestep))
                    'no-evt)
                    ((operator-call (list-ref event-operator-list (insn-op-index i)))
                 i (truncate-histories reg timestep)))))
        ((operator-call (list-ref behavior-operator-list (insn-op-index i))) i reg)))

#;(define (print-insn i reg)
  (let ([op (list-ref operator-list (insn-op-index i))])
    ((operator-print op) i reg)))

#;(define (print-single-insn bound-holes varname past-vars)
  (define op (operator-name (list-ref operator-list (insn-op-index bound-holes))))
  (define op-args (print-struct-insn bound-holes past-vars))
  (format "  (define ~a (~a ~a))" varname op op-args))

#;(define (print-struct-insn i past-vars)
  (let ([op (list-ref operator-list (insn-op-index i))])
    ((operator-print op) i past-vars)))

#;(define (string-from-holes bound-holes retval input-count)
  (let* ([arg-list (for/list ([i (range input-count)])
                    (format "input~a" (add1 i)))]
        [input-stmt-list (for/list ([i (range input-count)])
                           (format "  (define r~a input~a)" (add1 i) (add1 i)))]
        [depth (length bound-holes)]
        [varlist (for/list ([i (range (add1 (+ input-count depth)))])
                                            (format "r~a" (add1 i)))]
        [synthed-stmt-list (for/list ([i (range depth)])
                             (print-single-insn (list-ref bound-holes i) (list-ref varlist (+ input-count i))
                                                (take varlist (+ input-count i))))]
        [return-stmt (format "  ~a)" (list-ref varlist retval))])
    (string-append (format "(define (synthesized-function ~a)\n" (string-join arg-list))
                   (string-join input-stmt-list "\n")
                   "\n"
                   (string-join synthed-stmt-list "\n")
                   "\n"
                   return-stmt)))

#;(define (print-from-holes bound-holes retval input-count)
  (displayln (string-from-holes bound-holes retval input-count)))

#;(define (eval-graph graph . inputs)
  (behavior (apply graph (map behavior-init inputs))
            (apply (curry map graph) (map behavior-changes inputs))))
