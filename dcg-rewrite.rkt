#lang racket

;; (%rule (var-id ...) clause ...)
;; where clause = [(expr ...) subgoal ...]
;;       subgoal = (subgoal-fun-expr arg-expr ...)
;; Example:
;;    (%rule () [() (%noun-phrase) (%verb-phrase)])
;; => (%rel (s0 s1 s2)
;;      [(s0 s2) (%noun-phrase s0 s1) (%verb-phrase s1 s2)])
(define-syntax (%rule stx)
 (syntax-case stx ()
   [(_ (v ...) clause ...)
    (let ()
      ;; How many aux vars might we need? Each subgoal connects
      ;; two vars, so each clause needs one more than number of
      ;; subgoals, so need max over all clauses.
      ;; FIXME: what if no claues?
      (define aux-var-count
        (apply max (for/list ([clause (syntax->list #'(clause ...))])
                     (add1 (clause-count-subgoals clause)))))
      (define all-aux-vars
        (generate-temporaries (make-list aux-var-count #'s)))
      (with-syntax
          ([(aux-var ...) all-aux-vars]
           [(new-clause ...)
            (for/list ([clause (in-list (syntax->list #'(clause ...)))])
              (rewrite-clause clause all-aux-vars))])
        #'(%rel (aux-var ... v ...) new-clause ...)))]))

;; Compile-time helper functions
(begin-for-syntax
 ;; clause-count-subgoals : Syntax -> Nat
 (define (clause-count-subgoals clause)
   (syntax-case clause ()
     [((a ...) subgoal ...)
      (length (syntax->list #'(subgoal ...)))]))

 ;; rewrite-clause : Syntax (Listof Identifier) -> Syntax
 ;;    ((a ...) (subgoalfn_0 e ...) ... (subgoalfn_n e ...))
 ;; => ((a ... aux_0 aux_n+1)
 ;;     (subgoalfn_0 e ... aux_0 aux_1) ...
 ;;     (subgoalfn_n e ... aux_n aux_n+1))
 (define (rewrite-clause clause all-aux-vars)
   (define subgoal-count (clause-count-subgoals clause))
   (define aux-vars (take all-aux-vars (add1 subgoal-count)))
   (syntax-case clause ()
     [((a ...) subgoal ...)
      (with-syntax
          ([(new-subgoal ...)
            (for/list ([subgoal (in-list (syntax->list #'(subgoal ...)))]
                       [start-aux (in-list (drop-right aux-vars 1))]
                       [end-aux   (in-list (drop aux-vars 1))])
              (rewrite-subgoal subgoal start-aux end-aux))]
           [start-aux (first aux-vars)]
           [end-aux   (last aux-vars)])
        #'((a ... start-aux end-aux) new-subgoal ...))]))

 ;; rewrite-subgoal : Syntax Identifier Identifier -> Syntax
 ;; (subgoalfn_k e ...) => (subgoalfn_k var_k var_k+1 e ...)
 (define (rewrite-subgoal subgoal start-aux end-aux)
   (syntax-case subgoal ()
     [(subgoalfn e ...)
      (with-syntax ([start-aux start-aux]
                    [end-aux end-aux])
        #'(subgoalfn e ... start-aux end-aux))]))
 )
