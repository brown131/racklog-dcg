#lang racket
;;;; Racklog/DCG
;;;;
;;;; test - Test module for the project
;;;;
;;;; Copyright (c) Scott Brown 2013, All rights reserved.
;;;; 
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3.0 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library.

(require racklog-dcg rackunit rackunit/text-ui racklog
         (for-syntax racket/format racket/list))

;;; Define a rule using DCG notation.
(define-syntax (%xrule stx)
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

(define %sentence (%xrule () [() (%noun-phrase) (%verb-phrase)]))
(define %noun-phrase (%xrule () [() (%proper-noun)]
                                [() (%det) (%noun)]
                                [() (%det) (%noun) (%rel-clause)]))
(define %verb-phrase (%xrule () [() (%trans-verb) (%noun-phrase)]
                                [() (%intrans-verb)]))
(define %rel-clause (%xrule () [() (%dem-pronoun) (%verb-phrase)]))
(define %det (%term [the] [every] [a]))
(define %noun (%term [cat] [bat]))
(define %proper-noun (%term [john] [mary]))
(define %dem-pronoun (%term [that]))
(define %trans-verb (%term [eats]))
(define %intrans-verb (%term [lives]))

(%which (x) (%sentence x null))
(%which () (%sentence '(a cat eats the bat) null))
(%which (x) (%noun x null))
(%which (x) (%intrans-verb x null))

;;; Return the %rule syntax as a string for testing.
(define-syntax test-%rule
  (syntax-rules ()
    ((_ (v ...) ((a ...) subgoal ...) ...)     
     (syntax->datum #`(%rel #,(append (for/list ([i (in-range (sub1 (apply max (for/list ([g '(((a ...) subgoal ...) ...)]) 
                                                                                 (length g)))))]) 
                                        (string->symbol (~a "s" i))) '(s v ...))
                            #,(let ([nsgs (length '(subgoal ...))])
                                (cons '(s0 s a ...) 
                                      (for/list ([sg '(subgoal ...)]
                                                 [i (in-range nsgs)])
                                        (append sg (list (string->symbol (~a "s" i)) 
                                                         (if (= i (sub1 nsgs)) 's 
                                                             (string->symbol (~a "s" (add1 i))))))))) ...)))))

;;; Return the %term syntax as a string for testing.
#|
(define-syntax test-%term
  (syntax-rules ()
    ((%term (t ...) ...) 
     '(%rel (x) [((append '(t ...) x) x)] ...))))
|#

(define-syntax (test-%term stx)
  (syntax-case stx ()
    [(%term (t ...) ...) 
     #''(%rel (x) [((append '(t ...) x) x)] ...)]))

(define-test-suite test-dcg
  (test-equal? "fancy rule ok?" (test-%rule (x) [(x) (%noun-phrase) (%verb-phrase)] 
                                                [(x) (%noun-phrase) (%verb-phrase) (%noun-phrase)]) 
                '(%rel
                  (x s0 s1 s2 s)
                  ((x s0 s) (%noun-phrase s0 s1) (%verb-phrase s1 s))
                  ((x s0 s) (%noun-phrase s0 s1) (%verb-phrase s1 s2) (%noun-phrase s2 s))))
  (test-equal? "rule ok?" (test-%rule () [() (%noun-phrase) (%verb-phrase)])
                '(%rel (s0 s1 s) ((s0 s) (%noun-phrase s0 s1) (%verb-phrase s1 s))))

  ;(test-equal? "rule ok?" (%xrule (x) [(x) (%noun-phrase) (%verb-phrase)]) #f)
  
  (test-equal? "rule ok?" (%which (x) (%sentence x null)) '((x john eats john)))
  (test-equal? "rule ok?" (%which () (%sentence '(a cat eats the bat) null)) '())

  (test-equal? "test term ok?" (test-%term [big cat] [rat]) 
               '(%rel (x) (((append '(big cat) x) x)) (((append '(rat) x) x))))
  
  (test-equal? "test ok?" (%which (x) (%noun x null)) '((x cat)))
  (test-equal? "test ok?" (%which (x) (%intrans-verb x null)) '((x lives)))
)

(run-tests test-dcg 'verbose)
