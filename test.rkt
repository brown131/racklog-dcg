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

(require racklog rackunit rackunit/text-ui "racklog-dcg.rkt")

;;; Define namespace anchor.
(define-namespace-anchor test-namespace-anchor)
(define test-namespace (namespace-anchor->namespace test-namespace-anchor))

;;; Define a rule using DCG notation.
(define-syntax %xrule
  (syntax-rules ()
    ((_ (v ...) ((a ...) subgoal ...) ...)     
     (eval-syntax #`(%rel #,(append (for/list ([i (in-range (sub1 (apply max (for/list ([g '(((a ...) subgoal ...) ...)]) 
                                                                                 (length g)))))]) 
                                        (string->symbol (~a "s" i))) '(s v ...))
                            #,(let ([nsgs (length '(subgoal ...))])
                                (cons '(s0 s a ...) 
                                      (for/list ([sg '(subgoal ...)]
                                                 [i (in-range nsgs)])
                                        (append sg (list (string->symbol (~a "s" i)) 
                                                         (if (= i (sub1 nsgs)) 's 
                                                             (string->symbol (~a "s" (add1 i))))))))) ...)
                  test-namespace))))



(define %sentence (%yrule () [() (%noun-phrase) (%verb-phrase)]))
(define %noun-phrase (%yrule () [() (%proper-noun)]
                               [() (%det) (%noun)]
                               [() (%det) (%noun) (%rel-clause)]))
(define %verb-phrase (%yrule () [() (%trans-verb) (%noun-phrase)]
                               [() (%intrans-verb)]))
(define %rel-clause (%yrule () [() (%dem-pronoun) (%verb-phrase)]))
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
(define-syntax test-%term
  (syntax-rules ()
    ((%term (t ...) ...)
     (syntax->datum #`(%rel (x) [((append '(t ...) x) x)] ...)))))
 

(define-test-suite test-dcg
  (test-equal? "fancy rule ok?" (test-%rule (x) [(x) (%noun-phrase) (%verb-phrase)] 
                                     [(x) (%noun-phrase) (%verb-phrase) (%noun-phrase)]) 
                '(%rel
                  (s0 s1 s2 s x)
                  ((s0 s x) (%noun-phrase s0 s1) (%verb-phrase s1 s))
                  ((s0 s x) (%noun-phrase s0 s1) (%verb-phrase s1 s2) (%noun-phrase s2 s))))
  (test-equal? "rule ok?" (test-%rule () [() (%noun-phrase) (%verb-phrase)])
                '(%rel (s0 s1 s) ((s0 s) (%noun-phrase s0 s1) (%verb-phrase s1 s))))
  
  (test-equal? "rule ok?" (%which (x) (%sentence x null)) '((x john eats john)))
  (test-equal? "rule ok?" (%which () (%sentence '(a cat eats the bat) null)) '())

  (test-equal? "test term ok?" (test-%term [big cat] [rat]) 
               '(%rel (x) (((append '(big cat) x) x)) (((append '(rat) x) x))))
  
  (test-equal? "test ok?" (%which (x) (%noun x null)) '((x cat)))
  (test-equal? "test ok?" (%which (x) (%intrans-verb x null)) '((x lives)))
)

(run-tests test-dcg 'verbose)
