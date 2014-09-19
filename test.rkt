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

(require racklog-dcg rackunit rackunit/text-ui racklog "util.rkt"
         (for-template "util.rkt")
         (for-syntax racket/format racket/list racket/stxparam racklog))


;;; TEST UTILS

    
(define-test-suite test-utils
  (test-equal? "count ok" (aux-subgoal-count '((x) (%noun-phrase) (%verb-phrase))) 2)
  (test-equal? "count with cut ok" (aux-subgoal-count '((x) (%noun-phrase) (%verb-phrase) !)) 2)
  (test-equal? "count with cut in middle ok" 
               (aux-subgoal-count '((x) (%noun-phrase) ! (%verb-phrase))) 2)
  
  (test-equal? "rewrite ok?" 
               (syntax->datum (rewrite-clause '((x) (%noun-phrase) (%verb-phrase)) '(s0 s1 s2))) 
               '((x s0 s2) (%noun-phrase s0 s1) (%verb-phrase s1 s2))) 
  (test-equal? "rewrite with cut ok?" 
               (syntax->datum (rewrite-clause '((x) (%noun-phrase) (%verb-phrase) !) '(s0 s1 s2))) 
               '((x s0 s2) (%noun-phrase s0 s1) (%verb-phrase s1 s2) !)) 
  (test-equal? "rewrite with cut in middle ok?" 
               (syntax->datum (rewrite-clause '((x) (%noun-phrase) ! (%verb-phrase)) '(s0 s1 s2))) 
               '((x s0 s2) (%noun-phrase s0 s1) ! (%verb-phrase s1 s2))) 
)


;;; TEST DCG


(define %sentence (%rule () [() (%noun-phrase) (%verb-phrase)]))
(define %noun-phrase (%rule () [() (%proper-noun)]
                                [() (%det) (%noun)]
                                [() (%det) (%noun) (%rel-clause)])) 
(define %verb-phrase (%rule () [() (%trans-verb) (%noun-phrase)]
                                [() (%intrans-verb)]))
(define %rel-clause (%rule () [() (%dem-pronoun) (%verb-phrase)]))
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
(define-syntax (test-%rule stx)
 (syntax-case stx ()
   [(_ (v ...) clause ...)
    (let ()
      (define aux-var-count
        (apply max (for/list ([clause (syntax->list #'(clause ...))])
                     (add1 (aux-subgoal-count clause)))))
      (define all-aux-vars
        (generate-temporaries (make-list aux-var-count #'s)))
      (with-syntax
          ([(aux-var ...) all-aux-vars]
           [(new-clause ...)
            (for/list ([clause (in-list (syntax->list #'(clause ...)))])
              (rewrite-clause clause all-aux-vars))])
        #''(%rel (aux-var ... v ...) new-clause ...)))]))


(define-syntax (test-%term stx)
  (syntax-case stx ()
    [(%term (t ...) ...) 
     #''(%rel (x) [((append '(t ...) x) x)] ...)]))

(define-test-suite test-dcg
  (test-equal? "fancy rule ok?" (test-%rule (x) [(x) (%noun-phrase) (%verb-phrase)] 
                                                [(x) (%noun-phrase) (%verb-phrase) (%noun-phrase)]) 
                '(%rel
  (s59 s60 s61 s62 x)
  ((x s59 s61) (%noun-phrase s59 s60) (%verb-phrase s60 s61))
  ((x s59 s62)
   (%noun-phrase s59 s60)
   (%verb-phrase s60 s61)
   (%noun-phrase s61 s62))))
  
  (test-equal? "simple rule ok?" (test-%rule () [() (%noun-phrase) (%verb-phrase)])
                '(%rel (s67 s68 s69) ((s67 s69) (%noun-phrase s67 s68) (%verb-phrase s68 s69))))
   
  (test-equal? "rule with cut ok?" (test-%rule () [() (%noun-phrase) (%verb-phrase) !])
                '(%rel (s43 s44 s45) ((s43 s45) (%noun-phrase s43 s44) (%verb-phrase s44 s45))))
 
  (test-equal? "sentence found?" (%which (x) (%sentence x null)) '((x john eats john)))
  (test-equal? "is indeed a sentence?" (%which () (%sentence '(a cat eats the bat) null)) '())

  (test-equal? "test term ok?" (test-%term [big cat] [rat]) 
               '(%rel (x) (((append '(big cat) x) x)) (((append '(rat) x) x))))
  
  (test-equal? "test ok?" (%which (x) (%noun x null)) '((x cat)))
  (test-equal? "test ok?" (%which (x) (%intrans-verb x null)) '((x lives)))
)


;;; TEST SUITES


(run-tests test-utils 'verbose)
;(run-tests test-dcg 'verbose)
