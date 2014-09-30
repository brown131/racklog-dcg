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
  (test-equal? "count ok" (subgoal-count '((x) (%noun-phrase) (%verb-phrase))) 2)
  (test-equal? "count with cut ok" (subgoal-count '((x) (%noun-phrase) (%verb-phrase) !)) 3)
  (test-equal? "count with cut in middle ok" 
               (subgoal-count '((x) (%noun-phrase) ! (%verb-phrase))) 3)
  
  (test-equal? "rewrite ok?" 
               (syntax->datum (rewrite-clause '((x) (%noun-phrase) (%verb-phrase)) '(s0 s1 s2))) 
               '((x s0 s2) (%noun-phrase s0 s1) (%verb-phrase s1 s2))) 
  (test-equal? "rewrite with cut ok?" 
               (syntax->datum (rewrite-clause '((x) (%noun-phrase) (%verb-phrase) !) '(s0 s1 s2 s3))) 
               '((x s0 s3) (%noun-phrase s0 s1) (%verb-phrase s1 s2) ! (%= s2 s3))) 
  (test-equal? "rewrite with cut in middle ok?" 
               (syntax->datum (rewrite-clause '((x) (%noun-phrase) ! (%verb-phrase)) '(s0 s1 s2 s3))) 
               '((x s0 s3) (%noun-phrase s0 s1) ! (%= s1 s2) (%verb-phrase s2 s3))) 
)


;;; DEFINE A DCG SYNTAX


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

#|
http://cs.union.edu/~striegnk/courses/esslli04prolog/practical.day5.php?s=practical.day5.node8

This is a tricky one:
list_length(0) --> [].
list_length(Length) --> [_], 
                        list_length(RestLength), 
	                     {Length is RestLength + 1}.

list_length(X, [a, b, c], []) ==> 3

Translates to, using: listing(list_length//1).
list_length(0, s0, s0).
list_length(Length, [_|s0], s1) :-
	list_length(RestLength, s0, s2),
	Length is RestLength+1,
	s1=s2.

The RackLog DCG would thus probably be:
(define %list-length (%rule (length restLength) 
  [(0)]
  [(length (list (_))) (%list-length restLength)
                       {%goal (%is length (add1 restLength)}]))
(%which (X) (%list-length X '(a b c) null))

which translates to:
(define %list-length (%rel (length restLength s0 s1 s2) 
                            [(0 s0 s0)]
                            [(length (cons (_) s0) s1)
                             (%list-length restLength s0 s2)
                             (%is length (add1 restLength))
                             (%= s1 s2)]))
Also works:
(define %list-length (%rel (length restLength s0 s1 s2) 
                            [(0 s0 s0)]
                            [(length (cons (_) s0) s1)
                             (%list-length restLength s0 s1)
                             (%is length (add1 restLength))
                             (%= s1 s2)]))

|#

(define %list-length (%rel (length restLength s0 s1 s2) 
                            [(0 s0 s0)]
                            [(length (cons (_) s0) s1)
                             (%list-length restLength s0 s1)
                             (%is length (add1 restLength))
                             (%= s1 s2)]))


;;; TEST VERSIONS OF SYNTAX CASES


;;; Return the %rule syntax as a string for testing.
(define-syntax (test-%rule stx)
 (syntax-case stx ()
   [(_ (v ...) clause ...)
    (let ()
      (define aux-var-count (apply max (for/list ([clause (syntax->list #'(clause ...))])
                                         (add1 (subgoal-count clause)))))
      (define all-aux-vars (take '(s0 s1 s2 s3 s4 s5 s6 s7) aux-var-count))
      (with-syntax
          ([(aux-var ...) all-aux-vars]
           [(new-clause ...) (for/list ([clause (in-list (syntax->list #'(clause ...)))])
                               (rewrite-clause clause all-aux-vars))])
        #''(%rel (aux-var ... v ...) new-clause ...)))]))


(define-syntax (test-%term stx)
  (syntax-case stx ()
    [(%term (t ...) ...) 
     #''(%rel (l) [((append '(t ...) l) l)] ...)]))


;;; TEST DCG


(define-test-suite test-dcg
  ;; %rule tests
  (test-equal? "fancy rule ok?" (test-%rule (x) [(x) (%noun-phrase) (%verb-phrase)] 
                                                [(x) (%noun-phrase) (%verb-phrase) (%noun-phrase)]) 
               '(%rel (s0 s1 s2 s3 x)
                      ((x s0 s2) (%noun-phrase s0 s1) (%verb-phrase s1 s2))
                      ((x s0 s3) (%noun-phrase s0 s1) (%verb-phrase s1 s2) (%noun-phrase s2 s3)))) 
  (test-equal? "simple rule ok?" (test-%rule () [() (%noun-phrase) (%verb-phrase)])
                '(%rel (s0 s1 s2) ((s0 s2) (%noun-phrase s0 s1) (%verb-phrase s1 s2))))   
  (test-equal? "rule with cut in middle ok?" (test-%rule () [() (%noun-phrase) ! (%verb-phrase)])
                '(%rel (s0 s1 s2 s3) ((s0 s3) (%noun-phrase s0 s1)
                                              ! (%= s1 s2)
                                              (%verb-phrase s2 s3))))
  (test-equal? "rule with cut as end ok?" (test-%rule () [() (%noun-phrase) (%verb-phrase) !])
                '(%rel (s0 s1 s2 s3) ((s0 s3) (%noun-phrase s0 s1) 
                                              (%verb-phrase s1 s2) ! (%= s2 s3))))

  (test-equal? "sentence found?" (%which (x) (%sentence x null)) '((x john eats john)))
  (test-equal? "is indeed a sentence?" (%which () (%sentence '(a cat eats the bat) null)) '())
  (test-equal? "list length ok?" (%which (x) (%list-length x '(a b c) null)) '((x . 3)))
  
  ;; %term tests
  
  (test-equal? "test term ok?" (test-%term [big cat] [rat]) 
               '(%rel (l) (((append '(big cat) l) l)) (((append '(rat) l) l))))
  (test-equal? "term found?" (%which (x) (%noun x null)) '((x cat)))
  (test-equal? "another term found?" (%which (x) (%intrans-verb x null)) '((x lives)))
  
  ;; %goal tests
  (test-equal? "rule with goal ok?" 
               (test-%rule (x) [(x) (%noun-phrase) (%verb-phrase) 
                                    {%goal (%noun x null) (%proper-noun x null)}])
               '(%rel (s0 s1 s2 s3 x)
                      ((x s0 s3) (%noun-phrase s0 s1) (%verb-phrase s1 s2) 
                                 (%noun x null) (%proper-noun x null) 
                                 (%= s2 s3)))) 
  (test-equal? "list length rule ok?" 
               (test-%rule (len restLen) 
                           [(0)]
                           [(len) (list (_))
                                  (%list-length restLen)
                                  {%goal (%is len (add1 restLen))}])
               '(%rel (s0 s1 s2 len restLen) 
                      [(0 s0 s0)]
                      [(len (cons (_) s0) s1)
                       (%list-length restLen s0 s2)
                       (%is len (add1 restLen))
                       (%= s1 s2)]))
)


;;; TEST SUITES


(run-tests test-utils 'verbose)
(run-tests test-dcg 'verbose)
