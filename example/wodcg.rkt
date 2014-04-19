#lang racket
;;;; A simple English grammar without Racklog DCG notation.

(require racklog)

(define %sentence 
  (%rel (s0 s1 s) [(s0 s) (%noun-phrase s0 s1) (%verb-phrase s1 s)]))
(define %noun-phrase 
  (%rel (s0 s1 s2 s) 
        [(s0 s) (%proper-noun s0 s)]
        [(s0 s) (%det s0 s1) (%noun s1 s)]
        [(s0 s) (%det s0 s1) (%noun s1 s2) (%rel-clause s2 s)]))
(define %verb-phrase 
  (%rel (s0 s1 s) [(s0 s) (%trans-verb s0 s1) (%noun-phrase s1 s)]
                  [(s0 s) (%intrans-verb s0 s)]))
(define %rel-clause
  (%rel (x s0 s1 s) [(s0 s) (%dem-pronoun s0 s1) (%verb-phrase s1 s)]))
(define %det (%rel (x) [((append '(the) x) x)]
                       [((append '(every) x) x)]            
                       [((append '(a) x) x)]))
(define %noun (%rel (x) [((append '(cat) x) x)]
                        [((append '(bat) x) x)]))
(define %proper-noun (%rel (x) [((append '(john) x) x)]
                               [((append '(mary) x) x)]))
(define %dem-pronoun (%rel (x) [((append '(that) x) x)]))
(define %trans-verb (%rel (x) [((append '(eats) x) x)]))
(define %intrans-verb (%rel (x) [((append '(lives) x) x)]))

(%which (x) (%sentence x null))
(%which () (%sentence '(a cat eats the bat) null))
(%which (x) (%noun x null))
