#lang racket
;;;; A simple English grammar in Racklog DCG notation.

(require racklog "../racklog-dcg.rkt")

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
