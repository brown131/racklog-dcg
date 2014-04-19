#lang racket

(require racklog)

#|
 sentence(S0,S) :- noun_phrase(S0,S1), verb_phrase(S1,S).
 noun_phrase(S0,S) :- proper_noun(S0,S).
 noun_phrase(S0,S) :- det(S0,S1), noun(S1,S).
 noun_phrase(S0,S) :- det(S0,S1), noun(S1,S2), rel_clause(S2,S).
 verb_phrase(S0,S) :- trans_verb(S0,S1), noun_phrase(S1,S).
 verb_phrase(S0,S) :- intrans_verb(S0,S).
 rel_clause(S0,S) :- dem_pronoun(S0,S1), verb_phrase(S1,S).
 det([the|X], X).
 det([a|X], X).
 noun([cat|X], X).
 noun([bat|X], X).
 proper_noun([john|X], X).
 proper_noun([mary|X], X).
 dem_pronoun([that|X], X).
 trans_verb([eats|X], X).
 intrans_verb([lives|X], X).
|#

(define %sentence 
  (%rel (s0 s1 s) 
        [(s0 s) (%noun-phrase s0 s1) 
                (%verb-phrase s1 s)]))

(define %noun-phrase 
  (%rel (s0 s1 s2 s) 
        [(s0 s) (%proper-noun s0 s)]
        [(s0 s) (%det s0 s1) 
                (%noun s1 s)]
        [(s0 s) (%det s0 s1) 
                (%noun s1 s2)
                (%rel-clause s2 s)]))

(define %verb-phrase 
  (%rel (s0 s1 s)  [(s0 s) (%trans-verb s0 s1) 
                           (%noun-phrase s1 s)]
                   [(s0 s) (%intrans-verb s0 s)]))

(define %rel-clause
  (%rel (x s0 s1 s) [(s0 s) (%dem-pronoun s0 s1)
                            (%verb-phrase s1 s)]))

(define %det (%rel (x) [((cons 'the x) x)]
                       [((cons 'every x) x)]            
                       [((cons 'a x) x)]))

(define %noun (%rel (x) [((cons 'cat x) x)]
                        [((cons 'bat x) x)]))

(define %proper-noun (%rel (x) [((cons 'john x) x)]
                               [((cons 'mary x) x)]))

(define %dem-pronoun (%rel (x) [((cons 'that x) x)]))

(define %trans-verb (%rel (x) [((cons 'eats x) x)]))

(define %intrans-verb (%rel (x) [((cons 'lives x) x)]))

(%which (x) (%sentence x null))
(for/list ([i (in-range 15)]) (%more))
(%which () (%sentence '(a cat eats the bat that lives) null))
(%which () (%sentence '(a cat eats the bat) null))
(%which () (%sentence '(a cat eats john) null))
(%which () (%sentence '(a cat eats the eats) null))
