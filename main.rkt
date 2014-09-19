#lang racket
;;;; Racklog/DCG
;;;;
;;;; main - Main file.
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

(require racklog "util.rkt" (for-syntax racket/format racket/list racket/stxparam))

(provide %rule %term %goal %translate-rule)

#|
Some of the features I still want to do are support for the cut (don't try
to add extra vars to it), a syntax for excluding goals from the DCG (ala
Prolog DCG's curly braces) and a way to allow DCG clauses to be added with
%assert!.
|#

;;; Define a rule using DCG notation.
;;; (%rule (var-id ...) clause ...)
;;; where clause = [(expr ...) subgoal ...]
;;;       subgoal = (subgoal-fun-expr arg-expr ...)
;;; Example:
;;;    (%rule () [() (%noun-phrase) (%verb-phrase)])
;;; => (%rel (s0 s1 s2)
;;;      [(s0 s2) (%noun-phrase s0 s1) (%verb-phrase s1 s2)])
(define-syntax (%rule stx)
 (syntax-case stx ()
   [(_ (v ...) clause ...)
    (let ()
      (define aux-var-count (apply max (for/list ([clause (syntax->list #'(clause ...))])
                                         (add1 (aux-subgoal-count clause)))))
      (define all-aux-vars (generate-temporaries (make-list aux-var-count #'s)))
      (with-syntax
          ([(aux-var ...) all-aux-vars]
           [(new-clause ...) (for/list ([clause (in-list (syntax->list #'(clause ...)))])
                               (rewrite-clause clause all-aux-vars))])
        #'(%rel (aux-var ... v ...) new-clause ...)))]))

;;; Define a terminal using DCG notation. (PAIP uses :word)
(define-syntax (%term stx)
  (syntax-case stx ()
    [(%term (t ...) ...) 
     #'(%rel (x) [((append '(t ...) x) x)] ...)]))

;;; Ordinary Racklog goal. Is not translated. Same as braces in Prolog. (PAIP uses ":test")
(define %goal 1)

;;; Translate a DCG rule or terminal into ordinary Racklog. Used for %assert!.
(define %translate-rule 1)
