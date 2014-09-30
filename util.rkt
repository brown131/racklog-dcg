#lang racket
;;;; Racklog/DCG
;;;;
;;;; util - Utilities file.
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

(require racklog (for-syntax racket/list syntax/parse))

(begin-for-syntax   

  ;;; Count of the number of subgoals in a clause.
  ;(: clause syntax)
  (define (subgoal-count clause)
    (syntax-case clause ()
      [((var-id ...) subgoal ...)
       (length (syntax->list #'(subgoal ...)))]))

  ;;; Rewrite the subgoals in a clause to use auxilliary variables.
  ;;; a) (subgoal) => (subgoal s1 s2)
  ;;; b) ! => ! (%= s1 s2)
  ;;; c) (%goal (goal1) (goal2)) => (goal1) (goal2) (%= s1 s2)
  
  ;;; TODO: 
  ;;; 1. (list x) => (cons x s0) -- check all contexts of this 
  ;;; 2. ! => ! (%= s1 s2)
  ;;; 3. (%goal (goal1) (goal2) => (goal1) (goal2) (%= s1 s2)
  ;;; 4. Possibly support curly braces instead of, or in addition to, %goal.
  (define (rewrite-clause clause all-aux-vars)
    (define aux-vars (take all-aux-vars (add1 (subgoal-count clause))))
    (define start-vars (take all-aux-vars (subgoal-count clause)))
    (define end-vars (take (rest all-aux-vars) (subgoal-count clause)))
    (syntax-case clause ()
      [((var-id ...) subgoal ...)
       (with-syntax
           ([(new-subgoal ...) 
             (let ([start-idx -1])
               (foldl (λ (sg s e l) (append l (rewrite-subgoal sg s e)))
                      null (syntax->list #'(subgoal ...)) start-vars end-vars))]
            [start-aux (first aux-vars)]
            [end-aux (last aux-vars)])
         #'((var-id ... start-aux end-aux) new-subgoal ...))]))
  
  ;;; Rewrite a goal with the inner arguments as the contained goal.
  (define (rewrite-goal subgoal start-aux end-aux)
    (syntax-case subgoal ()
      [(%goal goal ...) 
       (with-syntax ([start-aux start-aux]
                     [end-aux end-aux])
         #'(goal ... (%= start-aux end-aux)))]))
  
  ;;; Rewrite a subgoal adding additional arguments using auxilliary variables.
  (define (rewrite-subgoal subgoal start-aux end-aux)
    (syntax-parse subgoal
      [(~datum !)
       (with-syntax ([start-aux start-aux]
                     [end-aux end-aux])
         (list subgoal #'(%= start-aux end-aux)))]
      [((~datum %goal) subgoalexpr ...)
        (with-syntax ([start-aux start-aux]
                      [end-aux end-aux])
         #'(subgoalexpr ... (%= start-aux end-aux)))]
      [(subgoalfn arg ...)
       (with-syntax ([start-aux start-aux]
                     [end-aux end-aux])
         (list #'(subgoalfn arg ... start-aux end-aux)))]))
 )

(provide (for-syntax subgoal-count rewrite-clause))
