#lang racket
;;;; Racklog-DCG
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
       (- (length (syntax->list #'(subgoal ...)))
          (if (find-terminal #'(subgoal ...)) 1 0))]))

  ;;; Rewrite the subgoals in a clause to use auxilliary variables.
  ;;; a) (subgoal) => (subgoal s1 s2)
  ;;; b) ! => ! (%= s1 s2)
  ;;; c) (%goal (goal1) (goal2)) => (goal1) (goal2) (%= s1 s2)
  
  ;;; TODO: 
  ;;; 1. (list x) => (append 'x s0) -- check all contexts of this 
  ;;; 2. ! => ! (%= s1 s2)
  ;;; 3. (%goal (goal1) (goal2) => (goal1) (goal2) (%= s1 s2)
  ;;; 4. Possibly support curly braces instead of, or in addition to, %goal.
  (define (rewrite-clause clause all-aux-vars)
    (define aux-vars (take all-aux-vars (add1 (subgoal-count clause))))
    (syntax-case clause ()
      [((var-id ...) subgoal ...)
       (with-syntax
           ([(var ...)
             (if (find-terminal #'(subgoal ...)) 
                 ;; Has a terminal subgoal
                 (append (syntax->list #'(var-id ...))
                         (list (list 'append (find-terminal #'(subgoal ...)) (first aux-vars))))
                 ;; Normal subgoals
                 (append (syntax->list #'(var-id ...)) (take aux-vars 1)))]
            [(new-subgoal ...) 
             (foldl (λ (sg s e l) (append l (rewrite-subgoal sg s e))) null
                    ;; Subgoals
                    (if (find-terminal #'(subgoal ...)) 
                        (rest (syntax->list #'(subgoal ...)))
                        (syntax->list #'(subgoal ...))) 
                    ;; Starting auxilliary vars
                    (take all-aux-vars (subgoal-count clause))
                    ;; Ending auxilliary vars
                    (take (rest all-aux-vars) (subgoal-count clause)))]
            [start-aux (first aux-vars)]
            [end-aux (last aux-vars)])
         #'((var ... end-aux) new-subgoal ...))]))

  ;;; Find a terminal in a list of subgoals.
  (define (find-terminal subgoals)
    (syntax-case subgoals ()
      [(subgoal ...)
       (findf (λ (sg) (if (and (list? (syntax-e sg))
                               (or (eq? (syntax-e (first (syntax->list sg))) 'list)
                                   (eq? (syntax-e (first (syntax->list sg))) 'quote))) sg #f)) 
              (syntax->list #'(subgoal ...)))]))
  
  ;;; Rewrite a subgoal adding additional arguments using auxilliary variables.
  (define (rewrite-subgoal subgoal start-aux end-aux)
    (syntax-parse subgoal
      ;; Cut
      [(~datum !)
       (with-syntax ([start-aux start-aux]
                     [end-aux end-aux])
         (list subgoal #'(%= start-aux end-aux)))]
      ;; Terminal
      [((~or (~datum list) (~datum quote)) subgoalexpr ...)
       (with-syntax ([start-aux start-aux]
                     [subgoal subgoal]
                     [end-aux end-aux])
         (list #'(%= start-aux (append subgoal end-aux))))]
      ;; Normal Goal
      [((~datum %goal) subgoalexpr ...)
        (with-syntax ([start-aux start-aux]
                      [end-aux end-aux])
         #'(subgoalexpr ... (%= start-aux end-aux)))]
      ;; DCG Goal
      [(subgoalfn arg ...)
       (with-syntax ([start-aux start-aux]
                     [end-aux end-aux])
         (list #'(subgoalfn arg ... start-aux end-aux)))]))
 )

(provide (for-syntax subgoal-count rewrite-clause))
