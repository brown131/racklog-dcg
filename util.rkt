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

(require racklog (for-syntax racket/list))

(begin-for-syntax   

  ;;; Count of the number of subgoals in a clause.
  (define (aux-subgoal-count clause)
    (syntax-case clause ()
      [((var-id ...) subgoal ...)
       ;(length (syntax->list #'(subgoal ...)))]))
       (count (λ (x) (not (eq? (syntax->datum x) '!))) (syntax->list #'(subgoal ...)))]))

  ;;; Rewrite the subgoals in a clause to use auxilliary variables.
  (define (rewrite-clause clause all-aux-vars)
    (define aux-vars (take all-aux-vars (add1 (aux-subgoal-count clause))))
    (syntax-case clause ()
      [((var-id ...) subgoal ...)
       (with-syntax
           ([(new-subgoal ...)
             (let ([start-idx -1])
               (for/list ([subgoal (in-list (syntax->list #'(subgoal ...)))])
                 (if (eq? (syntax->datum subgoal) '!) subgoal
                     (let ()
                       (set! start-idx (add1 start-idx))
                       (rewrite-subgoal subgoal (list-ref aux-vars start-idx) 
                                        (list-ref aux-vars (add1 start-idx)))))))]
            [start-aux (first aux-vars)]
            [end-aux (last aux-vars)])
         #'((var-id ... start-aux end-aux) new-subgoal ...))]))
  
  ;;; Rewrite a subgoal adding additional arguments using auxilliary variables.
  (define (rewrite-subgoal subgoal start-aux end-aux)
    (syntax-case subgoal ()
      [(subgoalfn arg ...)
       (with-syntax ([start-aux start-aux]
                     [end-aux end-aux])
         #'(subgoalfn arg ... start-aux end-aux))]))
 )

(provide (for-syntax aux-subgoal-count rewrite-clause))
