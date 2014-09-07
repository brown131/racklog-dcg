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

(require racklog (for-syntax racket/format racket/list))

;;; Define a rule using DCG notation.
#|
(define-syntax (%rule stx)
  (define (make-vars vars goals)
      (with-syntax ([vs vars]
                    [gs goals])
        #'(append (for/list ([i (in-range (sub1 (apply max (for/list ([g gs]) (length g)))))]) 
                              (string->symbol (~a "s" i))) '(s) vs)))
  (syntax-case stx ()
    [(_ (v ...) ((a ...) subgoal ...) ...)
     (with-syntax ([vars #'(v ...)]
                   [goals (syntax->datum #'(((a ...) subgoal ...) ...))]
                   )
   #|    (let* ([subgoals (for/list ([g goals])
                          (cons (append '(s0 s) (first g)) 
                                (for/list ([sg (rest g)]
                                           [i (in-range (length (rest g)))])
                                  (append sg (list (string->symbol (~a "s" i))
                                                   (if (= i (sub1 (length (rest g)))) 's 
                                                       (string->symbol (~a "s" (add1 i)))))))))])|#
         #'(%rel (make-vars vars goals)))]))
  ;(append '(%rel) (list vars) subgoals)))]))
|#
;;; Define a rule using DCG notation.
(define-syntax %rule-test
  (syntax-rules ()
    [(_ (v ...) ((a ...) subgoal ...) ...)   
     (eval-syntax #`(%rel #,(append (for/list ([i (in-range (sub1 (apply max (for/list ([g '(((a ...) subgoal ...) ...)]) 
                                                                                 (length g)))))]) 
                                        (string->symbol (~a "s" i))) '(s v ...))
                            #,(let ([nsgs (length '(subgoal ...))])
                                (cons '(s0 s a ...) 
                                      (for/list ([sg '(subgoal ...)]
                                                 [i (in-range nsgs)])
                                        (append sg (list (string->symbol (~a "s" i)) 
                                                         (if (= i (sub1 nsgs)) 's 
                                                             (string->symbol (~a "s" (add1 i))))))))) ...))]))

(define-syntax (%rule stx)
  (syntax-case stx ()
    [(_ (v ...) ((a ...) subgoal ...) ...)
     (with-syntax ([gls (for/list ([g (syntax->list #'(((a ...) subgoal ...) ...))]) (length (syntax->list g)))])
       (with-syntax ([rule (append '(%rel) 
                                   (list (append (for/list ([i (in-range (sub1 (apply max (syntax->list #'gls))))]) 
                                                   (string->symbol (~a "s" i))) '(s) #'(v ...)))
                                   (for/list ([g (syntax->list #'(((a ...) subgoal ...) ...))])
                                     (cons (append '(s0 s) (first g)) 
                                           (for/list ([sg (rest g)]
                                                      [i (in-range (length (rest g)))])
                                             (append sg (list (string->symbol (~a "s" i))
                                                              (if (= i (sub1 (length (rest g)))) 's 
                                                                  (string->symbol (~a "s" (add1 i))))))))))])
         #'rule))]))

;;; Define a terminal using DCG notation. (PAIP uses :word)
(define-syntax (%term stx)
  (syntax-case stx ()
    [(%term (t ...) ...) 
     #'(%rel (x) [((append '(t ...) x) x)] ...)]))

;;; Ordinary Racklog goal. Is not translated. Same as braces in Prolog. (PAIP uses ":test")
(define %goal 1)

;;; Translate a DCG rule or terminal into ordinary Racklog. Used for %assert!.
(define %translate-rule 1)

(provide %rule-test %rule %term %goal %translate-rule)
