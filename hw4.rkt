#|
    ===> PLEASE DO NOT DISTRIBUTE THE SOLUTIONS PUBLICLY <===

   We ask that solutions be distributed only locally -- on paper, on a
   password-protected webpage, etc.

   Students are required to adhere to the University Policy on Academic
   Standards and Cheating, to the University Statement on Plagiarism and the
   Documentation of Written Work, and to the Code of Student Conduct as
   delineated in the catalog of Undergraduate Programs. The Code is available
   online at: http://www.umb.edu/life_on_campus/policies/code/

|#
;; PLEASE DO NOT CHANGE THE FOLLOWING LINES
#lang racket
(provide (all-defined-out))
(require rackunit)
(require "hw4-util.rkt")
;; END OF REQUIRES

;; Utility functions
(define (s:apply-arg1 app)
  (first (s:apply-args app)))
(define (s:lambda-param1 lam)
  (first (s:lambda-params lam)))
(define (s:lambda-body1 lam)
  (first (s:lambda-body lam)))

;; Utility functions
(define (e:apply-arg1 app)
  (first (e:apply-args app)))
(define (e:lambda-param1 lam)
  (first (e:lambda-params lam)))
(define (e:lambda-body1 lam)
  (first (e:lambda-body lam)))

;; Exercise 1
(define (s:subst exp var val)
  (cond [(s:number? exp) exp]
        [(and (s:variable? exp) (equal? exp var)) val]
        [(and (s:variable? exp) (not (equal? exp var))) exp]
        [(equal? (s:lambda-param1 exp) var) exp]
        [(not (equal? (s:lambda-param1 exp) var))
          (s:lambda (list (s:lambda-param1 exp)) (list (s:subst (s:lambda-body1 exp) var val)))]
        [else (s:apply (list (s:apply-func exp)) (list (s:subst (s:apply-arg1 exp) var val)))]))

;; Exercise 2
(define (s:eval subst exp)
  (cond [(s:value? exp) exp]))

;; Exercise 3
(define (e:eval env exp)
  (cond [(e:value? exp) exp]
        [(e:variable? exp) (hash-ref env exp)]
        [(e:lambda? exp) (e:closure env exp)]))
 
;; Exercise 4 (Manually graded)
#|
Implementing without environment :-

When you are doing simply functions. To be more precise implementing without an
environment is only useful when we dont have to go through the entire function to
get the answer

Implementing with environment :-

It is usefully when you are working with complicated functions like substitution. The
ones where going throught the entire function is needed to obtain the answer. 
|#

;; Exercise 5 (Manually graded)
#|
- Provides insights and understanding of the software requirements and software design

- It helps to prove that the program conforms to its specifications and also help
with the creation of software tools that can assist with the debugging and understanding 
|#
