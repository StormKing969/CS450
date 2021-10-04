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
(require "hw5-util.rkt")
(require rackunit)
(provide d:eval-exp d:eval-term)
(define (d:apply-arg1 app)
  (first (d:apply-args app)))
(define (d:lambda-param1 lam)
  (first (d:lambda-params lam)))
;; END OF REQUIRES

;; Exercise 1
(define/contract (d:eval-exp mem env exp)
  (-> mem? handle? d:expression? eff?)
  (cond [(d:value? exp) (eff mem exp)]
        [(d:variable? exp) (eff mem (environ-get mem env exp))]
        [(d:lambda? exp) (eff mem (d:closure env exp))]
        ))

;; Exercise 2
(define/contract (d:eval-term mem env term)
  (-> mem? handle? d:term? eff?)
  'todo)

;; Exercise 3 (Manually graded)
#|
A difference between our Language and Racket would be the use of the computer
memory. Our language is not designed to be efficient in its use of memory. As
such, the longer the program runs, the harder it will be for our language to
process it and might eventually crash.
|#
