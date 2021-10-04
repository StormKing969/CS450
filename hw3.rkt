#lang racket
#|
    ===> PLEASE DO NOT DISTRIBUTE SOLUTIONS NOR TESTS PUBLICLY <===

   We ask that solutions be distributed only locally -- on paper, on a
   password-protected webpage, etc.

   Students are required to adhere to the University Policy on Academic
   Standards and Cheating, to the University Statement on Plagiarism and the
   Documentation of Written Work, and to the Code of Student Conduct as
   delineated in the catalog of Undergraduate Programs. The Code is available
   online at: http://www.umb.edu/life_on_campus/policies/code/

|#
(require rackunit)
(require "ast.rkt")
(provide (all-defined-out))

;; Exercise 1.a
(define p:empty (delay empty))

;; Exercise 1.b
(define (p:empty? p) (cond [(and (promise? p) (list? (force p))) #t]
                           [else #f]))

;; Exercise 1.c
#|
Yes we can

(define (p:cons x l) (cond [(p:empty? l) (delay (cons x p:empty))]
                           [else (delay (cons x (delay l p:empty)))]))

(check-equal? (car (force (p:cons 1 2) )) 1)
|#

;; Exercise 1.d
(define (p:first l) (car (force l)))

;; Exercise 1.e
(define (p:rest l) (cdr (force l)))

;; Exercise 1.f
(define (p:append l1 l2) (cond [(p:empty? l1) l2]
                               [else (delay (cons (p:first l1) (p:append (p:rest l1) l2)))]))

;; Exercise 2.a
;; Auxiliary functions
(define (tree-left self) (first self))
(define (tree-value self) (second self))
(define (tree-right self) (third self))
#|
(define (bst->list self)
  (cond [(empty? self) self]
        [else
         (append
           (bst->list (tree-left self))
           (cons (tree-value self)
                 (bst->list (tree-right self))))]))
|#
(define (bst->p:list self)
  (cond [(p:empty? self) self]
        [else (p:append
               (bst->p:list (first (force self)))
               (cons (second (force self))
                     (bst->p:list (third (force self)))))]))

;; Exercise 2.b
#|
The only time that I can see the lazy evaluation to be better than the eager evaluation
is when the list the function is supposed to process is long or even there are parts
of the evaluation that can be skipped (eager evaluation does each and every step of the
function vs lazy leaves everything to the end thus possible skipping the redundant parts)
|#

;; Exercise 3
;; Auxiliary functions
(define (stream-get stream) (car stream))
(define (stream-next stream) ((cdr stream)))
(define (stream-foldl f a s)
  (define (stream-foldl-iter final-list old-list) (f final-list (thunk (stream-foldl-iter (f (stream-get old-list) final-list) (stream-next old-list)))))
  
  (stream-foldl-iter a s)) 

;; Exercise 4
(define (stream-skip n s)
  (define (stream-skip-iter num_skip squence_s num_skipped)
    
    (cond [(equal? num_skipped num_skip) squence_s]
          [else (stream-skip-iter num_skip (stream-next squence_s) (+ 1 num_skipped))]))
  
  (stream-skip-iter n s 0))

;; Exercise 5
(define (r:eval-builtin sym)
  (cond [(equal? sym '+) +]
        [(equal? sym '*) *]
        [(equal? sym '-) -]
        [(equal? sym '/) /]
        [(equal? sym (not #f)) #t]
        [else #f]))

(define (r:eval-exp exp)
  (cond
    ; 1. When evaluating a number, just return that number
    [(r:number? exp) (r:number-value exp)]
    ; 2. When evaluating an arithmetic symbol,
    ;    return the respective arithmetic function
    [(r:variable? exp) (r:eval-builtin (r:variable-name exp))]
    ; 3. When evaluating a function call evaluate each expression and apply
    ;    the first expression to remaining ones
    [(r:apply? exp)
     ((r:eval-exp (r:apply-func exp))
      (r:eval-exp (first (r:apply-args exp)))
      (r:eval-exp (second (r:apply-args exp))))]
    [(r:bool? exp) (r:eval-builtin (r:bool-value exp))]
    [else (error "Unknown expression:" exp)]))

(struct r:bool (value) #:transparent)