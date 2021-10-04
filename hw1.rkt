#lang racket
(require rackunit)
;; Please, do not remove this line and do not change the function names,
;; otherwise the grader will not work and your submission will get 0 points.
(provide (all-defined-out))

(define ex1 (/ (+ 12 2) (* (- 9 6) (* 15 5))))
(define ex2 (list(/ (+ 12 2) (* (- 9 6) (* 15 5)))(/ 14 (* (- 9 6) (* 15 5)))(/ 14 (* 3 (* 15 5)))(/ 14 (* 3 75))(/ 14 225)14/225))
(define (ex3 x y)  (> (* (* x y) y) (+ (* x 5) (- x 10))) )

;; Constructs a tree from two trees and a value
(define (tree left value right) (list left value right))
;; Constructs a tree with a single node
(define (tree-leaf value) (list null value null))

;; Accessors
(define (tree-left self) (first self))
(define (tree-value self) (second self))
(define (tree-right self) (third self))

;; Copies the source and updates one of the fields
(define (tree-set-value self value) (list (tree-left self) value (tree-right self)))
(define (tree-set-left self left) (list left (tree-value self) (tree-right self)))
(define (tree-set-right self right) (list (tree-left self) (tree-value self) right))

;; Function that inserts a value in a BST
(define (bst-insert self value) (cond [(equal? self null) (list null value null)]
                                      [(equal? value (tree-value self)) (tree-set-value self value)]
                                      [(< value (tree-value self)) (tree-set-left self (bst-insert (tree-left self) value))] 
                                      [else (tree-set-right self (bst-insert (tree-right self) value))]))

;; lambda
(define (lambda? node) (cond [(number? node) #f]
                             [(<= (length node) 2) #f]
                             [(and (equal? (first node) 'lambda) (not (symbol? (second node))) (not (number? (second node))) (andmap symbol? (second node))) #t]
                             [else #f]))

(check-true (lambda? (quote (lambda (x) x))))
(check-false (lambda? (quote 1)))
(check-false (lambda? (quote (lambda (10)))))
(check-false (lambda? (quote (lambda (2) 	()))))
(check-true (lambda? (quote (lambda (x y z) 52))))
(check-false (lambda? (quote (lambda (x 7 z) 5))))
(check-false (lambda? (quote (lambda (x)))))
(check-false (lambda? (quote (- 9 1))))
(check-true (lambda? (quote (lambda (a b c) x y z))))
(check-true (lambda? '(lambda (x) '())))
(check-true (lambda? '(lambda () (+ 1 32))))
(check-true (lambda? '(lambda (x) (+ 11 72))))
(check-false (lambda? '(lambda ())))
(check-false (lambda? '(define ())))
(check-false (lambda? (quote 7)))
(check-false (lambda? '(lambda x 2)))
(check-false (lambda? '(lambda x 5)))
(check-false (lambda? '(lambda 4 5)))
(check-false (lambda? '(lambda 1 '())))

(define (lambda-params node) (and (equal? (first node) 'lambda) (second node)))

(define (lambda-body node) (rest(rest node)))

(check-equal? '() (lambda-body (quote (lambda (x) ))))
(check-true (equal?  (list 1) (lambda-body (quote (lambda (x) 1)))))
(check-true (equal?  (list 'y) (lambda-body (quote (lambda (x) y)))))
(check-equal? '((y)) (lambda-body (quote(lambda (x) (y)))))
(check-equal?  '('(x y z)) (lambda-body (quote(lambda (x) '(x y z)))))

;; apply
(define (apply? l) (cond [(equal? l null) #f]
                         [(not (list? l)) #f]
                         [(number? (first l)) #t]
                         [(equal? (first l) 'define) #f]
                         [(not (equal? (first l) 'lambda))]
                         [else #f]))

(check-true (apply? (quote (3 x))))
(check-true (apply? (quote (x 5))))
(check-true (apply? (quote (a b))))
(check-false (apply? (quote 8)))
(check-false (apply? (quote ())))
(check-false (apply? (quote (define))))
(check-false (apply? (quote (lambda (x) x))))

(define (apply-func node) (cond [(symbol? (first node)) first (first node)]))

(check-equal? 'x (apply-func (quote (x y))))

(define (apply-args node) (cond [(symbol? (first node)) first (rest node)]))

(check-equal? (list 'y) (apply-args (quote (x y))))

;; define
(define (define? node) (cond [(and (>= (length node) 3) (equal? (first node) 'define) (not (number? (second node))) (symbol? (second node)))]
                             [(and (>= (length node) 3) (not (null? (second node)))
                                  (equal? (first node) 'define)
                                  (list? (second node))
                                  (andmap symbol? (second node))
                                  (not (null? (third node))))]
                             [else #f]))


(check-true (define? (quote (define x 11))))
(check-false (define? (quote (void))))
(check-false (define? (quote (define))))
(check-false (define? (quote (define (void)))))
(check-false (define? (quote (define ()))))
(check-false (define? (quote (define x))))
(check-false (define? (quote (define -))))
(check-false (define? (quote (define 1))))
(check-false (define? (quote (define car))))
(check-false (define? (quote (define define))))
(check-false (define? (quote (define 10 x))))
(check-true (define? (quote (define (f x) (+ x 1)))))

(define (define-basic? node) (cond [(equal? (first node) null) #f]
                                   [(<= (length node) 2) #f]
                                   [(and (equal? (first node) 'define) (symbol? (second node)))]
                                   [else #f]))

(check-false (define-basic? '(define (x) 13)))
(check-true (define-basic? '(define x x)))
(check-false (define-basic? '(define (x) '())))
(check-false (define-basic? '(define '() x)))
(check-false (define-basic? '(define (2) 4)))
(check-true (define-basic? '(define car 6)))
(check-true (define-basic? '(define - 4)))
(check-true (define-basic? '(define sdfe 8)))

(define (define-func? node) (and (>= (length node) 3) (not (null? (second node)))
                                  (equal? (first node) 'define)
                                  (list? (second node))
                                  (andmap symbol? (second node))
                                  (not (null? (third node)))))

(check-true (define-func? (quote (define (x) 15))))
(check-false (define-func? (quote (define (x)))))
(check-false (define-func? (quote (define (7) 7))))
(check-false (define-func? (quote (define () 44))))
(check-false (define-func? (quote (define))))
(check-false (define-func? (quote (define (x y 6) 6))))
(check-true (define-func? '(define (a b c) x y z)))
(check-true (define-func? '(define (x) '())))
(check-true (define-func? '(define (f x y z) (+ 1 32))))

(check-true (define-func? (quote (define (x) 3))))
(check-false (define-func? (quote (define x 3))))
(check-true (define-func? (quote (define (x y) 3 2))))
(check-true (define-func? (quote (define (x y z a b) 3 2 1 4 5))))
(check-false (define-func? (quote (define (x)))))
(check-true (define-func? (quote (define (x) 1 2))))
(check-false (define-func? (quote (define () 3))))
(check-false (define-func? (quote (define))))
(check-false (define-func? (quote (define (x y 3) 3))))
(check-true (define-func? (quote (define (x) 3))))
(check-false (define-func? (quote (define (x)))))
(check-false (define-func? (quote (define (3) 3))))