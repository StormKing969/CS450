#lang racket
#|
            #####################################################
            ###  PLEASE DO NOT DISTRIBUTE SOLUTIONS PUBLICLY  ###
            #####################################################

  Copy your solution of HW1 as file "hw1.rkt". The file should be in the same
  directory as "hw2.rkt" and "ast.rkt".
|#
(require "ast.rkt")
(require "hw1.rkt")
(require rackunit)
(provide (all-defined-out))
;; ^^^^^ DO NOT CHANGE ANY CODE ABOVE THIS LINE ^^^^^


;; Exercise 1.a: Read-write cell
;; Solution has 3 lines.
(define (rw-cell x) 
  (define (view_update m)
    (cond [(empty? m) x]
          [(not (empty? m)) (define x (car m)) (rw-cell x)]))
  view_update)

;; Exercise 1.b: Read-only cell
;; Solution has 4 lines.
(define (ro-cell x)
  (define (view_only m)
    (cond [(empty? m) x]
          [(not (empty? m)) (ro-cell x)])) view_only)

;; Exercise 2: Interperse
;; Solution has 11 lines.
(define (intersperse l v)
  
  (define (creation element-one v) (flatten (append (list element-one) (list v))))
  
  (define (new-list first-element old-l)
    
    (define final-list (creation first-element (list (first old-l) v)))
      (cond [(= (length (rest old-l)) 1) (flatten (append (list final-list) (rest old-l)))]
            [else (new-list final-list (rest old-l))]))
  
  (cond [(or (empty? l) (equal? (length l) 1)) l]
        [else (new-list '() l)]))

;; Exercise 3.a: Generic find
;; Solution has 7 lines.
(define (find pred l) (define (counts a x counter) (cond [(eq? counter 0) #f]
                                                         [(eq? x #f) #f]
                                                         [(null? a) (cons counter x)]
                                                         [(eq? (first a) x) (counts (rest a) x counter)]
                                                         [(not (eq? (first a) x)) (counts (rest a) x (- counter 1))]))

  (cond [(empty? l) #f]
        [else (counts l pred (length l))]))

(check-equal? (cons 0 10) (find (lambda (idx elem) #t) (list 10 20 30)))

;; Exercise 3.b: Member using find
;; Solution has 3 lines.
(define (member x l) (define (checking a x) (cond [(null? a) #f]
                                                  [(eq? (first a) x) #t]
                                                  [(not (eq? (first a) x)) (checking (rest a) x)]
                                                  [else #f]))

  (cond [(empty? l) #f]
        [else (checking l x)]))

;; Exercise 3.c: index-of using find
;; Solution has 4 lines.
(define (index-of l x) (define (counts a x counter) (cond [(eq? counter 0) #f]
                                                          [(null? a) counter]
                                                          [(eq? (first a) x) (counts (rest a) x counter)]
                                                          [(not (eq? (first a) x)) (counts (rest a) x (- counter 1))]))

  (cond [(empty? l) #f]
        [else (counts l x (length l))]))

;; Exercise 4: uncurry, tail-recursive
;; Solution has 8 lines.
(define (uncurry f) 'todo)

;; Exercise 5: Parse a quoted AST
;; Solution has 26 lines.
(define (parse-ast node)
  (define (make-define-func node) (r:define (r:variable (first (define-head node)))
                                            (r:lambda (map parse-ast (rest (define-head node)))
                                                      (map parse-ast (define-body node)))))
  (define (make-define-basic node) (r:define (parse-ast (define-head node)) (parse-ast (third node))))
  (define (make-lambda node) (r:lambda (map parse-ast (lambda-params node)) (map parse-ast (lambda-body node))))
  (define (make-apply node) (r:apply (parse-ast (first node)) (map parse-ast (apply-args node))))
  (define (make-number node) (r:number node))
  (define (make-variable node) (r:variable node))

  (cond
    [(define-basic? node) (make-define-basic node)]
    [(define-func? node) (make-define-func node)]
    [(symbol? node) (make-variable node)]
    [(real? node) (make-number node)]
    [(lambda? node) (make-lambda node)]
    [else (make-apply node)]))
