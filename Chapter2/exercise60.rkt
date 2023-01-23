#lang sicp

;; Exercise 2.60 - Union with duplicated
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (union-set a b)
  (cond ((null? a) b)
        ((null? b) a)
        (else (cons (car a) (union-set (cdr a) b)))))

(define (intersection-set a b)
  (cond ((null? a) '())
        ((null? b) '())
        ((element-of-set? (car a) b) (cons (car a) (intersection-set (cdr a) b)))
        (else (intersection-set (cdr a) b))))

;; Because the element of the set can be duplicated, so my solution is correct. - I thought.
