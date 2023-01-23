#lang sicp
;;; Representing Sets
;; Sets as unorderd lists
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

;; Exercise 2.59 - Union set
(define (union-set a b)
  (cond ((null? a) b)
        ((null? b) a)
        ((element-of-set? (car a) b) (union-set (cdr a) b))
        (else (cons (car a) (union-set (cdr a) b)))))
