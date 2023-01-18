#lang sicp
;; Mapping over trees
(define nil '())
(define (square x) (* x x))


;; Exercise 2.30
(define (square-tree tree)
  ;; Squaring over trees is to square when number and square-tree recursively when tree.
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (square tree)))
       tree))

;; Exercise 2.31
