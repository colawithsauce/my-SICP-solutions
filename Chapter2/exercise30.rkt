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
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

;; Exercise 2.32 - Generate the set of subset of a set.
;; (define (subsets s)
;;   (if (null? s)
;;       (list nil)
;;       (let ((rest (subsets (cdr s))))
;;         (append rest (map subsets rest)))))

;; Explain why it works:
;; - Because if we assume the procedure `(map subsets rest)' would yield correct answer that "the set of the set of subset of the set rest"
;; -- Because it can't works !!! XP

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x))
                          rest)))))

(display (subsets '(1 2 3)))

;; Explain why it works: - Because the subsets of a set is equal to "the subsets
;; of its subset that only excludes its first element" (let's call it rest)
;; append with "rest" in which each subset append the original first element.
