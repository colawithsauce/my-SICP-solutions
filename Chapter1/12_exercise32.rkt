#lang sicp

;; Exersice 32 - Much higher order procedure, recursive version
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

;; Iterative version
(define (acc combiner null-value term a next b)
  (define (acc-helper a ACC)
    (if (> a b)
        ACC
        (acc-helper (next a) (combiner ACC (term a)))))
  (acc-helper a null-value))
