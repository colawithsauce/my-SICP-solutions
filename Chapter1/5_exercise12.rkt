#lang racket
;; Recursive solution
(define (pascal line n)
  (cond ((or (> n line) (< n 0)) 0)
        ((or (= n 1) (= n line)) 1)
        (else (+ (pascal (- line 1) (- n 1)) (pascal (- line 1) n)))))