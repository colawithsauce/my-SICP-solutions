#lang racket
;; Recursive solution is easy.
(define (f-recursive n)
  (if (< n 3)
      n
      (+ (f-recursive (- n 1))
         (* 2 (f-recursive (- n 2)))
         (* 3 (f-recursive (- n 3))))))

;; Iterative solution
;; Solution from: http://community.schemewiki.org/?sicp-ex-1.11
;; When writting iter solutions, use count rather than original 'n' is better.
(define (f n)
  (define (f-iter fi-3 fi-2 fi-1 count)
    (cond ((= count 0) fi-3)
          (else (f-iter fi-2 fi-1 (+ fi-1 (* 2 fi-2) (* 3 fi-3)) (- count 1)))))
  (if (< n 3)
      n
      (f-iter 0 1 2 n)))