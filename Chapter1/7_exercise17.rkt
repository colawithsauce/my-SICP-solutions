#lang racket
(define (multiply a b)
  (define (double a) (* a 2))
  (define (halve a) (/ a 2))
  (cond ((= b 1) a)
        ((even? b) (multiply (double a) (halve b))) ;; a * b = (a*2)*(b/2)
        (else (+ a (multiply a (- b 1))))))  ;; a * b = a + a * (b - 1) 

;; The upper is recursive
;; to write a iterative procedure, we need have another variable to save the product
(define (fast-mult a b)
  (define (double a) (* a 2))
  (define (halve a) (/ a 2))
  (define (helper a b product)
    (cond ((= b 0) product)
          ((even? b) (helper (double a) (halve b) product))
          (else (helper a (- b 1) (+ a product)))))
  (helper a b 0))

;; The thought here is fundmental, b must once be 1, which follow the odd branch