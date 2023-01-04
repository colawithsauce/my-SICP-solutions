#lang sicp

;; Exercise 30 - Another implementation of interative sum.
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;; test
(define (pi-sum a b)
  (define (pi-term x)
          (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
          (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000))

;;;;; Exercise 31: Write a analoguous procedure called `product' that returns the product of the
;; values of a function at points over a given range
;;
;;;; a. write it and caculate the result of PI
;;; PROCEDURE:
(define (product term a next b)
  (define (helper a prod)
    (if (> a b)
        prod
        (helper (next a) (* prod (term a)))))
  (helper a 1))
;; (newline) (display "Exercise 31: ")
;;; CACULATE PI
(define (pi-term x)
  (if (odd? x)
      (/ (+ x 1.0) (+ x 2.0))
      (/ (+ x 2.0) (+ x 1.0))))

(* 4 (product pi-term 1 inc 100))

;;; b. Write it's recursive version.
(define (product-recursive term a next b)
  (if (> a b)
      1
      (* (term a)
       (product-recursive term (next a) next b))))
