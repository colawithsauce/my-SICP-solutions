#lang racket
;; Better Newton's square root algorithm.
(define (sqrt x) (sqrt-iter 1.0 2.0 x))

(define (sqrt-iter guess last-guess x)
  (if (good-enough? guess last-guess)
      guess
      (sqrt-iter (improve guess x) guess x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (square x) (* x x))
;; original 
;; (define (good-enough? guess former-guess)
;;  (< (abs (- (square guess) x)) 0.0001))

;; new definition of good-enough?
(define (good-enough? guess last-guess)
  (< (abs(- guess last-guess)) 0.00001))

;;; CUBE root - exercise 1.8
(define (cube-root x)
  (cube-iter 1.0 2.0 x))

(define (cube-iter guess last-guess x)
  (if (good-enough? guess last-guess)
      guess
      (cube-iter (cube-improve guess x) guess x)))

(define (cube-improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))