#lang racket
;;; Define a procedure that takes three numbers
;;; as arguments and returns the sum of the squares of the two
;;; larger numbers

(define (squares-of-the-two-larger num1 num2 num3)
  (cond ((and (< num1 num2) (< num1 num3)) (* num2 num3))
        ((and (< num2 num1) (< num2 num3)) (* num1 num3))
        (else (* num1 * num2))))