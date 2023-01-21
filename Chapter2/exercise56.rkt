#lang sicp
;; Symbolic differentiation
(define (=number? x a) (and (number? x) (= x a)))

;; Exercise 56 - Implement symbolic derive of exponentiation.
(define variable? symbol?)
(define same-variable? eq?)
(define (sum? x) (eq? '+ (car x)))
(define addend cadr)
(define augend caddr)
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else
         `(+ ,a1 ,a2))))
(define (product? x) (eq? '* (car x)))
(define multiplier cadr)
(define multiplicand caddr)
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else
         `(* ,m1 ,m2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        (else
         (error "unkown expression type: DERIV" exp))))

(deriv '(+ x (* x y)) 'x)
