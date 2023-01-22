#lang sicp
;; TODO - Unimplemented, too hard, give up.
(define (=number? x a) (and (number? x) (= x a)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-till sym op initial sequence)
  (if (or (null? sequence)
          (eq? sym (car sequence)))
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;; Exercise 2.58 - Supporting of in-order math expressions.
;; (b)
(define variable? symbol?)
(define same-variable? eq?)

(define (sum? x) (and (pair? x) (memq '+ x) (not (memq '* x)))) ; Is sum if is not product.
(define (addend x)
  (accumulate-till '+ cons nil x))
(define (augend x)
  (accumulate make-sum 0 (filter (lambda (x) (not (eq? x '+))) (memq '+ x))))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else
         (list a1 '+ a2))))

(define (product? x) (and (pair? x) (memq '* x)))
(define multiplier car)
(define (multiplicand x)
  (accumulate make-product 1 (cddr x)))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else
         (list m1 '* m2))))




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
