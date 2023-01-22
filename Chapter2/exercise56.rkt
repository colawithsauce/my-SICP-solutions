#lang sicp
;; Symbolic differentiation
(define (=number? x a) (and (number? x) (= x a)))

(define (exp b n)
  (define (exp-helper a b n) ;; The fundmental idea is simple, a*b^n = constant.
    (cond ((= n 0) a)
          ((even? n) (exp-helper a (* b b) (/ n 2)))  ;; * when n is even, a*b^n = a*(b^{n/2})^2 = a* b^2^{n/2} *
          (else (exp-helper (* a b) b (- n 1)))))  ;; when n is odd, a*b^n = (a*b)*b^{n-1}
  (exp-helper 1 b n))

;; Symbolic derive
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
         (list '+ a1 a2))))
(define (product? x) (eq? '* (car x)))
(define multiplier cadr)
(define multiplicand caddr)
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else
         (list '* m1 m2))))

;; (define (deriv exp var)
;;   (cond ((number? exp) 0)
;;         ((variable? exp) (if (same-variable? exp var) 1 0))
;;         ((sum? exp) (make-sum (deriv (addend exp) var)
;;                               (deriv (augend exp) var)))
;;         ((product? exp)
;;          (make-sum
;;           (make-product (multiplier exp)
;;                         (deriv (multiplicand exp) var))
;;           (make-product (multiplicand exp)
;;                         (deriv (multiplier exp) var))))
;;         (else
;;          (error "unkown expression type: DERIV" exp))))

;; Exercise 56 - Implement symbolic derive of exponentiation.
(define (expon? z) (eq? '** (car z)))
(define base cadr)
(define exponent caddr)
(define (make-expon x y)
  (cond ((and (number? x) (number? y)) (exp x y))
        ((=number? y 0) 1)
        ((=number? x 0) 0)
        ((=number? x 1) 1)
        ((=number? y 1) x)
        (else
         (list '** x y))))              ; REVIEW: Had made mistake here, wrote infinitive recursive.

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
        ((expon? exp)
         (let ((base (base exp))
               (exponent (exponent exp)))
           (make-product (deriv base var)
                         (make-product (make-expon base (make-sum exponent -1))
                                       exponent))))
        (else
         (error "unkown expression type: DERIV" exp))))
