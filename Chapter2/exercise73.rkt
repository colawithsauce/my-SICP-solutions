#lang sicp
;; 2.73 - Rewrite symbolic deriv in apply-generic style.

;; (define (deriv exp var)
;;   (cond ((number? exp) 0)
;;         ((variable? exp) (if (same-variable? exp var) 1 0))
;;         (else ((get 'deriv (operator exp))
;;                (operands exp) var))))

;; 2.73 - 2.77 skipped
