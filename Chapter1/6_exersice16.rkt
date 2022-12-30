#lang racket
;; Design a procedure that evolves an iterative exponentiation process that uses sussessive squaring
;; and uses a logarithmic number of steps, as does fast-expt.

;; Can't solve it independently. whatch solution at http://community.schemewiki.org/?sicp-ex-1.16
(define (exp b n)
  (define (exp-helper a b n) ;; The fundmental idea is simple, a*b^n = constant.
    (cond ((= n 0) a)
          ((even? n) (exp-helper a (* b b) (/ n 2)))  ;; * when n is even, a*b^n = a*(b^{n/2})^2 = a* b^2^{n/2} *
          (else (exp-helper (* a b) b (- n 1)))))  ;; when n is odd, a*b^n = (a*b)*b^{n-1}
  (exp-helper 1 b n))
  