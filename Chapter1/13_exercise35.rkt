#lang sicp

;; Half interval method to find root of equation.
(define (average a b) (/ (+ a b) 2))
(define (close-enough? a b) (< (abs (- a b)) 0.0001))
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are must of opposite sign" a b)))))
(half-interval-method sin 2.0 4.0)

;; reminiscent, converge, oscillating, damping

;; Finding fixed point
;; (define tolerance 0.000001)
;; (define (fixed-point f first-guess)
;;   (define (close-enough? v1 v2)
;;     (< (abs (- v1 v2))
;;        tolerance))
;;   (define (try guess)
;;     (let ((next (f guess)))
;;       (if (close-enough? guess next)
;;           next
;;           (try next))))
;;   (try first-guess))

;; (fixed-point cos 1.0)

;;;;; Exercise 35 - show \phi
;; (newline)
;; (display "Ex35: \\phi = ")
;; (display (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
;; (newline)


;; Exercise 36
(newline)
(display "Exercise 36")
(newline)
(define tolerance 0.0001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 10.0)


;; Exercise 37

;; Recursive version
(define (cont-frac n d k)
  (define (cf-helper n d k i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (cf-helper n d k (- i 1))))))
  (cf-helper n d k 1))

;; Iterative version
(define (cont-frac-i n d k)
  (define (cf-helper n d k rst)
    (if (= k 0)
        rst
        (cf-helper n d (- k 1) (/ (n k) (+ (d k) rst)))))
  (cf-helper n d k 0))
