#lang sicp

;; Chapter 1.3 procedure as argument
;; the sum function, recursive version
(define (sum-rec term a next b)
  (if (> a b)
     0
     (+ (term a)
       (sum-rec term (next a) next b))))

;; The former recursive version is convinent to be adapted to it's iterative
;; version. All need is to change the "plus" into the procedure itself, which
;; can implemented by introducing a helper procedure.
(define (sum term a next b)
  (define (sum-helper term a next b product)
    (if (> a b)
       product
       (sum-helper term (next a) next b (+ product (term a)))))
  (sum-helper term a next b 0))

;; test it with sum numbers
(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(sum-integers 1 100) ; -> 5500

;; Simpson's method, I once thought the n should be the footer of each y, but
;; actually it should be a fixed number, which is the scale of problem.
;;
;;;;   MY FAILED SOLUTION:
;;
;; (define (simpson-integral f a b n)
;;   (define h (/ (- b a) n))

;;   ;; each time add \sigma * f(a + kh), where \sigma equals 0 when k = 0, 2 when k is even, 4 when odd.
;;   (define (term x)
;;     (define k (/ (- x a) h))
;;     (cond ((= 0 k) k)
;;           ((even? k) (* 2.0 k))
;;           (else (* 4.0 k))))

;;   ;; y_k = f(a + kh)
;;   (define (next a)
;;     (+ a h))
;;   (* (/ h 3.0) (sum term a next (- b (* 2.0 h)))))
;;
;;
;;; Here is the fundmental idea that convert the INTEGRAL from a to b into a SUM from k = 0 to n
;;;;; SOLUTION:
(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (simpson-term k)
    (define y (f (+ a (* k h))))        ; y_k = f(a + kh)
    (* y (cond ((or (= k 0) (= k n)) 1) ; also notice when k = n, also be 1
               ((even? k) 2)
               (else 4))))
  (* (/ h 3.0) (sum simpson-term 0 inc n)))

(define (cube x) (* x x x))
(simpson-integral cube 0 1 100000)
