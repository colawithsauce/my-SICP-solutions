#lang sicp

(define (display-interval i)
  (newline)
  (display "[")
  (display (lower-bound i))
  (display ",")
  (display (upper-bound i))
  (display "]"))


(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

;; (define (mul-interval x y)
;;   (let ((p1 (* (lower-bound x) (lower-bound y)))
;;         (p2 (* (lower-bound x) (upper-bound y)))
;;         (p3 (* (upper-bound x) (lower-bound y)))
;;         (p4 (* (upper-bound x) (upper-bound y))))
;;     (make-interval (min p1 p2 p3 p4)
;;                    (max p1 p2 p3 p4))))

;; (define (div-interval x y)
;;   (mul-interval
;;    x
;;    (make-interval (/ 1.0 (upper-bound y))
;;                   (/ 1.0 (lower-bound y)))))

;; Exercise 2.07 - Define the constructor and selector of interval.

(define (make-interval a b) (cons a b))
(define (upper-bound z) (max (car z) (cdr z)))
(define (lower-bound z) (min (car z) (cdr z)))


;; Exercise 2.08 - Sub-interval
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; Exercise 2.09 - Prove that only when adding and substracting two interval
;; that the width of the result is the function of the width of arguments, and
;; that when multiplying and dividing is not the case.
;;
;; -> It's a Proving problem, skip.


;; Exercise 2.10 - Handle divide zero problem.
;;
;; EDIT: Shouldn't simply check if the bounds equals zero, because when it spawn
;; 0, then the upper-bound should be x/0+ which is positive infinity, and it's
;; lower-bound should be accordingly negative infinity.

(define (div-interval x y)
  (if (<= 0 (upper-bound y) (lower-bound y))
      (error "Division Error (Divisor Spans 0).")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

;; Exercise 2.11 - Rewrite `mul-interval' to make it able to handle negative
;; inputs.

;; Can't figure out how to amend it, because the original one can already handle
;; negative values.

;; The idea here is simple
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define x (make-interval 1 10))
(define y (make-interval -10 1))
(display-interval (mul-interval x y))


;; Exercise 2.12 - Write a procedure make-center-percent, that takes a center
;; and a percentage tolerance and procedures the desired interval.

(define (make-center-percent center tolerance)
  ; tolerance should be given as the number before percentage mark.
  (make-interval (- center (abs (* center (/ tolerance))))
                 (+ center (abs (* center (/ tolerance))))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))


;; NOTE:
;; 1. Exercise 2.13 - It's totally a math question. Skip
;; 2. Exercise 2.14, 2.15, 2.16, I don't know how to solve them. So I read @jz
;; 's answer at sicp solution page. Alas. Thanks @jz, you are excellent!
