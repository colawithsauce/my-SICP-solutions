#lang sicp
;; Nested mapping
;; Supporting functions:
(define nil '())

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime? x)
  (define (test divisor)
    (cond ((> (* divisor divisor) x) true)
          ((= 0 (remainder x divisor)) false)
          (else (test (+ divisor 1)))))
  (test 2))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-sum-pair pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;; Exercise 2.40 - Define a procedure `unique-pairs' given an integer n,
;; generates the sequence of pairs (i, j) with 1 <= j < i <= n

;; Failed to solve it independently, resort to answer page.
;; http://community.schemewiki.org/?sicp-ex-2.40
(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list j i))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

;; (display (unique-pairs 5))

(define (prime-sum-pairs n)
  (map make-sum-pair (filter prime-sum? (unique-pairs n))))

;; (newline)
;; (display (prime-sum-pairs 10))


;; Exercise 2.41 - Write a procedure to find all ordered triples of *distinct*
;; positive integers i, j and k less than or equal to a given integer n that sum
;; to a given integer s

(define (triples s n)
  ;; All tirpels that element of which distinct each other and <= n, and that sum to s
  (filter (lambda (t) (= (+ (car t) (cadr t) (caddr t)) s))
          (flatmap
           (lambda (k)
             (map (lambda (p) (cons k p))
                  (unique-pairs (- k 1))))
           (enumerate-interval 1 n))))

(newline)
(display (triples 40 100))
