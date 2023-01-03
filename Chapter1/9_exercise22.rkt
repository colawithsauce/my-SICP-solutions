#lang sicp
;; from 1.2.6

(define (square a) (* a a))
(define (smallest-divisor n)
  (find-divisor n 2))
;;; This is the old one,
;; (define (find-divisor n test-divisor)
;;   (cond ((> (square test-divisor) n) n)
;;         ((divides? test-divisor n) test-divisor)
;;         (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

;; Fermat test
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;; The stem of exercise 22, modified in the assitance of [SICP Answer](http://community.schemewiki.org/?sicp-ex-1.22)
(define (timed-prime-test n)
  (define (start-prime-test n start-time)
    (if (prime? n)
       (report-prime n (- (runtime) start-time))
       #f))                             ; return false when test failed

  (define (report-prime n elapsed-time)
    (newline)
    (display n)
    (display " *** ")
    (display elapsed-time))

  (start-prime-test n (runtime))
)


;; Search for n primes bigger than 'start', reference: http://community.schemewiki.org/?sicp-ex-1.22
(define (search-for-primes n counter)
  (if (even? n)
      (s-f-p (+ n 1) counter)
      (s-f-p n counter)))

(define (s-f-p n counter)
  (if (> counter 0)
      (if (timed-prime-test n)
          (s-f-p (+ n 2) (- counter 1))
          (s-f-p (+ n 2) counter)
      )))

;; Exercise 23
;;; NEW find divisor procedure
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (next devisor)
  (if (= devisor 2)
      3
      (+ devisor 2)))

;; SKIP exercise 24 - 28, because I don't know how to implement it.
