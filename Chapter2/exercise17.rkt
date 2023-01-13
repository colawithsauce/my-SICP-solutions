#lang sicp
;; Exercise 17 - Define a procedure last-pair that returns the list that
;; contains only the last element of a given (nonempty) list.
(define (last-pair i)
  (if (pair? (cdr i))
      (last-pair (cdr i))
      i))

(display "Exercise 17:")
(newline)
(display (last-pair (list 1 2 3 4)))
(newline)


;; Exercise 18 - Define a procedure reverse that takes a list as arguent and
;; returns a list of the same elements in reverse order
(define (reverse x)
  (define (helper x result)
    (cond ((null? x) result)
          (else
           (helper (cdr x) (cons (car x) result)))))
  (helper x '()))

(display "Exercise 18:")
(newline)
(display (reverse (list 1 2 3 4)))
(newline)


;; Exercise 2.19 - Rewrite procedure in Section 1.2.2
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(define (no-more? coin-values)
  (null? coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(display "Exercise 19:")
(newline)
(display (cc 100 us-coins))
(newline)

;; The order of denominations doesn't change the result.

;; Exercise 2.20 - Using the notion of arbitrary number of arguments, write a
;; procedure `same-parity' that takes one or more integers and returns a list of
;; all the arguments that have the same even-odd parity as the first argument.
;;
;; TODO: Using append like this is expensive, cuz it will recursively find the
;; rear of the list each time called. A practical way to amend this is to build
;; list in a reversed style and finally reverse it in the end of procedure.

;; (define (same-parity . x)
;;   (define (same-parity? a b)
;;     (or (and (odd? a) (odd? b))
;;         (and (even? a) (even? b))))
;;   (define (helper a x)
;;     (cond ((null? x) x)
;;           ((same-parity? a (car x))
;;            (append (list (car x)) (helper a (cdr x))))
;;           (else
;;            (helper a (cdr x)))))
;;   (helper (car x) x))

(define (same-parity first . rest)
  (define (same-parity? a b)
    (or (and (odd? a) (odd? b))
        (and (even? a) (even? b))))
  (define (helper first rest result)
    (cond ((null? rest) (reverse result))
          ((same-parity? first (car rest))
           (helper first (cdr rest) (cons (car rest) result)))
          (else
           (helper first (cdr rest) result))))
  (helper first rest (list first)))

(display "Exercise 20:")
(newline)
(display (same-parity 1 2 3 4 5 6 7))
(newline)
(display (same-parity 2 3 4 5 6 7))
(newline)
