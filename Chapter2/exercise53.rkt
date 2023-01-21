#lang sicp
;;; Symbolic Data
;; Quotation

;; Exercise 2.53 - What would the interpreter print in response to evaluating
;; each of the following expression?
;;
;; Skip, because they are kinda straightforward.

;; Exercise 2.54 - Define a procedure `equal?' that could figure out if two list are identical.
(define (equal? la lb)
  (cond ((null? la) (null? lb))
        ((null? lb) (null? la))
        ((and (pair? (car la))          ; Should recursively check if the list are identical.
              (pair? (car lb)))
         (and (equal? (car la) (car lb))
              (equal? (cdr la) (cdr lb))))
        (else
         (and (eq? (car la) (car lb))
              (equal? (cdr la) (cdr lb))))))

(equal? (list 'a 'b 'c 'd)
        (list 'a 'b 'c 'd))

(equal? '('a 'b 'c 'd)
        '('a 'b 'c 'd))

(equal? '(this (is a) list)
        '(this (is a) list))

;; Exercise 2.55 - Why the interpreter yield this?
;;
;; Because ''a would be interpreted as (quote (quote a)), the second `quote' was quoted.
