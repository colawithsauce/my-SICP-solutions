#lang sicp
;; Exercise 17 - Define a procedure last-pair that returns the list that
;; contains only the last element of a given (nonempty) list.
(define (last-pair i)
  (if (pair? (cdr i))
      (last-pair (cdr i))
      i))

(display "Exercise 17")
(newline)
(display (last-pair (list 1 2 3 4)))
(newline)


;; Exercise 18 - Define a procedure reverse that takes a list as arguent and
;; returns a list of the same elements in reverse order
(define (reverse x)
  (if (null? (cdr x))
      x
      (append (reverse (cdr x)) (list (car x)))))

(display "Exercise 18")
(newline)
(display (reverse (list 1 2 3 4)))
(newline)
