#lang sicp
(define nil '())

;; Exercise 2.21 - Complete two square-list procedure.
(define (square x) (* x x))
(define (square-list-a items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list-a (cdr items)))))
(define (square-list-b items)
  (map square items))

(display "Exercise 21:")
(define ex20-items (list 1 2 3 4 5))
(newline)
(display (square-list-a ex20-items))
(newline)
(display (square-list-b ex20-items))
(newline)


;; Exercise 2.22 - Why the procedure produce reverse answer? And why the amend doesn't work?
;;
;; Because the first procedure push things at the head of the list, which makes
;; it yield the reverse of supposed result. And the second one produced wrong
;; answer, because the structure it made is `((a . nil) . b)', which is not the
;; conform the structure that how list is defined in Scheme.


;; Exercise 2.23 - Implement for-each
;; :P
;; (define (my-for-each f x)
;;   (map f x)
;;   #t)

(define (my-for-each f x)
  (cond ((null? x) #t)
        (else (f (car x))
              (my-for-each f (cdr x)))))
(my-for-each (lambda (x) (newline) (display x)) (list 57 321 88))

;; Exercise 2.24 - Not scheme problem, skip.
;; Exercise 2.25 - Get 7 in follow lists.
(define la '(1 3 (5 7) 9))
(define lb '((7)))
(define lc '(1 (2 (3 (4 (5 (6 7)))))))
(car (cdr (car (cdr (cdr la)))))
(car (car lb))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr lc))))))))))))

;; Exercise 2.26 - Not programming problem, skip.
;; Exercise 2.27 - Deep reverse a list.

(define (reverse x)
  (define (helper x result)
    (if (null? x) result
        (helper (cdr x) (cons (car x) result))))
  (helper x nil))

(define (deep-reverse x)
  (define (helper x result)
    (cond ((null? x) result)
          ((pair? (car x))
           (helper (cdr x) (cons (helper (car x) nil) result)))
          (else
          (helper (cdr x) (cons (car x) result)))))
  (helper x nil))

(display "Exercise 27:")
(define ex27-items '((1 2 3) (4 5)))
(newline)
(display (reverse ex27-items))
(newline)
(display (deep-reverse ex27-items))
(newline)

;; Exercise 2.28 - Write a procedure `fringe' that takes as argument a tree and
;; returns a list whose elements are all the leaves of the tree arranged in
;; left-to-right order.
;;
;; Failed to work it out independently, thus resort to seek help from
;; http://community.schemewiki.org/?sicp-ex-2.28. Thanks @rnsmit!
;;
;; Fringe of Tree x, is equal to fringe of it's right as the result appends to
;; fringe of it's left.
(define (fringe x)
  (define (helper x result)
    (cond ((null? x) result)
          ((pair? x)
           (helper (car x) (helper (cdr x) result)))
          (else (cons x result))))
  (helper x nil))

(display (fringe (list (list 1 2) (list 3 4 (list 5 (list 6))))))

