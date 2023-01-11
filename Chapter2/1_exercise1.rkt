#lang sicp
;; Exercise 2.01 - Define a better version of `make-rat' that handles both
;; positive and negative arguments.
;;
;; The thought here is fundmental: Check the sign of numerator and the
;; denominator of the rational. When they are same, positive, else negative.
(define (make-rat n d)
  (define (same-sig? a b)
    (or (and (positive? a) (positive? b))
        (and (negative? a) (negative? b))))
  (let ((g (abs (gcd n d)))
        (abs-n (abs n))
        (abs-d (abs d)))
    (if (same-sig? n d)
        (cons (/ abs-n g) (/ abs-d g))
        (cons (- (/ abs-n g)) (/ abs-d g)))))
