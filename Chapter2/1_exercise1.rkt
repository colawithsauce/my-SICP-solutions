#lang sicp
;; Exercise 2.01 - Define a better version of `make-rat' that handles both
;; positive and negative arguments.
;;
;; The thought here is fundmental: Check the sign of numerator and the
;; denominator of the rational. When they are same, positive, else negative.

;; (define (make-rat n d)
;;   (define (same-sig? a b)
;;     (or (and (positive? a) (positive? b))
;;         (and (negative? a) (negative? b))))
;;   (let ((g (abs (gcd n d)))
;;         (abs-n (abs n))
;;         (abs-d (abs d)))
;;     (if (same-sig? n d)
;;         (cons (/ abs-n g) (/ abs-d g))
;;         (cons (- (/ abs-n g)) (/ abs-d g)))))

;; A cleverer solution, from http://community.schemewiki.org/?sicp-ex-2.1
(define (make-rat n d)
  (let ((g ((if (negative? d) - +) (gcd n d))))
    (cons (/ n g) (/ d g))))

;; Exercise 2.02
(define make-point cons)
(define x-point car)
(define y-point cdr)

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define (midpoint-segment segment)
  (let ((start-x (x-point (start-segment segment)))
        (start-y (y-point (start-segment segment)))
        (end-x (x-point (end-segment segment)))
        (end-y (y-point (end-segment segment))))
    (make-point (/ (+ start-x end-x) 2)
                (/ (+ start-y end-y) 2))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

;; Testing
(define seg (make-segment (make-point 2 3)
                          (make-point 10 15)))

(print-point (midpoint-segment seg))

;; Exercise 2.03 - Representation for rectangles in a plane.
;;
;; The notion behind my solution is also fundmental: To define a rectangle only
;; needs two points, that is the left-upper point and the right-lower point.
(define make-rectangle cons)
(define first-rec-point car)
(define last-rec-point cdr)

(define (rec-length rectangle)
  (abs (- (x-point (first-rec-point rectangle))
          (x-point (last-rec-point rectangle)))))
(define (rec-height rectangle)
  (abs (- (y-point (first-rec-point rectangle))
          (y-point (last-rec-point rectangle)))))

(define (perimeter rectangle)
  (let ((length (rec-length rectangle))
        (height (rec-height rectangle)))
    (+ (* 2 length) (* 2 height))))

(define (area rectangle)
  (let ((length (rec-length rectangle))
        (height (rec-height rectangle)))
    (* height length)))

;; Test
(define my-rectangle
  (make-rectangle
   (make-point -5 -5)
   (make-point 5 5)))

(perimeter my-rectangle)                ; Supposed to be 40
(area my-rectangle)                     ; Supposed to be 100

;; Exercise 2.04 - Trivial, skipped.


;; Exercise 2.05 - Using 2^a*3^b to define cons, car, cdr.
;;
;; NOTE: Failed to workout independently, resort to find solution at
;; http://community.schemewiki.org/?sicp-ex-2.5
(define (pow-cons a b)
  (* (exp 2 a) (exp 3 b)))
(define (iter-for-remainder x divisor)
  (define (iter try-exp)
    (if (= 0 (remainder x (exp divisor try-exp)))
        (iter (+ try-exp 1))
        (- try-exp 1)))
  (iter 1))

(define (pow-car x)
  (iter-for-remainder x 2))
(define (pow-cdr x)
  (iter-for-remainder x 3))


