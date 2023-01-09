#lang sicp
;; Exercise 40
(define tolerance 0.0001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average a b) (/ (+ a b) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (deriv g)
  (lambda (x) (/ (- (g ( + x dx)) (g x)) dx)))
(define dx 0.00001)

(define (newton-transform g)
  (lambda (x) (- x (g x) ((deriv g) x))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cube x) (* x x x))
(define (square x) (* x x))
(define (cubic a b c)
  (lambda (x)                           ; Returns a function that takes x as an arugument.
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))


;; Exercise 41
(define (double f)
  (lambda (x)
    (f (f x))))

;; Exercise 42
(define (compose f g)
  (lambda (x)
    (f (g x))))

;; (define (square x) (* x x))
;; ((compose square inc) 6)

;; Exercise 43
;; TODO can't finish independently.
(define (repeated f n)                  ; Apply `f' n times
  (if (< n 1)
      (lambda (x) x)                    ; Why not `f' here, rather than `(lambda (x) x)' ?
      (compose f (repeated f (- 1 n)))))

;; Exercise 1.44
(define (smooth f)
  (lambda (x) (/ (+ (f x)
                    (f (+ x dx))
                    (f (- x dx)))
                 3)))

(define (n-fold f n)
  ((repeated smooth n) f))

;; Exercise 1.45
;; TODO: Can't understand it.
