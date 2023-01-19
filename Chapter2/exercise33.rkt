#lang sicp
;; Sequence Operations

;; Accumulates the result of the first and the already-accumulated rest.
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; Exercise 2.33 - It's hard, bro. XP
(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil
              sequence))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (my-length sequence)
  (accumulate (lambda (_x y) (+ 1 y)) 0 sequence))

(display (my-map (lambda (x) (* x 2)) '(1 2 3)))
(newline)
(display (my-append '(1 2 3) '(4 5 6)))
(newline)
(display (my-length '(1 2 3)))
(newline)

;; Exercise 2.34 - Horner eval
;; Didn't finish it independently, thanks @jz on http://community.schemewiki.org/?sicp-ex-2.34
(define (horner-eval x coefficient-sequence)
  ;; The `higher-terms` stands for "already accumulated rest", which is the same type of `initial`.
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff
                   (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 2 '(1 3 0 5 0 1))

;; Exercise 2.35 - Redefine count-leaves from section 2.2.2 as an accumulation
(define (count-leaves t)
  (accumulate
   + 0
   (map (lambda (x)
          (cond ((null? x) 0)
                ((pair? x) (count-leaves x))
                (else
                 1)))
        t)))

(display "Exercise 2.35")
(newline)
(display (count-leaves (quote (1 () () (() ()) () () 2))))
(newline)

;; Exercise 2.36 - Procedure accumulate-n
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(display (accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12))))
(newline)


;; Exercies 2.37 - Matrix manipulating
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v))
         m)))

;; Testing
(define matrix (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12)))
(display (matrix-*-matrix matrix (list (list 1 2) (list 1 2) (list 1 2) (list 1 2))))
(newline)


;; Exercise 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
(define fold-right accumulate)
;; Might be (op a b) always yield same result as (op b a) ? - That is to say: commutative

;; Exercise 2.39
(define (reverse-right sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse-left sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(newline)
(display "Exercies 2.39")
(newline)
(display (reverse-right '(1 2 3 4 5 6)))
(newline)
(display (reverse-left '(1 2 3 4 5 6)))

;; Explain it: fold right is naturally an recursive procedure, and fold left is naturally an iterative procedure.
