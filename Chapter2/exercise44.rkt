#lang sicp
(#%require sicp-pict)
;; Picture Language

;; Exercise 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(paint (up-split einstein 4))

;; Hight order operations
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(paint (flipped-pairs einstein))

;; Exercise 2.45 - Define right-split and up-split with a general `split'
(define (split proc1 proc2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split proc1 proc2) painter (- n 1))))
          (proc1 painter (proc2 smaller smaller))))))

(define right-split (split beside below))

;; Exercise 2.46 - Vector data abstraction
(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect a b)
  (make-vect (+ (xcor-vect a) (xcor-vect b))
             (+ (ycor-vect a) (ycor-vect b))))
(define (sub-vect a b)
  (make-vect (- (xcor-vect a) (xcor-vect b))
             (- (xcor-vect a) (xcor-vect b))))
(define (scale-vect a s)
  (make-vect (* s (xcor-vect a))
             (* s (ycor-vect a))))

;; Exercise 2.47 - Implement frame
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define frame-origin car)
(define frame-edge1 cadr)
(define frame-edge2 caddr)

;; Skip 2.48 - 2.49
(define outline
  (segments->painter
   (list
    (segment (vect 0.0 0.0) (vect 0.0 1.0))
    (segment (vect 0.0 0.0) (vect 1.0 0.0))
    (segment (vect 0.0 1.0) (vect 1.0 1.0))
    (segment (vect 1.0 0.0) (vect 1.0 1.0)))))

(define x-painter
  (segments->painter
   (list
    (segment (vect 0.0 0.0) (vect 1.0 1.0))
    (segment (vect 0.0 1.0) (vect 1.0 0.0)))))


;; Exercies 2.50 - Define `flip-horiz'
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

;; Exercise 2.51 - Define the below operation.
(define (my-below bottom top)
  (let ((split-point (make-vect 0.0 0.5)))
    (lambda (frame)
      ((transform-painter bottom (make-vect 0.0 0.0)
                          (make-vect 1.0 0.0)
                          split-point)
       frame)
      ((transform-painter top split-point
                          (make-vect 1.0 0.5)
                          (make-vect 0.0 1.0))
       frame))))

;; It's correct, however can't run, unless I implement my version of `drawline'.

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((left (up-split painter n))
            (right (below (below (right-split painter (- n 1))
                                 (right-split painter (- n 1)))
                          (corner-split painter (- n 1)))))
        (beside left right))))

(paint (corner-split einstein 3))
;; Interesting!
