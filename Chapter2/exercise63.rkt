#lang sicp
;;; Sets as binary trees

;; Binary tree define.
(define entry car)
(define left-branch cadr)
(define right-branch caddr)
(define (make-tree entry left-branch right-branch)
  (list entry left-branch right-branch))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        (else (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set)) (make-tree (entry set)
                                      (adjoin-set x (left-branch set))
                                      (right-branch set)))
        ((> x (entry set)) (make-tree (entry set)
                                      (left-branch set)
                                      (adjoin-set x (right-branch set))))))

;; Exercise 2.63 - Compare two procedure.
;;
;; Not a scheme problem, skip.
(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

;; Exercise 2.64 - The procedure list->tree convert an ordered list to a balanced binary tree.
(define (list->tree l)
  (car (partial-tree l (length l))))

(define (partial-tree elts n)
  (if (= n 0) (cons '() elts)
      (let ((left-number (quotient (- n 1) 2))) ; NOTE: '/` in lisp will do rational operation, wouldn't must be an integer.
        (let ((left-result (partial-tree elts left-number)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-number (- n (+ left-number 1))))
            (let ((this-elem (car non-left-elts))
                  (right-elts (cdr non-left-elts)))
              (let ((right-result (partial-tree right-elts right-number)))
                (let ((right-tree (car right-result))
                      (rest-elts (cdr right-result)))
                  (cons (make-tree this-elem
                                   left-tree
                                   right-tree)
                        rest-elts)))))))))

;; (display (car (partial-tree '(10 11) 2)))
;; (display (list->tree '(1 2 3 4 9 10)))
;; Exercise 2.65 - Give O(n) implementation of union-set and intersection-set
(define (union-set set1 set2)
  (define (union-set-helper a b)
    (cond ((null? a) b)
          ((null? b) a)
          (else (cons (car a) (union-set-helper (cdr a) b)))))
  (list->tree (union-set-helper (tree->list set1)
                                (tree->list set2))))

;; (display (union-set (list->tree '(1 2 3 4 5)) (list->tree '(34 8 9))))
(display (tree->list (list->tree '(1 2 3 4 5 6))))
(union-set (list->tree '(1 2 3)) (list->tree '(10)))

;; Exercies 2.66 - Define `lookup' for binary tree.
(define (lookup given-key set-of-records)
  (let ((this-elem (entry set-of-records)))
    (cond ((null? set-of-records) false)
          ((= this-elem given-key) this-elem)
          ((< this-elem given-key)
           (lookup given-key (right-branch set-of-records)))
          ((> this-elem given-key)
           (lookup given-key (left-branch set-of-records))))))
