#lang sicp
;; Huffman trees

;; Leaves
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

;; Tree
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right)) ; -> list
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; Decoding
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((eq? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;; Exercise 2.67 - Here define an encoding tree and a sample message, and give the result
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(display (decode sample-message sample-tree))
(newline)

(define unimplemented! (lambda () (error "UNIMPLEMENTED!")))
(define (assert! predicate) (if (not predicate) (error "ASSERT!" predicate)))

;; Exercise 2.68 - Complete the procedure
(define (encode message tree)
  (if (null? message) '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol sym tree)
  (cond ((not (element-of-set? sym (symbols tree)))
         (error "bad symbol: ENCODE-SYMBOL" sym))
        ((leaf? tree) nil)
        ((element-of-set? sym (symbols (left-branch tree)))
         (cons 0 (encode-symbol sym (left-branch tree))))
        (else (cons 1 (encode-symbol sym (right-branch tree))))))

(display (encode (decode sample-message sample-tree) sample-tree))
(assert! (equal? sample-message
                 (encode (decode sample-message sample-tree) sample-tree)))
(newline)

;; Exercise 2.69 - Generage a huffman tree.
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  ;; Assuming that the leaf-set is ascending ordered by weight.
  (if (= (length leaf-set) 1)
      (car leaf-set)
      (let ((new-elem (make-code-tree (car leaf-set) (cadr leaf-set))))
        (successive-merge (adjoin-set new-elem (cddr leaf-set))))))

;; Exercise 2.70 - Encoding Rock Lyrics
(define rock-pairs
  '((WAH 1) (BOOM 1) (A 2) (GET 2) (JOB 2)
            (SHA 3) (YIP 9) (NA 16)))
(define rock-message
  '(GET A JOB
        SHA NA NA NA NA NA NA NA NA
        GET A JOB
        SHA NA NA NA NA NA NA NA NA
        WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
        SHA BOOM))
(length (encode rock-message (generate-huffman-tree rock-pairs)))
;; 2.71 - 4 when n = 5, and 9 when n = 10.
;; 2.72 - O(1) in least frequent, and O(n-1) in most frequent.
