#lang racket
(provide (all-defined-out))

; Q4.1
; Signature: shift-left(ls)
; Purpose: To shift all the element of 'ls' to the left by one place.
; Type: [List(T) -> List(T)]
; Example: (shift-left '(1 2 3)) should produce '(2 3 1)
(define shift-left
  (lambda (ls)
    (if (<= (length ls) 1)
      ls
      (append (cdr ls) (cons (car ls) '()))))) ; append the first element (car) to the rest of the list (cdr)

; Q4.2
; Signature: shift-k-left(ls)
; Purpose: To shift all the element of 'ls' to the left k times.
; Type: [List(T) * Number -> List(T)]
; Pre-conditions: k non-negative integer
; Example: (shift-k-left '(1 2 3) 3) should produce '(1 2 3)
(define shift-k-left
  (lambda (ls k)
    (if (<= (length ls) 1)
        ls
        (let ((m (modulo k (length ls)))) ; avoid unnecessary shifts using modulo
          (if (= m 0)
              ls
              (shift-k-left (shift-left ls) (- m 1))))))) ; shift once using shift-left and apply recursively with k = m-1

; Q4.3
; Signature: shift-right(ls)
; Purpose: To shift all the element of 'ls' to the right by one place.
; Type: [List(T) -> List(T)]
; Example: (shift-right '(1 2 3)) should produce '(3 1 2)
(define shift-right
  (lambda (ls)
    (if (<= (length ls) 1)
      ls
      (append (cons (last ls) '()) (take ls (- (length ls) 1)))))) ; move the last element to the beginning of the list

; Q4.4
; Signature: combine(ls1, ls2)
; Purpose: To combine the elements of 'ls1' and 'ls2'
;          in an alternating manner starting from ls1.
; Type: [List(T1) * List(T2) -> List(T1 | T2)]
; Example: (combine '(1 3) '(2 4)) should produce '(1 2 3 4)
(define combine
  (lambda (ls1 ls2)
    (if (empty? ls1)
        ls2
        (if (empty? ls2)
            ls1
            (cons (car ls1) (combine ls2 (cdr ls1))))))) ; add the first element of ls1 and call combine with ls2 as first argument

; Q4.5
; Signature: sum-tree(tree)
; Purpose: To compute the sum of numbers present in all tree nodes.
; Type: [Tree(Number) -> Number]
; Example: (sum-tree '(5 (1 (2) (3)))) should produce 11
(define sum-tree
  (lambda (tree)
    (if (empty? tree)
        0
        (foldl + (car tree) (map sum-tree (cdr tree)))))) ; apply sum-tree to all of the sub-trees and reduce

; Q4.6
; Signature: inverse-tree(tree)
; Purpose: To return a tree whose nodes are either
;          the inverse of the original node (where the original node is a number), or
;          the logical not of the original node (where the original node is a boolean)
; Type: [Tree(Number | Boolean) -> Tree(Number | Boolean)]
; Example: (inverse-tree '(-5 (1 (-2) (3) (#f)) (#t))) should produce '(5 (-1 (2) (-3) (#t)) (#f))
(define inverse-tree
  (lambda (tree)
    (if (empty? tree)
        '()
        (let ((x (car tree)))
          (if (boolean? x)
              (cons (not x) (map inverse-tree (cdr tree)))
              (cons (* x -1) (map inverse-tree (cdr tree)))))))) ; inverse first element, apply inverse-tree to all of the sub-trees and reduce
