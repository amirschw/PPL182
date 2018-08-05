#lang racket
(require "q4.rkt")

; run all test with the script (test)

(define test-shift-left
  (lambda ()
    (if (and (equal? (shift-left '()) '())
             (equal? (shift-left '(5)) '(5))
             (equal? (shift-left '(1 2)) '(2 1))
             (equal? (shift-left '(1 2 3 4)) '(2 3 4 1))
             (equal? (shift-left '(1 (2 3) 4)) '((2 3) 4 1)))
        #t
        (error "test-shift-left failed"))))

(define test-shift-k-left
  (lambda ()
    (if (and (equal? (shift-k-left '() 5) '())
             (equal? (shift-k-left '(1) 100) '(1))
             (equal? (shift-k-left '(1 2 3) 0) '(1 2 3))
             (equal? (shift-k-left '(1 2 3) 1) '(2 3 1))
             (equal? (shift-k-left '(1 2 3) 2) '(3 1 2))
             (equal? (shift-k-left '(1 2 3) 3) '(1 2 3)))
        #t
        (error "test-shift-k-left failed"))))

(define test-shift-right
  (lambda ()
    (if (and (equal? (shift-right '()) '())
             (equal? (shift-right '(1)) '(1))
             (equal? (shift-right '(1 2 3)) '(3 1 2))
             (equal? (shift-right '(3 1 2)) '(2 3 1)))
        #t
        (error "test-shift-right failed"))))

(define test-combine
  (lambda ()
    (if (and (equal? (combine '() '()) '())
             (equal? (combine '(1 2 3) '()) '(1 2 3))
             (equal? (combine '() '(4 5 6)) '(4 5 6))
             (equal? (combine '(1 3) '(2 4)) '(1 2 3 4))
             (equal? (combine '(1 3) '(2 4 5 6)) '(1 2 3 4 5 6))
             (equal? (combine '(1 2 3 4) '(5 6)) '(1 5 2 6 3 4)))
        #t
        (error "test-combine failed"))))

(define test-sum-tree
  (lambda ()
    (if (and (equal? (sum-tree '()) 0)
             (equal? (sum-tree '(5)) 5)
             (equal? (sum-tree '(5 (1 (2) (3)))) 11)
             (equal? (sum-tree '(5 (1 (2) (3) (6)) (7))) 24)
             (equal? (sum-tree '(5 (1 (2) (3 (12) (12)) (6)) (7))) 48))
        #t
        (error "test-sum-tree failed"))))

(define test-inverse-tree
  (lambda ()
    (if (and (equal? (inverse-tree '()) '())
             (equal? (inverse-tree '(5)) '(-5))
             (equal? (inverse-tree '(0)) '(0))
             (equal? (inverse-tree '(#f)) '(#t))
             (equal? (inverse-tree '(#t)) '(#f))
             (equal? (inverse-tree '(-5 (1 (-2) (3) (#f)) (#t))) '(5 (-1 (2) (-3) (#t)) (#f))))
        #t
        (error "test-inverse-tree failed"))))

(define test
  (lambda ()
    (and (test-shift-left)
         (test-shift-k-left)
         (test-shift-right)
         (test-combine)
         (test-sum-tree)
         (test-inverse-tree))))

(test)