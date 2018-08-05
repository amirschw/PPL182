#lang racket

; Q3 Scheme Programing

; Q3.1 count-syllables

(define vowels '(a e i o u))

; Signature: count-syllables(l)
; Purpose: To count the number of syllables in a word represented by a list of letters
; Type: [List(string) -> Number]
; Example: (count-syllables '(s o a r i n g)) should return 2
(define count-syllables
  (lambda (l)
    (letrec ((loop (lambda (l prev)
                     (if (empty? l)
                         0
                         (if (not prev)
                             (if (member (car l) vowels) 
                                 (+ 1 (loop (cdr l) #t)) ; current letter is a vowel but previous letter is not
                                 (loop (cdr l) #f))      ; both previous letter and current letter are not vowels
                             (if (member (car l) vowels) 
                                 (loop (cdr l) #t)       ; both previous letter and current letter are vowels
                                 (loop (cdr l) #f))))))) ; previous letter is a vowel but current letter is not
    (loop l #f))))

; Tests:
(define test-count-syllables
  (lambda ()
    (if (and (equal? (count-syllables '(s o a r i n g)) 2)
             (equal? (count-syllables '(b e e p)) 1)
             (equal? (count-syllables '(b a e i o u p)) 1)
             (equal? (count-syllables '()) 0)
             (equal? (count-syllables '(a a b i i c o o d u u)) 4)
             (equal? (count-syllables '(x a a a a a o o o o y i i i e e e i z w o)) 3)
             (equal? (count-syllables '(b c d f g h j)) 0))
        #t
        (error "test failed"))))

; Q3.2 sorted?

; Signature: sorted?(l op)
; Purpose: To check if the list l is sorted according to the operator argument
; Type: [List(Number) * Numerical Predicate -> Boolean]
; Example: (sorted? '(1 3 5) <) should return #t
(define sorted?
  (lambda (l op)
    (if (or (empty? l) (empty? (cdr l))) ; base case - empty list and list of 1 element are trivially sorted
        #t
        (and (op (car l) (cadr l)) (sorted? (cdr l) op)))))

; Q3.3 merge

; Signature: merge(l1, l2)
; Purpose: To merge two lists sorted in increasing order into one sorted list
; Type: [List(Number) * List(Number) -> List(Number)]
; Example: (merge '(1 3 8) '(2 5 6)) should return '(1 2 3 5 6 8)
; Pre-conditions: (sorted? l1 <), (sorted? l2 <)
(define merge
  (lambda (l1 l2)
    (if (not (and (sorted? l1 <) (sorted? l2 <)))
        (error "Both lists must be sorted in increasing order.")
        (if (empty? l1)
            l2
            (if (empty? l2)
                l1
                (if (< (car l1) (car l2))
                    (cons (car l1) (merge (cdr l1) l2))
                    (if (> (car l1) (car l2))
                         (cons (car l2) (merge l1 (cdr l2)))
                         (cons (car l1) (merge (cdr l1) (cdr l2))))))))))

; Tests:
(define test-merge
  (lambda ()
    (if (and (equal? (merge '(1 3 8) '(2 5 6)) '(1 2 3 5 6 8))
             (equal? (merge '() '(1 2)) '(1 2))
             (equal? (merge '() '(-2 -1)) '(-2 -1))
             (equal? (merge '(1 3 8 9 10 12 15 19 127) '(3 5 6 9 18 100)) '(1 3 5 6 8 9 10 12 15 18 19 100 127)))
        #t
        (error "test failed"))))

; Q3.4 remove-adjacent-duplicates
; Signature: remove-adjacent-duplicates(l)
; Purpose: To remove adjecent duplicate elements from a list
; Type: [List(T) -> List(T)]
; Example: (remove-adjacent-duplicates '(y a b b a d a b b a d o o)) should return '(y a b a d a b a d o)
(define remove-adjacent-duplicates
  (lambda (l)
    (if (empty? l)
        '()
        (letrec ((loop (lambda (curr rest prev)
                         (if (empty? rest)
                             curr ; base case - end of list
                             (if (equal? prev (car rest))
                                 (loop curr (cdr rest) (car rest))                                 ; previous and current element are duplicates
                                 (loop (append curr (list (car rest))) (cdr rest) (car rest))))))) ; current element is different from previous one
          (loop (list (car l)) (cdr l) (car l))))))

; Tests:
(define test-remove-adjacent-duplicates
  (lambda ()
    (if (and (equal? (remove-adjacent-duplicates '(y a b b a d a b b a d o o)) '(y a b a d a b a d o))
             (equal? (remove-adjacent-duplicates '(spl182 spl182 assignment assignment assignment assignment 1 1 1 1 1 1 1 1)) '(spl182 assignment 1))
             (equal? (remove-adjacent-duplicates '(+ + + + + #t #t #t #t #f #t #t #t #t + + + + +)) '(+ #t #f #t +))
             (equal? (remove-adjacent-duplicates '(+ #t #f #t +)) '(+ #t #f #t +)))
        #t
        (error "test failed"))))
