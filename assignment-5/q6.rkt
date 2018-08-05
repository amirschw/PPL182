#lang racket
(require racket/sandbox)
(require racket/exn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 1: The lazy lists interface ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cons-lzl cons)

(define empty-lzl empty)

(define empty-lzl? empty?)

(define head car)

(define tail
  (lambda (lz-lst)
    ((cdr lz-lst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 2: Auxiliary functions for testing ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Signature: check-inf-loop(mission)
; Purpose: check if the result is infinite loop,
;          if so, return 'infinite
;          otherwise the actual result
; Type: [[Empty -> T1] -> Union(T1, Symbol)]
(define check-inf-loop
  (lambda (mission)
    (with-handlers ([exn:fail:resource?
                     (λ (e)
                       (if (equal? (exn->string e)
                                   "with-limit: out of time\n")
                           'infinite
                           'error))])
      (call-with-limits 1 #f mission))))

; A function that creates an infinite loop
(define (inf x) (inf x))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 3: The assignment ;
;;;;;;;;;;;;;;;;;;;;;;;;;;

; Signature: all-subs(long)
; Type: [List(T) -> LZL(List(T))]
; Purpose: compute all lists that can be obtained 
; from long by removing items from it.
; Pre-conditions: -
; Tests:
; (take (all-subs '(1 2 3)) 8) ->
; '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
(define all-subs
  (lambda (long)
    (if (empty? long)
        (cons-lzl (list) (lambda () empty-lzl))
        (let ((lzl-without-first (all-subs (cdr long))))
          (lzl-append lzl-without-first
                      (add-first (car long) lzl-without-first))))))

;; Signature: lz-lst-append(lz1, lz2)
;; Type: [LZL(T) * LZL(T) -> LZL(T)]
(define lzl-append
  (lambda (lz1 lz2)
    (if (empty-lzl? lz1)
        lz2
        (cons-lzl (head lz1)
                  (lambda () (lzl-append (tail lz1) lz2))))))

;; Signature: add-car(c, lzl)
;; Type: [T * LZL(List(T)) -> LZL(List(T))]
(define add-first
  (lambda (c lzl)
    (if (empty-lzl? lzl)
        lzl
        (cons-lzl (cons c (head lzl))
                  (lambda () (add-first c (tail lzl)))))))

;; Signature: take(lz-lst,n)
;; Type: [LZL * Number -> List]
;; If n > length(lz-lst) then the result is lz-lst as a List
(define take
  (lambda (lz-lst n)
    (if (or (= n 0) (empty-lzl? lz-lst))
      empty-lzl
      (cons (head lz-lst)
            (take (tail lz-lst) (- n 1))))))

(define take1
  (lambda (lz-lst pred?)
    (cond ((empty-lzl? lz-lst) '())
          ((pred? (head lz-lst)) (cons-lzl (head lz-lst)
                                           (take1 (tail lz-lst) pred?)))
          (else '()))))

;;;;;;;;;;;;;;;;;;;;;
; Part 4: The tests ;
;;;;;;;;;;;;;;;;;;;;;

;; Make sure to add take or another utility to test here
;; If the results are obained in a different order, change the test accordingly.
(check-inf-loop (lambda () (take (all-subs '(1 2 3)) 9)))

;; Write more tests - at least 5 tests.
(check-inf-loop (lambda () (take (all-subs '()) 2)))
(check-inf-loop (lambda () (take (all-subs '(1 2 0 7)) 20)))
(check-inf-loop (lambda () (take (all-subs '(a b c d e f g h i j k l m n o p q r s t u v w x y z)) 25)))
(check-inf-loop (lambda () (take1 (all-subs '(a 1 b 2)) identity)))
(check-inf-loop (lambda () (take1 (all-subs '((1 2) 3 (4))) identity)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 5: The tests expected results;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
> (check-inf-loop (lambda () (take (all-subs '(1 2 3)) 9)))
'(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
> (check-inf-loop (lambda () (take (all-subs '()) 2)))
'(())
> (check-inf-loop (lambda () (take (all-subs '(1 2 0 7)) 20)))
'(() (7) (0) (0 7) (2) (2 7) (2 0) (2 0 7) (1) (1 7) (1 0) (1 0 7) (1 2) (1 2 7) (1 2 0) (1 2 0 7))
> (check-inf-loop (lambda () (take '(a b c d e f g h i j k l m n o p q r s t u v w x y z) 25)))
'(() (z) (y) (y z) (x) (x z) (x y) (x y z) (w) (w z) (w y) (w y z) (w x) (w x z) (w x y) (w x y z) (v) (v z) (v y) (v y z) (v x) (v x z) (v x y) (v x y z) (v w))
> (check-inf-loop (lambda () (take1 (all-subs '(a 1 b 2)) identity)))
'(() (2) (b) (b 2) (1) (1 2) (1 b) (1 b 2) (a) (a 2) (a b) (a b 2) (a 1) (a 1 2) (a 1 b) (a 1 b 2))
> (check-inf-loop (lambda () (take1 (all-subs '((1 2) 3 (4))) identity)))
'(() ((4)) (3) (3 (4)) ((1 2)) ((1 2) (4)) ((1 2) 3) ((1 2) 3 (4)))
|#