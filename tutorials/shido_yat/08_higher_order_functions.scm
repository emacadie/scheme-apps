;; 08 Higher Order Functions

;; 1. intro: Sort is in guile and chez, but not kawa
;; also not listed in r6rs docs

;; 2. Mapping: map returns the converted list, for-each is used for side effects
;; adding each item of '(1 2 3) and '(4 5 6)
(map + '(1 2 3) '(4 5 6)) ;; '(5 7 9)
;; squaring each item
(map (lambda (x) (* x x)) '(1 2 3)) ;; '(1 4 9)

(define sum 0)
(for-each (lambda (x) (set! sum (+ sum x))) '(1 2 3 4))
;; it looks like nothing happens, so look at sum
sum ;; 10

;; exercise 1
;; A function that makes it twice that each item of a list of numbers.
(map (lambda (x) (* 2 x)) '(2 3 4)) ;; '(4 6 8)
;; shido's answer:
(define (double ls)
  (map (lambda (x) (* x 2)) ls))
;; A function that subtracts items of two lists.
(map - '(21 23 25) '(14 15 16)) ;; '(7 8 9)
;; shido's answer
(define (sub ls1 ls2)
  (map - ls1 ls2))

;; 3. Filtering
;; there is a "filter" out of the box in chez and guile
;; for Kawa, do this: (require 'list-lib)
(filter positive? '(1 2 -3 -4 5)) ;; '(1 2 5)

;; exercise 2
;; filtering out even numbers in a list.
(filter even? '(1 2 -3 -4 5)) ;; (2 -4)
;; filtering out numbers (x) that are not 10 ≤ x ≤ 100. 
(filter (lambda (x) (and (<= 10 x) (<= x 100))) '(1 2 3 11 22 33 44 55 66 77 88 99 100 101 111 122)))
;; (11 22 33 44 55 66 77 88 99 100)
;; Shido's answers use keep-matching-items, which does not exist in Kawa

;; 4 Folding aka reducing - neither of those names really says what it does
(reduce + 0 '(1 2 3 4)) ;; 10
(reduce + 0 '(1 2)) ;; 3
(reduce + 0 '(1)) ;; 1
(reduce + 0 '()) ;; 0
(reduce + 0 '(foo)) ;; foo
(reduce list '() '(1 2 3 4)) ;; does not work

;; exercise 3
;; Write a function that squares each item of a list, then sums them and then makes square root of it. 
;; that sounds like map to reduce
;; cats and dogs living together
(sqrt (reduce + 0 (map (lambda (x) (* x x)) '(1 2 3))))
(define (square-sum-square-root ls)
    (sqrt (reduce + 0 (map (lambda (x) (* x x)) ls))))
(square-sum-square-root '(1 2 3)) ;; 3.7416573867739413

;; shido's answer:
(define (sqrt-sum-sq ls)
  (sqrt (reduce + 0 (map (lambda (x) (* x x)) ls))))
(sqrt-sum-sq '(1 2 3)) ;; 3.7416573867739413

;; 5 sorting
;; for Kawa: do this: (import (srfi 95)) or (require 'srfi-95)
(sort '(3 4 1 4 -1) <) ;; (-1 1 3 4 4)

;; Exercise 4
;; Write followings:
;; Sort by magnitude of sin(x) in ascending order.
(sort '(1 2 3 4) (lambda (x y) (< (sin x) (sin y)))) ;; (4 3 1 2)
(sin 4) ;; -0.7568024953079282
(sin 3) ;; 0.1411200080598672
(sin 2) ;; 0.9092974268256817
(sin 1) ;; 0.8414709848078965
;; shido's answer:
(define (sort-sin ls)
  (sort ls (lambda (x y) (< (sin x) (sin y)))))
(sort-sin '(1 2 3 4)) ;; (4 3 1 2)
;; Sort by length of lists in descending order. 
;; this won't work with '(1) '(1 2), etc
(sort '( (list 1) (list 1 2)  (list 1 2 3 4) (list 1 2 3)) (lambda (x y) (> (length x) (length y))))
;; ((list 1 2 3 4) (list 1 2 3) (list 1 2) (list 1))
;; shido's answer
(define (sort-length ls)
  (sort ls (lambda (x y) (> (length x) (length y)))))
(sort-length '((list 1) (list 1 2 3 4) (list 1 2) (list 1 2 3)))
;; ((list 1 2 3 4) (list 1 2 3) (list 1 2) (list 1))

;; 6 Apply - applies a function to a list
(apply max '(1 2 3)) ;; 3
(apply max '(1 3 2)) ;; 3
(+ 1 2 3 4 5) ;; 15
(apply + 1 2 '(3 4 5)) ;; 15

(- 100 5 12 17) ;; 66
(apply - 100 '(5 12 17)) ;; 66
;; Exercise 5 Use apply to write a same function as that of Exercise 3. 
;; Write a function that squares each item of a list, then sums them and then makes square root of it. 
(define (square-sum-square-root ls)
    (sqrt (reduce + 0 (map (lambda (x) (* x x)) ls))))
'(1 2 3)
(apply square-sum-square-root 1 2 3)
;; that doesn't work, you get this:
;; call to 'square-sum-square-root' has too many arguments (3; must be 1)
;; so it expands the list
;; shido's answer:
(define (sqrt-sum-sq-a ls)
  (sqrt (apply + (map (lambda (x) (* x x)) ls))))
;; so he/she (?) doesn't start with apply, he uses it in the middle


