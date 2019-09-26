#lang simply-scheme

; Chapter 23 Vectors

(require (prefix-in more: "more-simply.rkt"))
; (require (prefix-in srfi-13: srfi/13))

;; Chapter 23 Vectors
(butfirst '(this is chapter 23: vectors))


; (define *lap-vector* (make-vector 100))

(define *lap-vector* (make-vector 100 0))

(define (initialize-lap-vector index)
  (if (< index 0)
      'done
      (begin (vector-set! *lap-vector* index 0)
	     (initialize-lap-vector (- index 1)))))

; (initialize-lap-vector 99)

;; an easier way to initialize a vector w/zeroes
(define *lv-02* (make-vector 100 0))

(define (lap car-number)
  ;; set the veector element - side effect
  (vector-set! *lap-vector*
	       car-number
	       (+ (vector-ref *lap-vector* car-number) 1))
  ;; return that element - return value
  (vector-ref *lap-vector* car-number))

#|
Vectors versus Lists
Important section
"List programming is characterized by two operations: 
dividing a list into its first element and all the rest, 
and sticking one new element onto the front of a list. 
Vector programming is characterized by selecting elements in any order, 
from a collection whose size is set permanently when the vector is created. "
I do not think vectors in Java are created with a set size.
Granted, Java devs use java.util.List more.
Java lists are dynamic like Lisp/Scheme lists, but you do not add to the front.
I looked at chapter 17, and the authors do not generally add to the front either,
unless they use cons more than append.
But in Java, you do not have the first/rest paradigm like with Lisp/Scheme lists.
Scheme vectors are like C/C++/Java arrays.
Adding to the end of a list feels more natural/intuitive. 
Again, I like the way Clojure does it with vectors.
Although, Scheme does have "append" for lists, so you can add to the back.
I think since this book was written, the world has moved away from fixed-length data structures.
From the text:
"The best way to understand these differences in style is to know the operations that are most efficient for each kind of aggregate. 
In each case, there are certain operations that can be done in one small unit of time, 
regardless of the number of elements in the aggregate, while other operations take more time for more elements. 
The constant time operations for lists are cons, car, cdr, and null?; the ones for vectors are vector-ref, vector-set!, and vector-length."
"In general, vectors are good at selecting elements in arbitrary order from a fixed-size collection; 
lists are good only at selecting elements strictly from left to right, but they can vary in size. "
empty? and butfirst are constant-time, so
(empty? (butfirst sent))
is better than
(= (count sent) 1)
I might go back and change things
|#

; Do not solve any of the following exercises by converting a vector to a list, 
; using list procedures, and then converting the result back to a vector.

; 23.1  Write a procedure sum-vector that takes a vector full of numbers as its argument 
; and returns the sum of all the numbers:

;> (sum-vector '#(6 7 8))
;21

(define (sum-vector-r the-vec total counter)
  ; (more:display-all "sum-vector-r with the-vec: " the-vec ", total: " total ", counter: " counter)
  (cond [(equal? counter (vector-length the-vec)) total]
        [else (sum-vector-r the-vec 
                            (+ total (vector-ref the-vec counter)) 
                            (+ 1 counter))]))

(define (sum-vector the-vec)
  (sum-vector-r the-vec 0 0))

;;  23.2  Some versions of Scheme provide a procedure vector-fill! 
; that takes a vector and anything as its two arguments. 
; It replaces every element of the vector with the second argument, like this:
; > (define vec (vector 'one 'two 'three 'four))
; > vec
; #(one two three four)
; > (vector-fill! vec 'yeah)
; > vec
; #(yeah yeah yeah yeah)

; Write vector-fill!. (It doesn't matter what value it returns.) 

(define (vector-fill-r! new-vec filler counter)
  (cond [(equal? counter (vector-length new-vec)) new-vec]
        [else (begin
                (vector-set! new-vec counter filler)
                (vector-fill-r! new-vec filler (+ 1 counter)))]))

(define (my-vector-fill! the-vec filler)
  (vector-fill-r! (make-vector (vector-length the-vec)) filler 0))

;;  23.3  Write a function vector-append that works just like regular append, but for vectors:
; > (vector-append '#(not a) '#(second time))
; #(not a second time)

(define (vec-append-helper output vec-a vec-b counter)
  ; (more:display-all "In vec-append-helper with output: " output ", vec-a: " vec-a ", vec-b: " vec-b ", counter: " counter)
  (cond [(equal? counter (vector-length output)) output]
        [(< counter (vector-length vec-a)) 
         (begin
           (vector-set! output counter (vector-ref vec-a counter))
           (vec-append-helper output vec-a vec-b (+ 1 counter)))]
        [else 
         (begin
           (vector-set! output 
                        counter 
                        (vector-ref vec-b (- counter (vector-length vec-a))))
           (vec-append-helper output vec-a vec-b (+ 1 counter)))]))

(define (my-vector-append vec-a vec-b)
  (vec-append-helper (make-vector (+ (vector-length vec-a)
                                     (vector-length vec-b)))
                     vec-a
                     vec-b
                     0))

;; 23.4  Write vector->list. 
;; I assume we do not have to do the one with indexes
(define (my-v->l-helper the-vec output counter)
  (cond [(equal? counter (vector-length the-vec)) output]
        [else (my-v->l-helper the-vec 
                              (append output (list (vector-ref the-vec counter))) 
                              (+ 1 counter))]))

(define (my-vector->list the-vec)
  (my-v->l-helper the-vec '() 0))

;; 23.5  Write a procedure vector-map that takes two arguments, 
;; a function and a vector, 
;; and returns a new vector in which each box contains the result of applying the function to the corresponding element of the argument vector. 
(define (vec-map-helper the-func the-vec output counter)
  (cond [(equal? counter (vector-length output)) output]
        [else
         (begin
           (vector-set! output counter (the-func (vector-ref the-vec counter)))
           (vec-map-helper the-func the-vec output (+ 1 counter)))]))

(define (my-vector-map the-func the-vec)
  (vec-map-helper the-func the-vec (make-vector (vector-length the-vec)) 0))

;; 23.6  Write a procedure vector-map! that takes two arguments, 
;; a function and a vector, 
;; and modifies the argument vector by replacing each element with the result of applying the function to that element. 
;; Your procedure should return the same vector. 

(module+ test
  (require rackunit)
  (check-true #t)
  (define (check-three-things-equal? result their-append-rsl my-append-rsl)
  (unless (and (check-equal? result their-append-rsl)
               (check-equal? result my-append-rsl))
    (fail-check)))

  ; 23.1
  (check-equal? (sum-vector '#(6 7 8))   21)
  (check-equal? (sum-vector '#(1 2 3))   6)
  (check-equal? (sum-vector '#(1 2 3 4)) 10)

  ; 23.2
  (check-equal? (my-vector-fill! #(one two three four) 'yeah)
                #(yeah yeah yeah yeah))
  (check-equal? (my-vector-fill! #(one two three four five) 'no)
                #(no no no no no))

  ; 23.3
  (check-equal? (my-vector-append '#(not a) '#(second time))
                #(not a second time))

  (check-equal? (my-vector-append #(a b c) #(d e f))
                #(a b c d e f))

  ; 23.4
  (check-equal? (my-vector->list '#(dah dah didah))
                '(dah dah didah))

  ; 23.5
  (check-equal? (my-vector-map more:square '#(2 3 4)) '#(4 9 16))
  (check-equal? (my-vector-map cadr '#((a b) (d e) (g h)))
#(b e h)
)


#|
  (check-equal?  )
|#


) ;; end module+ test 

