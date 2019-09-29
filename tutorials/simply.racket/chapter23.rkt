#lang simply-scheme

; Chapter 23 Vectors

(require (prefix-in more: "more-simply.rkt"))
(require (prefix-in srfi-69: srfi/69)) ; hash tables

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

(define (m-v-m-helper! the-func the-vec counter)
  (cond [(equal? counter (vector-length the-vec)) the-vec]
        [else
          (begin
            (let ([temp-data (the-func (vector-ref the-vec counter))])
              (vector-set! the-vec counter temp-data)
              (m-v-m-helper! the-func the-vec (+ 1 counter))))]))

(define (my-vector-map! the-func the-vec)
  (m-v-m-helper! the-func the-vec 0))

#|
23.7  Could you write vector-filter? How about vector-filter!? 
Explain the issues involved. 
You would have to run through the vector twice for vector-filter: 
once to filter, and iterate through the first result to get rid of empties
or maybe three times depending on how/when you want to count empties
vector-filter! would be bad, because then you have some values that are changed
and some that are not in the original vector
|#

;; 23.8  Modify the lap procedure to print "Car 34 wins!"
;; when car 34 completes its 200th lap. 
;; (A harder but more correct modification is to print the message 
;; only if no other car has completed 200 laps.) 
(define (my-lap-23-8 car-number)
  (vector-set! *lap-vector*
	       car-number
	       (+ (vector-ref *lap-vector* car-number) 1))
  (when (and (equal? 34 car-number)
             (equal? 200 (vector-ref *lap-vector* car-number)))
    (more:display-all "Car 34 wins!"))
  (vector-ref *lap-vector* car-number))

;; make a more manageable lap vector
(define *my-lap-v* (make-vector 10 0))
(define (my-lap car-number the-vec)
  (vector-set! the-vec
	       car-number
	       (+ (vector-ref the-vec car-number) 1))
  (vector-ref the-vec car-number))

(define (my-leader the-vec)
  (my-leader-helper 0 1 the-vec))

(define (my-leader-helper leader index the-vec)
  (more:display-all "in my-leader helper with leader " leader
                    ", index: " index ", vector: " the-vec)
  (cond [(= index (vector-length the-vec)) leader]
        [(> (my-lap index the-vec) (my-lap leader the-vec))
         (my-leader-helper index (+ index 1) the-vec)]
        [else (my-leader-helper leader (+ index 1) the-vec)]))

(define lap-a (list->vector '(4 5 6 3 1 2 2 7 1 6)))

; 23.10
; their version does not work because the "lap" function changes the vector
; I should have seen that
; I guess I would have if I went in order
; And "leader" will return the index, not the value

;; 23.9 Write a procedure leader that says which car is in the lead right now. 
(define (my-leader-2 the-vec)
  (my-leader-helper-2 0 1 the-vec))

(define (my-leader-helper-2 leader index the-vec)
  #| (more:display-all "in my-leader-helper-2 with leader " leader ", index: " index ", vector: " the-vec) |#

  (cond [(= index (vector-length the-vec)) (vector-ref the-vec leader)]
        [(> (vector-ref the-vec index) (vector-ref the-vec leader))
         (my-leader-helper-2 index (+ index 1) the-vec)]
        [else (my-leader-helper-2 leader (+ index 1) the-vec)]))

;;  23.11  In some restaurants, the servers use computer terminals to keep track of what each table has ordered. 
; Every time you order more food, the server enters your order into the computer. 
; When you're ready for the check, the computer prints your bill.

; You're going to write two procedures, order and bill. 
; Order takes a table number and an item as arguments and adds the cost of that item to that table's bill. 
; Bill takes a table number as its argument, 
; returns the amount owed by that table, 
; and resets the table for the next customers. 
; (Your order procedure can examine a global variable *menu* to find the price of each item.)

;> (order 3 'potstickers)
;> (order 3 'wor-won-ton)
;> (order 5 'egg-rolls)
;> (order 3 'shin-shin-special-prawns)
;> (bill 3)
;13.85
;> (bill 5)
;2.75

(define *menu* (srfi-69:make-hash-table))
(srfi-69:hash-table-set! *menu* 'potstickers 3.50)
(srfi-69:hash-table-set! *menu* 'wor-won-ton 4)
(srfi-69:hash-table-set! *menu* 'shin-shin-special-prawns 6.35)
(srfi-69:hash-table-set! *menu* 'egg-rolls 2.75)

(define *23-11-tables* (make-vector 10 0))

(define (order table-num the-item)
  (let ([temp-hold (vector-ref *23-11-tables* table-num)])
    (vector-set! *23-11-tables* 
                 table-num 
                 (+ temp-hold 
                    (srfi-69:hash-table-ref *menu* the-item)))))

(define (bill table-num)
  (let ([temp-hold (vector-ref *23-11-tables* table-num)])
    (begin
      (vector-set! *23-11-tables* table-num 0)
      temp-hold)))

; 23.12  Rewrite selection sort (from Chapter 15) to sort a vector. 
; This can be done in a way similar to the procedure for shuffling a deck: 
; Find the smallest element of the vector and exchange it (using vector-swap!) with the value in the first box. 
; Then find the smallest element not including the first box, 
; and exchange that with the second box, and so on. 
; For example, suppose we have a vector of numbers:

;#(23 4 18 7 95 60)

; Your program should transform the vector through these intermediate stages:

;#(4 23 18 7 95 60)   ; exchange 4 with 23
;#(4 7 18 23 95 60)   ; exchange 7 with 23
;#(4 7 18 23 95 60)   ; exchange 18 with itself
;#(4 7 18 23 95 60)   ; exchange 23 with itself
;#(4 7 18 23 60 95)   ; exchange 60 with 95

(define (vector-swap! vector index1 index2)
  (let ((temp (vector-ref vector index1)))
    (vector-set! vector index1 (vector-ref vector index2))
    (vector-set! vector index2 temp)))

(define (get-smallest-vec-index the-vec small-index index)
  (when (< index (vector-length the-vec))
      (more:display-all "smallest: the-vec: " the-vec ", small-index: "
                    small-index ", value: " (vector-ref the-vec small-index)
                    ", index: " index ", value: " (vector-ref the-vec index))
)


  (cond [(equal? index (vector-length the-vec)) small-index]
        [(< (vector-ref the-vec small-index) (vector-ref the-vec index))
         (get-smallest-vec-index the-vec small-index (+ 1 index))]
        [else (get-smallest-vec-index the-vec index (+ 1 index))]))

(define (vector-sort-helper the-vec index) 
  #true)

(define (vector-sort the-vec)
  (vector-sort-helper the-vec 0))

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
  (check-equal? (my-vector-map more:square (list->vector '(2 3 4))) '#(4 9 16))
  (check-equal? (my-vector-map cadr '#((a b) (d e) (g h))) #(b e h))

  ; 23.6
  (begin
    (define vec-23-6-a (list->vector '(2 3 4)))
    (define vec-23-6-b (my-vector-map! more:square vec-23-6-a))
    (check-equal? vec-23-6-a vec-23-6-b))
 
  ; 23.9
  (check-equal? (my-leader-2 (list->vector '(4 5 6 3 8 2 2 7 1 6))) 8)
  (check-equal? (my-leader-2 (list->vector '(4 5 6 3 1 2 2 7 1 6))) 7)

  ; 23.11
  (order 3 'potstickers)
  (order 3 'wor-won-ton)
  (order 5 'egg-rolls)
  (order 3 'shin-shin-special-prawns)
  
  (check-equal? (bill 3) 13.85)
  (check-equal? (bill 5) 2.75)
  (check-equal? (bill 2) 0)
  (check-equal? (bill 5) 0)

  ; 23.12
  (check-equal? (get-smallest-vec-index (list->vector '(23 3 18 7 95 60)) 0 0) 1)
  (check-equal? (get-smallest-vec-index (list->vector '(60 95 7 18 3 23)) 0 0) 4)
  (more:display-all "done")


#|
  (check-equal?  )
|#


) ;; end module+ test 

