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
  (let ([temp (vector-ref vector index1)])
    (vector-set! vector index1 (vector-ref vector index2))
    (vector-set! vector index2 temp)))

(define (get-smallest-vec-index-helper the-vec small-index index)
  (cond [(equal? index (vector-length the-vec)) small-index]
        [(< (vector-ref the-vec small-index) (vector-ref the-vec index))
         (get-smallest-vec-index-helper the-vec small-index (+ 1 index))]
        [else (get-smallest-vec-index-helper the-vec index (+ 1 index))]))

(define (get-smallest-vec-index the-vec index)
  (get-smallest-vec-index-helper the-vec index index))

(define (vector-sort-helper the-vec count) 
  (cond [(equal? count (vector-length the-vec)) the-vec]
        [else
         (begin 
           (vector-swap! the-vec 
                         count 
                         (get-smallest-vec-index the-vec count))
           (vector-sort-helper the-vec
                               (+ 1 count)))]))

(define (vector-sort the-vec)
  (vector-sort-helper the-vec 0))

;; 23.13 will not work because it will set two slots to the same value.
;; This is because there is no temp variable.

;; 23.14  Implement a two-dimensional version of vectors. 
;; (We'll call one of these structures a matrix.) 
;; The implementation will use a vector of vectors. 
;; For example, a three-by-five matrix will be a three-element vector, 
;; in which each of the elements is a five-element vector. Here's how it should work:
; > (define m (make-matrix 3 5))
; > (matrix-set! m 2 1 '(her majesty))
; > (matrix-ref m 2 1)
; (HER MAJESTY)

(define (matrix-ref the-m row column)
  (vector-ref (vector-ref the-m row)
              column))

(define (matrix-set! matrix row column value)
  (vector-set! (vector-ref matrix row) column value ))

(define (build-matrix the-m counter num-el-in-vecs)
  (cond [(equal? counter (vector-length the-m)) the-m]
        [else (begin
                (vector-set! the-m counter (make-vector num-el-in-vecs 0))
                (build-matrix the-m (+ 1 counter) num-el-in-vecs))]))

(define (make-matrix num-vecs num-el-in-vecs)
  (build-matrix (make-vector num-vecs) 0 num-el-in-vecs))

; 23.15  Generalize Exercise 23.14 by implementing an array structure that can have any number of dimensions. 
; Instead of taking two numbers as index arguments, as the matrix procedures do,
; the array procedures will take one argument, a list of numbers. 
; The number of numbers is the number of dimensions, and it will be constant for any particular array. 
; For example, here is a three-dimensional array (4×5×6):
; > (define a1 (make-array '(4 5 6)))
; > (array-set! a1 '(3 2 3) '(the end))

#| here is 5x3
'#(#((George Washington) Virginia None)
   #((John Adams) Maaaas Federalist)
   #((Millard Fillmore) (New York) Whig)
   #((Ulysses Grant) Ohio Republican)
   #((Harry Truman) Missouri Democratic))
'#(#((George Washington) Virginia None)
   #((John Adams) Maaaas Federalist)
   #((Millard Fillmore) (New York) Whig)
   #((Ulysses Grant) Ohio Republican)
   #((Harry Truman) Missouri Democratic))
|#

; 23.16  We want to reimplement sentences as vectors instead of lists.
; (a) Write versions of sentence, empty?, first, butfirst, last, and butlast 
; that use vectors. Your selectors need only work for sentences, not for words.
;> (sentence 'a 'b 'c)
;#(A B C)
;> (butfirst (sentence 'a 'b 'c))
;#(B C)
;(You don't have to make these procedures work on lists as well as vectors!)

(define (v-sentence-helper the-vec the-list counter)
  (cond [(empty? the-list) the-vec]
        [else (begin
                (vector-set! the-vec counter (car the-list))
                (v-sentence-helper the-vec (cdr the-list) (+ 1 counter)))]))

(define (v-sentence word . word-list)
  (v-sentence-helper (make-vector (count (cons word word-list))) 
                     (cons word word-list) 
                     0))

(define (v-first the-vec)
  (vector-ref the-vec 0))

(define (v-last the-vec)
  (vector-ref the-vec (- (vector-length the-vec) 1)))

(define (v-bf-helper orig-vec output-vec counter)
  (cond [(equal? counter (vector-length orig-vec)) output-vec]
        [else
         (begin
           (vector-set! output-vec (- counter 1) (vector-ref orig-vec counter))
           (v-bf-helper orig-vec output-vec (+ 1 counter)))]))

 (define (v-butfirst the-vec)
   (v-bf-helper the-vec (make-vector (- (vector-length the-vec) 1)) 1 ))

(define (v-bl-helper orig-vec output-vec counter)
  (cond [(equal? counter (vector-length output-vec)) output-vec]
        [else
         (begin
           (vector-set! output-vec counter (vector-ref orig-vec counter))
           (v-bl-helper orig-vec output-vec (+ 1 counter)))]))

(define (v-butlast the-vec)
  (v-bl-helper the-vec (make-vector (- (vector-length the-vec) 1)) 0))

(define (v-empty? the-vec)
  (cond [(equal? 0 (vector-length the-vec)) #t]
        [else #f]))

;(b) Does the following program still work with the new implementation of sentences? If not, fix the program.
;; okay, so I changed the call
(define (praise stuff)
  (v-sentence stuff 'is 'good))

;(c) Does the following program still work with the new implementation of sentences? If not, fix the program.

;(define (praise stuff)
;  (sentence stuff 'rules!))

;(d) Does the following program still work with the new implementation of sentences? 
; If not, fix the program. If so, is there some optional rewriting that would improve its performance?

(define (v-item n sent)
  (cond [(< n 1) #f]
        [(> n (vector-length sent)) #f] 
        [(= n 1) (v-first sent)]
        [else (v-item (- n 1) (v-butfirst sent))]))

;(e) Does the following program still work with the new implementation of sentences?
; If not, fix the program. If so, is there some optional rewriting that would improve its performance?
; this makes nested vectors for me
; gotta use a helper
(define (v-every fn sent)
  (more:display-all "in v-very with sent: " sent)
  (cond [(v-empty? sent) sent]
        [else (v-sentence (fn (v-first sent)) 
                          (v-every fn (v-butfirst sent)))]))

(define (v-e-helper fn orig-v out-v counter)
  (cond [(= counter (vector-length orig-v)) out-v]
        [else (begin
                (vector-set! out-v counter (fn (vector-ref orig-v counter)))
                (v-e-helper fn orig-v out-v (+ 1 counter)))]))

(define (v-every-r fn sent)
  (v-e-helper fn sent (make-vector (vector-length sent)) 0))

;(f) In what ways does using vectors to implement sentences affect the speed of the selectors and constructor? Why do you think we chose to use lists? 
;; I think I could have combined sentence, butfirst and butlast into one function
;; with another argument if I did this with lists. I might try that just to 
;; see if it can be done.

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
  (check-equal? (get-smallest-vec-index (list->vector '(23 3 18 7 95 60)) 0) 1)
  (check-equal? (get-smallest-vec-index (list->vector '(60 95 7 18 3 23)) 0) 4)

  (check-equal? (get-smallest-vec-index (list->vector '(23 3 18 7 95 60)) 2) 3)
  (check-equal? (get-smallest-vec-index (list->vector '(60 95 7 18 3 23)) 2) 4)
  (check-equal? (vector-sort (list->vector '(23 4 18 7 95 60)))
                             (list->vector '(4 7 18 23 60 95)))


  ; 23.14
  (define prez (make-matrix 5 3))
  (matrix-set! prez 0 0 '(George Washington))
  (matrix-set! prez 0 1 'Virginia)
  (matrix-set! prez 0 2 'None)
  (matrix-set! prez 1 0 '(John Adams)) ; our only Federalist Pres
  (matrix-set! prez 1 1 'Maaaas)
  (matrix-set! prez 1 2 'Federalist)
  (matrix-set! prez 2 0 '(Millard Fillmore))
  (matrix-set! prez 2 1 '(New York))
  (matrix-set! prez 2 2 'Whig)
  (matrix-set! prez 3 0 '(Ulysses Grant))
  (matrix-set! prez 3 1 'Ohio)
  (matrix-set! prez 3 2 'Republican)
  (matrix-set! prez 4 0 '(Harry Truman))
  (matrix-set! prez 4 1 'Missouri)
  (matrix-set! prez 4 2 'Democratic)

  ; 23.16 a
  (check-equal? (v-sentence 'a 'b 'c) (list->vector '(a b c)))
  (define q (v-sentence 7 8 9))
  (check-equal? (v-first q) 7)
  (check-equal? (v-last q) 9)
  (check-equal? (v-butfirst q) (list->vector '(8 9)))
  (check-equal? (v-butlast q) (list->vector '(7 8)))

  ; 23.16 b
  (check-equal? (praise 'Racket) (v-sentence 'Racket 'is 'good))

  ; 23.16 d
  (define w (v-sentence 'a 'b 'c 'd 'e))
  (check-equal? (v-item 0 w) #f)
  (check-equal? (v-item 1 w) 'a)
  (check-equal? (v-item 2 w) 'b)
  (check-equal? (v-item 3 w) 'c)
  (check-equal? (v-item 4 w) 'd)  
  (check-equal? (v-item 5 w) 'e)
  (check-equal? (v-item 6 w) #f)

  ; 23.16 e
  ; (v-every-r fn sent)
  (define e (v-sentence 2 3 4 5))
  (check-equal? (v-every-r more:square e) (v-sentence 4 9 16 25))
  (define (double-num x)
    (* x 2))
  (check-equal? (v-every-r double-num e) (v-sentence 4 6 8 10))

  (more:display-all "done")


#|
  (check-equal?  )
|#


) ;; end module+ test 

