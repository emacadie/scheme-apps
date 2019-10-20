#lang simply-scheme

; Chapter 23 Vectors, problem 15: arrays

(require (prefix-in more: "more-simply.rkt"))
(require (prefix-in srfi-69: srfi/69)) ; hash tables

;; Chapter 23 Vectors
(butfirst '(this is chapter 23 problem 15: vectors and arrays))

; I will code 2-d vector
; Then 3-d (which might call 2-d)
; Then 4-d (which might call 3-d)
; and go from there
; This is how they introduce recursion in chapter 11

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

;; 23.14  Implement a two-dimensional version of vectors. 
;; (We'll call one of these structures a matrix.) 
;; The implementation will use a vector of vectors. 
;; For example, a three-by-five matrix will be a three-element vector, 
;; in which each of the elements is a five-element vector. Here's how it should work:
; > (define m (make-matrix 3 5))
; > (matrix-set! m 2 1 '(her majesty))
; > (matrix-ref m 2 1)
; (HER MAJESTY)


; adding validation
(define (get-matrix-dimensions matrix)
  (list
   (vector-length matrix)
   (vector-length (vector-ref matrix 0))))

(define (within-matrix-dimensions? the-m row column)
  (let ([the-dims (get-matrix-dimensions the-m)])
    (cond [(or (> 0 row) (> 0 column)) #f]
          [(or (> row (- (car the-dims) 1)) (> column (- (list-ref the-dims 1) 1))) #f]
          [else #t])))

(define (matrix-ref the-m row column)
  (cond [(not (within-matrix-dimensions? the-m row column)) #f]
        [else (vector-ref (vector-ref the-m row) column)]))

(define (matrix-set! matrix row column value)
  (cond [(not (within-matrix-dimensions? matrix row column)) #f]
        [else (vector-set! (vector-ref matrix row) column value)]))

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


(define (get-2d-vec-dimensions matrix)
  (list (vector-length matrix)
        (vector-length (vector-ref matrix 0))))

(define (get-3d-vec-dimensions matrix)
  (cons (vector-length matrix)
        (get-2d-vec-dimensions (vector-ref matrix 0))))

(define (get-4d-vec-dimensions matrix)
  (cons (vector-length matrix)
        (get-3d-vec-dimensions (vector-ref matrix 0))))

(define (get-5d-vec-dimensions matrix)
  (cons (vector-length matrix)
        (get-4d-vec-dimensions (vector-ref matrix 0))))

(define (within-2d-vec-dimensions? the-m dims-in)
  (let ([the-dims (get-2d-vec-dimensions the-m)]
        [row (car dims-in)]
        [column (list-ref dims-in 1)])
    (cond [(or (> 0 row) (> 0 column)) #f]
          [(or (> row (- (car the-dims) 1)) 
               (> column (- (list-ref the-dims 1) 1))) #f]
          [else #t])))

(define (within-3d-vec-dimensions? the-m dims-in)
  (let ([the-dims (get-3d-vec-dimensions the-m)]
        [row (car dims-in)]
        [columns (cdr dims-in)])
    (cond [(> 0 row) #f]
          [(> row (- (car the-dims) 1))  #f]
          [else (within-2d-vec-dimensions? (vector-ref the-m 0) columns)])))

(define (within-4d-vec-dimensions? the-m dims-in)
  (let ([the-dims (get-4d-vec-dimensions the-m)]
        [row (car dims-in)]
        [columns (cdr dims-in)])
    (cond [(> 0 row) #f]
          [(> row (- (car the-dims) 1))  #f]
         [else (within-3d-vec-dimensions? (vector-ref the-m 0) columns)])))

(define (within-5d-vec-dimensions? the-m dims-in)
  (let ([the-dims (get-5d-vec-dimensions the-m)]
        [row (car dims-in)]
        [columns (cdr dims-in)])
    (cond [(> 0 row) #f]
          [(> row (- (car the-dims) 1))   #f]
         [else (within-4d-vec-dimensions? (vector-ref the-m 0) columns)])))

(define (2d-vec-ref the-m dims-in)
  (cond [(not (within-2d-vec-dimensions? the-m dims-in)) #f]
        [else (vector-ref (vector-ref the-m (car dims-in)) 
                          (list-ref dims-in 1))]))

(define (3d-vec-ref the-m dims-in)
  (cond [(not (within-3d-vec-dimensions? the-m dims-in)) #f]
        [else (2d-vec-ref (vector-ref the-m (car dims-in))
                          (cdr dims-in))]))

(define (4d-vec-ref the-m dims-in)
  (cond [(not (within-4d-vec-dimensions? the-m dims-in)) #f]
        [else (3d-vec-ref (vector-ref the-m (car dims-in))
                          (cdr dims-in))]))

(define (5d-vec-ref the-m dims-in)
  (cond [(not (within-4d-vec-dimensions? the-m dims-in)) #f]
        [else (4d-vec-ref (vector-ref the-m (car dims-in))
                          (cdr dims-in))]))

(define (2d-vec-set! matrix dims-in value)
  (cond [(not (within-2d-vec-dimensions? matrix dims-in)) #f]
        [else (vector-set! (vector-ref matrix (car dims-in)) 
                           (list-ref dims-in 1) 
                           value)]))

(define (3d-vec-set! matrix dims-in value)
  (cond [(not (within-3d-vec-dimensions? matrix dims-in)) #f]
        [else (2d-vec-set! (vector-ref matrix (car dims-in))
                           (cdr dims-in)
                           value)]))

(define (4d-vec-set! matrix dims-in value)
  (cond [(not (within-4d-vec-dimensions? matrix dims-in)) #f]
        [else (3d-vec-set! (vector-ref matrix (car dims-in))
                           (cdr dims-in)
                           value)]))

(define (5d-vec-set! matrix dims-in value)
  (cond [(not (within-5d-vec-dimensions? matrix dims-in)) #f]
        [else (4d-vec-set! (vector-ref matrix (car dims-in))
                           (cdr dims-in)
                           value)]))

#|
(define test-vec (make-vector 5 0))
test-vec
'#(0 0 0 0 0)
(initialize-final-vec test-vec 2 0)
|#

(define global-vec-num 0)

(define (reset-global-vec-num)
  (set! global-vec-num 0))

(define (initialize-final-vec the-vec index)
  (if (>= index (vector-length the-vec))
      the-vec
      (begin
        (vector-set! the-vec 
                     index 
                     (string->symbol 
                      (string-append "x-" 
                                     (number->string global-vec-num))))
        (set! global-vec-num (+ 1 global-vec-num))
        (initialize-final-vec the-vec (+ index 1)))))

(define (2d-vec-builder the-m counter num-el-in-vecs)
  (cond [(equal? counter (vector-length the-m)) the-m]
        [else (begin
                (vector-set! the-m 
                             counter 
                             (initialize-final-vec (make-vector num-el-in-vecs) 
                                                   (+ 0)))
                (2d-vec-builder the-m 
                                (+ 1 counter) 
                                num-el-in-vecs))]))

(define (3d-vec-builder 3d-vec counter dims-list)
  (cond [(equal? counter (vector-length 3d-vec)) 3d-vec]
        [else (begin
                (vector-set! 3d-vec 
                             counter 
                             (make-2d-vec dims-list))
                (3d-vec-builder 3d-vec 
                                (+ 1 counter) 
                                dims-list ))]))
;; dims-list has the remaining dimensions
(define (4d-vec-builder 4d-vec counter dims-list)
  (cond [(equal? counter (vector-length 4d-vec)) 4d-vec]
        [else (begin
                (vector-set! 4d-vec 
                             counter 
                             (make-3d-vec dims-list ))
                (4d-vec-builder 4d-vec 
                                (+ 1 counter) 
                                dims-list ))]))

(define (5d-vec-builder 5d-vec counter dims-list)
  (cond [(= 5 (count dims-list))
         (5d-vec-builder (make-vector (car dims-list)) 
                         0 
                         (cdr dims-list))] 
        [(equal? counter (vector-length 5d-vec)) 5d-vec]
        [else (begin
                (vector-set! 5d-vec 
                             counter 
                             ; (make-4d-vec dims-list)
                             (4d-vec-builder (make-vector (car dims-list)) 
                                             0 
                                             (cdr dims-list))
)
                (5d-vec-builder 5d-vec 
                                (+ 1 counter) 
                                dims-list ))]))

(define (make-2d-vec dim-list)
  (2d-vec-builder (make-vector (car dim-list)) 
                  0 
                  (list-ref dim-list 1)))

(define (make-3d-vec dims-list)
  (3d-vec-builder (make-vector (car dims-list)) 
                  0 
                  (cdr dims-list)))

(define (make-4d-vec dims-list)
  (4d-vec-builder (make-vector (car dims-list)) 
                  0 
                  (cdr dims-list))
)

#|
(define (make-5d-vec dims-list init-val)
  (5d-vec-builder (make-vector (car dims-list)) 
                  0 
                  (cdr dims-list) 
                  init-val))
|#

#|
(reset-global-vec-num)
(define a3 (make-3d-vec '(4 5 6) 0))
(3d-vec-set! a3 '(3 2 3) 'whatever)
(3d-vec-ref a3 '(3 2 3))
(3d-vec-ref a3 '(3 3 2))

(define a4 (make-4d-vec '(4 5 3 6) 0))
(4d-vec-set! a4 '(3 3 2 4) 'whatever)
(4d-vec-ref a4 '(3 3 2 4))
(4d-vec-ref a4 '(3 3 2))

(define a5 (make-5d-vec '(4 5 3 2 4) 0))
(4d-vec-set! a4 '(3 3 2 4) 'whatever)
(4d-vec-ref a4 '(3 3 2 4))
(4d-vec-ref a4 '(3 3 2))


|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; now try to generalize it

(define (get-multi-dims-helper matrix dim-list)
  (cond [(not (vector? matrix)) dim-list]
        [else (get-multi-dims-helper (vector-ref matrix 0) 
                                     (append dim-list 
                                             (list (vector-length matrix))))]))

(define (get-multi-dimensions matrix)
  (get-multi-dims-helper matrix '()))

(define (within-multi-dims-helper m-dims dim-list result)
  (cond [(not (equal? (count m-dims) (count dim-list))) #f]
        [(empty? m-dims) result]
        [(> 0 (car dim-list)) #f]
        [(> (+ 1 (car dim-list)) (car m-dims)) #f]
        [else (within-multi-dims-helper (cdr m-dims) (cdr dim-list) result)]))

(define (within-multi-dimensions? matrix dim-list)
  (within-multi-dims-helper (get-multi-dimensions matrix) dim-list #t))

(define (multi-vec-ref-helper matrix dims-path result)
  (cond [(equal? 1 (count dims-path)) (vector-ref matrix (car dims-path))]
        [else (multi-vec-ref-helper (vector-ref matrix (car dims-path))
                                    (cdr dims-path)
                                    #f)]))

(define (multi-vec-ref matrix dims-path)
  (cond [(not (within-multi-dimensions? matrix dims-path)) #f]
        [else (multi-vec-ref-helper matrix dims-path #f)]))

(define (multi-vec-set-helper matrix dims-path value)
  (cond [(equal? 1 (count dims-path)) (vector-set! matrix 
                                                   (car dims-path) 
                                                   value)]
        [else (multi-vec-set-helper (vector-ref matrix (car dims-path)) 
                                    (cdr dims-path) 
                                    value)]))

(define (multi-vec-set! matrix dims-path value)
  (cond [(not (within-multi-dimensions? matrix dims-path)) #f]
        [else (multi-vec-set-helper matrix dims-path value)]))

#|
(define test-vec (make-vector 5 0))
test-vec
'#(0 0 0 0 0)
(initialize-final-vec test-vec 2 0)
|#

(define global-vec-numx 0)

(define (reset-global-vec-numx)
  (set! global-vec-numx 0))

(define (initialize-final-vecx the-vec index)
  (if (>= index (vector-length the-vec))
      the-vec
      (begin
        (vector-set! the-vec 
                     index 
                     (string->symbol 
                      (string-append "x-" 
                                     (number->string global-vec-numx))))
        (set! global-vec-numx (+ 1 global-vec-numx))
        (initialize-final-vecx the-vec 
                               (+ index 1)))))

(define (2d-vec-builderx the-m counter num-el-in-vecs)
  (more:display-all "2d-vec-builder with counter " counter)
  (cond [(equal? counter (vector-length the-m)) the-m]
        [else (begin
                (vector-set! the-m 
                             counter 
                             (initialize-final-vecx (make-vector num-el-in-vecs)
                                                    (+ 0)))
                (2d-vec-builderx the-m 
                                 (+ 1 counter) 
                                 num-el-in-vecs))]))

(define (3d-vec-builderx 3d-vec counter dims-list)
  (cond [(equal? counter (vector-length 3d-vec)) 3d-vec]
        [else (begin
                (vector-set! 3d-vec 
                             counter 
                             (make-2d-vecx dims-list))
                (3d-vec-builderx 3d-vec 
                                 (+ 1 counter) 
                                 dims-list))]))
;; dims-list has the remaining dimensions
(define (4d-vec-builderx 4d-vec counter dims-list)
  (cond [(equal? counter (vector-length 4d-vec)) 4d-vec]
        [else (begin
                (vector-set! 4d-vec 
                             counter 
                             (make-3d-vecx dims-list))
                (4d-vec-builderx 4d-vec 
                                 (+ 1 counter) 
                                 dims-list))]))

(define (5d-vec-builderx 5d-vec counter dims-list)
  (cond [(= 5 (count dims-list))   
         (5d-vec-builderx (make-vector (car dims-list)) 
                          0 
                          (cdr dims-list))] 
        [(equal? counter (vector-length 5d-vec)) 5d-vec]
        [else (begin
                (vector-set! 5d-vec 
                             counter 
                             (make-4d-vecx dims-list))
                (5d-vec-builderx 5d-vec 
                                 (+ 1 counter) 
                                 dims-list))]))

(define (make-5d-vecx dims-list)
  #f
)

(define (make-2d-vecx dim-list)
  (2d-vec-builderx (make-vector (car dim-list)) 
                   0 
                   (list-ref dim-list 1)))

(define (make-3d-vecx dims-list)
  (3d-vec-builderx (make-vector (car dims-list)) 
                   0 
                   (cdr dims-list)))

(define (make-4d-vecx dims-list)
  (4d-vec-builderx (make-vector (car dims-list)) 
                   0 
                   (cdr dims-list)))




; line 554 2019-10-14_02.00.07
(module+ test
  (require rackunit)
  (check-true #t)
  

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
  
  (check-equal? (get-matrix-dimensions prez) '(5 3))
  (check-equal? (within-matrix-dimensions? prez 5 2) #f)
  (check-equal? (within-matrix-dimensions? prez 6 2) #f )
  (check-equal? (within-matrix-dimensions? prez 4 2) #t)

  (check-equal? (within-matrix-dimensions? prez 4 3) #f)
  (check-equal? (within-matrix-dimensions? prez 4 1) #t)
  (check-equal? (within-matrix-dimensions? prez 4 -1) #f)
  (check-equal? (matrix-set! prez 5 1 'Invalid) #f)
  (check-equal? (matrix-ref prez 4 1) 'Missouri)
  (check-equal? (matrix-ref prez 4 3) #f)

  ; 23.15
; (make-2d-vec dim-list)
  (reset-global-vec-num)
  (define prez2 (make-2d-vec '(5 3)))
  (2d-vec-set! prez2 '(0 0) '(George Washington))
  (2d-vec-set! prez2 '(0 1) 'Virginia)
  (2d-vec-set! prez2 '(0 2) 'None)
  (2d-vec-set! prez2 '(1 0) '(John Adams)) ; our only Federalist Pres
  (2d-vec-set! prez2 '(1 1) 'Maaaas)
  (2d-vec-set! prez2 '(1 2) 'Federalist)
  (2d-vec-set! prez2 '(2 0) '(Millard Fillmore))
  (2d-vec-set! prez2 '(2 1) '(New York))
  (2d-vec-set! prez2 '(2 2) 'Whig)
  (2d-vec-set! prez2 '(3 0) '(Ulysses Grant))
  (2d-vec-set! prez2 '(3 1) 'Ohio)
  (2d-vec-set! prez2 '(3 2) 'Republican)
  (2d-vec-set! prez2 '(4 0) '(Harry Truman))
  (2d-vec-set! prez2 '(4 1) 'Missouri)
  (2d-vec-set! prez2 '(4 2) 'Democratic)

  (check-equal? (get-matrix-dimensions prez2) '(5 3))
  (check-equal? (within-2d-vec-dimensions? prez2 '(5 2)) #f)
  (check-equal? (within-2d-vec-dimensions? prez2 '(6 2)) #f)
  (check-equal? (within-2d-vec-dimensions? prez2 '(4 2)) #t)

  (check-equal? (within-matrix-dimensions? prez2 4 3) #f)
  (check-equal? (within-matrix-dimensions? prez2 4 1) #t)
  (check-equal? (within-matrix-dimensions? prez2 4 -1) #f)
  (check-equal? (2d-vec-set! prez2 '(5 1) 'Invalid) #f)
  
  (check-equal? (2d-vec-ref prez2 '(4 1)) 'Missouri)
  (check-equal? (2d-vec-ref prez2 '(4 3)) #f)
  (check-equal? (matrix-ref prez 4 1) (2d-vec-ref prez2 '(4 1)))
  (check-equal? (matrix-ref prez 4 3) (2d-vec-ref prez2 '(4 3)))

  ;; 3-d vecs
  (reset-global-vec-num)
  (define a-3 (make-3d-vec '(4 5 6)))
  (3d-vec-set! a-3 '(3 2 3) 'whatever)
  (check-equal? (3d-vec-ref a-3 '(3 2 3)) 'whatever)

  ; (3d-vec-ref a-3 '(3 3 2))

  ; test generic against specific
  (reset-global-vec-num)
  (define adams-3 (make-3d-vec '(4 5 6)))
  (reset-global-vec-num)
  (define alexander-4 (make-4d-vec '(4 5 3 6)))
  (reset-global-vec-num)
  ; (define bond-5 (make-5d-vec '(4 5 3 2 4)))
  (define bond-5 (5d-vec-builder '() 0 '(4 5 3 2 4)))


  (more:check-three-things-equal? '(5 3)
                                  (get-multi-dimensions prez2)
                                  (get-2d-vec-dimensions prez2))

  (more:check-three-things-equal? '(4 5 6)
                                  (get-multi-dimensions adams-3)
                                  (get-3d-vec-dimensions adams-3))

  (more:check-three-things-equal? '(4 5 3 6)
                                  (get-multi-dimensions alexander-4)
                                  (get-4d-vec-dimensions alexander-4))

  (more:check-three-things-equal? '(4 5 3 2 4)
                                  (get-multi-dimensions bond-5)
                                  (get-5d-vec-dimensions bond-5))

  (more:check-three-things-equal? #f
                                  (within-multi-dimensions? prez2 '(5 3))
                                  (within-2d-vec-dimensions? prez2 '(5 3)))

  (more:check-three-things-equal? #f
                                  (within-multi-dimensions? prez2 '(-2 3))
                                  (within-2d-vec-dimensions? prez2 '(-1 3)))

  (more:check-three-things-equal? #t
                                  (within-multi-dimensions? prez2 '(0 0))
                                  (within-2d-vec-dimensions? prez2 '(0 0)))

  (more:check-three-things-equal? #t
                                  (within-multi-dimensions? prez2 '(4 2))
                                  (within-2d-vec-dimensions? prez2 '(4 2)))

  (more:check-three-things-equal? #f ; 
                                  (within-multi-dimensions? bond-5 '(4 5 3 2 4))
                                  (within-5d-vec-dimensions? bond-5 '(4 5 3 2 4)))

  (more:check-three-things-equal? #f ; 
                                  (within-multi-dimensions? bond-5 '(3 5 3 2 4))
                                  (within-5d-vec-dimensions? bond-5 '(3 5 3 2 4)))

  (more:check-three-things-equal? #f ; 
                                  (within-multi-dimensions? bond-5 '(3 4 3 2 4))
                                  (within-5d-vec-dimensions? bond-5 '(3 4 3 2 4)))

  (more:check-three-things-equal? #f ; 
                                  (within-multi-dimensions? bond-5 '(3 4 2 2 4))
                                  (within-5d-vec-dimensions? bond-5 '(3 4 2 2 4)))

  (more:check-three-things-equal? #f ; 
                                  (within-multi-dimensions? bond-5 '(3 4 2 1 4))
                                  (within-5d-vec-dimensions? bond-5 '(3 4 2 1 4)))

  (more:check-three-things-equal? #t ; 
                                  (within-multi-dimensions? bond-5 '(3 4 2 1 3))
                                  (within-5d-vec-dimensions? bond-5 '(3 4 2 1 3)))

(more:check-three-things-equal? #f ; 
                                (within-multi-dimensions? bond-5 '(3 4 2 1 3 0))
                                (within-multi-dimensions? bond-5 '(3 4 2 1)))

(more:check-three-things-equal? 'Democratic 
                                (2d-vec-ref prez2 '(4 2)) 
                                (multi-vec-ref prez2 '(4 2)))

(more:check-three-things-equal? '(Harry Truman)
                                (2d-vec-ref prez2 '(4 0)) 
                                (multi-vec-ref prez2 '(4 0)))

(multi-vec-set! prez2 '(4 0) 'Truman)
(more:check-three-things-equal? 'Truman
                                (2d-vec-ref prez2 '(4 0)) 
                                (multi-vec-ref prez2 '(4 0)))

(more:check-three-things-equal? 'x-342 
                                (5d-vec-ref bond-5 '(2 4 0 1 2))
                                (multi-vec-ref bond-5 '(2 4 0 1 2)))
(multi-vec-set! bond-5 '(2 4 0 1 2) 'Boone)
(more:check-three-things-equal? 'Boone
                                (5d-vec-ref bond-5 '(2 4 0 1 2))
                                (multi-vec-ref bond-5 '(2 4 0 1 2)))

(more:check-three-things-equal? 'x-294
                                (5d-vec-ref bond-5 '(2 2 0 1 2))
                                (multi-vec-ref bond-5 '(2 2 0 1 2)))
(multi-vec-set! bond-5 '(2 2 0 1 2) 'Brown)
(more:check-three-things-equal? 'Brown
                                (5d-vec-ref bond-5 '(2 2 0 1 2))
                                (multi-vec-ref bond-5 '(2 2 0 1 2)))

(more:check-three-things-equal? 'x-94
                                (3d-vec-ref adams-3 '(3 0 4))
                                (multi-vec-ref adams-3 '(3 0 4)))
(multi-vec-set! adams-3 '(3 0 4) 'Bureau)
(more:check-three-things-equal? 'Bureau
                                (3d-vec-ref adams-3 '(3 0 4))
                                (multi-vec-ref adams-3 '(3 0 4)))

(more:check-three-things-equal? 'x-64
                                (3d-vec-ref adams-3 '(2 0 4))
                                (multi-vec-ref adams-3 '(2 0 4)))
(multi-vec-set! adams-3 '(2 0 4) 'Calhoun)
(more:check-three-things-equal? 'Calhoun
                                (3d-vec-ref adams-3 '(2 0 4))
                                (multi-vec-ref adams-3 '(2 0 4)))

(more:check-three-things-equal? #f
                                (4d-vec-ref alexander-4 '(2 0 4 5))
                                (multi-vec-ref alexander-4 '(2 0 4 5)))
(check-equal? (multi-vec-set! alexander-4 '(2 0 4 5) 'Carroll) #f)

(more:check-three-things-equal? #f
                                (4d-vec-ref alexander-4 '(2 0 3 5))
                                (multi-vec-ref alexander-4 '(2 0 3 5)))
(check-equal? (multi-vec-set! alexander-4 '(2 0 3 5) 'Carroll) #f)

(more:check-three-things-equal? 'x-197
                                (4d-vec-ref alexander-4 '(2 0 2 5))
                                (multi-vec-ref alexander-4 '(2 0 2 5)))
(multi-vec-set! alexander-4 '(2 0 2 5) 'Carroll)
(more:check-three-things-equal? 'Carroll
                                (4d-vec-ref alexander-4 '(2 0 2 5))
                                (multi-vec-ref alexander-4 '(2 0 2 5)))


; (multi-vec-ref matrix dims-path)
#|
(define 
  (cond [(not (within-multi-dimensions? matrix dims-path)) #f]
        [else (multi-vec-set-helper matrix dims-path value)]))

(define (2d-vec-setx! matrix dims-in value)
  (cond [(not (within-2d-vec-dimensions? matrix dims-in)) #f]
        [else (vector-set! (vector-ref matrix (car dims-in)) 
                           (list-ref dims-in 1) 
                           value)]))
  (more:check-three-things-equal? '(4 5 6)
                                  (get-multi-dimensions adams-3 '())
                                  (get-3d-vec-dimensions adams-3))

  (more:check-three-things-equal? '(4 5 3 6)
                                  (get-multi-dimensions alexander-4 '())
                                  (get-4d-vec-dimensions alexander-4))

(define (multi-vec-ref matrix dims-path)
  (cond [(not (within-multi-dimensions? matrix dims-path)) #f]
        [else (multi-vec-ref-helper matrix dims-path #f)]))

(define (2d-vec-refx the-m dims-in)

(define (within-multi-dimensions? matrix dim-list)
  (within-multi-dims-helper (get-multi-dimensions matrix '()) dim-list #t))

(define (within-2d-vec-dimensionsx? the-m dims-in)
|#

  (more:display-all "done")


#|
  (check-equal?  )
|#


) ;; end module+ test 

