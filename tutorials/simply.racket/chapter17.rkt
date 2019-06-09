#lang simply-scheme

; Chapter 17: Lists

(require "more-simply.rkt")

(butfirst '(This is chapter 17 lists))

;; Chapter 17 Lists
;; for procedures like cadar and cddadr, read the a's and d's from right to left

#|
From the text:
It's important that you understand how list, cons, and append differ from each other:

> (list '(i am) '(the walrus))
((I AM) (THE WALRUS))

> (cons '(i am) '(the walrus))
((I AM) THE WALRUS)

> (append '(i am) '(the walrus))
(I AM THE WALRUS)
so list will combine elements as lists
cons will flatten the second arg
cons is like conj for lists in Clojure
append flattens all
append takes a list and what you are adding to it
cons adds first arg to front of list (which is second arg)
(assoc 'Colin '((Rod Argent) (Chris White)
		  (Colin Blunstone) (Hugh Grundy) (Paul Atkinson)))
returned '(Colin Blunstone)
So assoc takes a key and a key/value list, and returns the element with 
the key and the value, else returns #f
Totally different than Clojure's assoc.


Concepts introduced in this chapter:
selectors and constructors
list, cons, append
car and cdr
list?, equal?, member?, list-ref, length, assoc
functions taking varying numbers of args
apply function: "It takes two arguments, a procedure and a list"
"Recursion on Arbitrary Structured Lists"
Searching for an element in a multi-level list
Using higher-order functions:
(define (deep-appearances wd structure)
  (if (word? structure)
      (if (equal? structure wd) 
          1 
          0)
      (reduce +
	      (map (lambda (sublist) (deep-appearances wd sublist))
		   structure))))
Using standard recursion (three base cases and two recursive calls)
(define (deep-appearances wd structure)
  (cond ((equal? wd structure) 1)              ; base case: desired word
        ((word? structure) 0)                  ; base case: other word
        ((null? structure) 0)                  ; base case: empty list
        (else (+ (deep-appearances wd (car structure))
                 (deep-appearances wd (cdr structure))))))
I don't think you can do tail-recursion for this stuff.
|#

;; 17.2  For each of the following examples, write a procedure of two arguments that, 
;; when applied to the sample arguments, returns the sample result. 
;; Your procedures may not include any quoted data.
; > (f1 '(a b c) '(d e f))
; ((B C D))
(define (f1 list1 list2)
  (list (append (cdr list1) (car list2))))

;> (f2 '(a b c) '(d e f))
;((B C) E)
(define (f2 list1 list2)
  (cons (cdr list1) (car (cdr list2))))
;> (f3 '(a b c) '(d e f))
;(A B C A B C)
(define (f3 list1 list2)
  (append list1 list1))
;> (f4 '(a b c) '(d e f))
;((A D) (B C E F))
(define (f4 list1 list2)
  (list (list (car list1) (car list2))
        (append (cdr list1) (cdr list2))))

;; 17.4  Describe the result of calling the following procedure with a list as its argument. 
;; (See if you can figure it out before you try it.) 
;; I think it reverses the list
(define (mystery lst)
  (mystery-helper lst '()))

(define (mystery-helper lst other)
  (if (null? lst)
      other
      (mystery-helper (cdr lst) (cons (car lst) other))))

;;  17.5 in a separate file

;; 17.6  Implement append using car, cdr, and cons. 
;; (Note: The built-in append can take any number of arguments. 
;; First write a version that accepts only two arguments. 
;; Then, optionally, try to write a version that takes any number.) 
;; may need a helper method
;; reverse first list, loop through it (with reduce? recursion?), cons each element to last list
;; reverse the first list, add stuff to the front, then reverse it at the end
;; https://github.com/pongsh/simply-scheme-exercises/blob/master/17-lists/17.6.scm does it better than mine
;; but not tail-recursive (and you know I love tail-recursive)
;; I am not handling pairs quite right, but I will go with it
(define (my-append listA listB)
  ; (printf "Calling my-append with listA: ~a and listB: ~a \n" listA listB)
  (cond [(or (null? listB) (empty? listB)) listA]
        [(or (null? listA) (empty? listA)) listB]
        [(and (not (list? listB)) (not (pair? listB))) 
         (begin
           ; (printf "listB is not a list: ~a\n" listB)
           (reverse (cons listB (reverse listA)))
           )
         ]
        
        [else
         (begin
           ; (printf "In else, with (car listB): ~a and (cdr listB): ~a \n" (car listA) (cdr listB))
           (my-append (reverse (cons (car listB) (reverse listA))) (cdr listB))
           )
              ]))

;; again, not perfect, does not handle pairs too well, but I think I will take it
;; Isn't a pair just a list anyway?
;; According to https://stackoverflow.com/questions/6006671/are-pair-and-list-different-in-scheme
;; they are different
;; But the book said they are pretty much the same
(define (append-multi-lists listA . listB)
  ; (printf "-- calling append-multi-lists with listA: ~a and listB: ~a\n" listA listB)
  (cond [(or (null? listB) (empty? listB)) listA]
        [(or (null? listA) (empty? listA)) (apply append-multi-lists listB)] ;; could this be done with car and cdr?
        ; [(or (null? listA) (empty? listA)) (append-multi-lists (car listB) (cdr listB))] ;; could this be done with car and cdr?
        [(and (= (count listB) 1 ) (list? (car listB)) (empty? (car listB))) listA]
        [else
         (begin
           ; (printf "In the else, car listB: ~a cdr listB: ~a\n" (car listB) (cdr listB))
           ; (append-multi-lists (my-append listA (car listB)) (cdr listB))
           (apply append-multi-lists (my-append listA (car listB)) (cdr listB))
           )]))

;; This works with lists, but not if some of the items are solo numbers
(define (append-m-lists-reduce listA . listB)
  (printf "calling append-m-lists-reduce with listA: ~a and listB: ~a\n" listA listB)
  (printf "here is our appended lists: ~a\n" (append '() listA listB))
  (my-append listA (reduce my-append listB))
)

;; from https://github.com/pongsh/simply-scheme-exercises/blob/master/17-lists/17.6.scm
(define (their-append lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (their-append (cdr lst1) lst2))))
(define (their-multi-append lst . rest-of-list )
  (if (null? rest-of-list)
      lst
      (apply their-multi-append (cons (their-append lst (car rest-of-list)) 
                                      (cdr rest-of-list)))))

;; 17.7  Append may remind you of sentence. 
;; They're similar, except that append works only with lists as arguments, whereas sentence will accept words as well as lists. 
;; Implement sentence using append. 
;; (Note: The built-in sentence can take any number of arguments. 
;; First write a version that accepts only two arguments. 
;; Then, optionally, try to write a version that takes any number. 
;; Also, you don't have to worry about the error checking that the real sentence does.) 
;; Also from text:
;; How does our sentence point of view differ from the built-in Scheme point of view using lists? There are three differences:
;; A sentence can contain only words, not sublists.
;; Sentence selectors are symmetrical front-to-back.
;; Sentences and words have the same selectors.

;; I will still check that if it's a list it does not have a sublist
(define (list-with-sublists the-list)
  (cond [(not (list? the-list)) #f]
        [(= 0 (keep list? the-list)) #f]
        [else #t])) 


(define (list-or-item the-item)
  (cond [(not (list? the-item)) (list the-item)]
        [(list-with-sublists the-item) null]
        [else the-item]))


#|
(append (list  'hello) '(this is a ren))
'(hello this is a ren)
chapter17.rkt/test> (append '(what is this) '(this is a ren))
'(what is this this is a ren)
chapter17.rkt/test> (append '(what is this) 're)
'(what is this . re)
chapter17.rkt/test> (append '(what is this) (list 're))
'(what is this re)
|#

(define (sent-via-append list-a list-b)
  (append (list-or-item list-a) (list-or-item list-b))
)


;; 

(module+ test
  (require rackunit)
  (check-true #t)
  ;; they said the dot is not part of the actual list, so I guess this is okay
  (printf "(f1 '(a b c) '(d e f)): ~a \n" (f1 '(a b c) '(d e f)))
  (check-equal? (f1 '(a b c) '(d e f)) '((b c . d)) "Error for (f1 '(a b c) '(d e f))")
  (printf "(f2 '(a b c) '(d e f)): ~a \n" (f2 '(a b c) '(d e f)))
  (check-equal? (f2 '(a b c) '(d e f)) '((b c) . e) "Error for (f2 '(a b c) '(d e f))")
  (printf "(f3 '(a b c) '(d e f)): ~a \n" (f3 '(a b c) '(d e f)))
  (check-equal? (f3 '(a b c) '(d e f)) '(a b c a b c) "Error for (f3 '(a b c) '(d e f))")
  (printf "(f4 '(a b c) '(d e f)): ~a \n" (f4 '(a b c) '(d e f)))
  (check-equal? (f4 '(a b c) '(d e f)) '((a d) (b c e f)) "Error for (f4 '(a b c) '(d e f))")

  ; (printf "(who '(sells out)): ~a \n" (who '(sells out)))
  ; (check-equal? (who '(sells out)) '(pete sells out roger sells out john sells out keith sells out) "Error for (who '(sells out))")
  (printf "(mystery '(1 2 3 4)): ~a \n" (mystery '(1 2 3 4)))
  (check-equal? (mystery '(1 2 3 4)) '(4 3 2 1) "Error for (mystery '(1 2 3 4))")

  (define-check (check-appends-equal? result append-rslt my-append-rslt)
    (unless (and (check-equal? result append-rslt)
                 (check-equal? result my-append-rslt))
      (fail-check)))

  (check-appends-equal? '(get back the word)      (append '(get back) '(the word))      (my-append '(get back) '(the word)))
  (check-appends-equal? '(i am the walrus)        (append '(i am) '(the walrus))        (my-append '(i am) '(the walrus)))
  (check-appends-equal? '(Rod Argent Chris White) (append '(Rod Argent) '(Chris White)) (my-append '(Rod Argent) '(Chris White)))
  ;; from Husk R7RS docs
  (check-appends-equal? '(x y)       (append '(x) '(y))       (my-append '(x) '(y)) )
  (check-appends-equal? '(a b c d)   (append '(a) '(b c d))   (my-append '(a) '(b c d))  )
  (check-appends-equal? '(a (b) (c)) (append '(a (b)) '((c))) (my-append '(a (b)) '((c))) )
  ;; orig from Husk R7RS: (append '(a b) '(c . d)) ==> (a b c . d)
  (check-appends-equal? '(a b c d)   (append '(a b) '(c d))   (my-append '(a b) '(c d)) )
  (check-appends-equal? 'a           (append '() 'a)          (my-append '() 'a) )

  ; (printf ": ~a \n" )
  ; (check-equal?  "Error for: ")

) ;; end module+ test 
  ; (printf ": ~a \n"  )
  ; (check-equal?  "Error for: ")

