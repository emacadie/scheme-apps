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
  (cond [(or (null? listB) (empty? listB)) listA]
        [(or (null? listA) (empty? listA)) listB]
        [(and (not (list? listB)) (not (pair? listB))) (reverse (cons listB (reverse listA))) ]
        [else (my-append (reverse (cons (car listB) (reverse listA))) (cdr listB))])) ; line 129

;; again, not perfect, does not handle pairs too well, but I think I will take it
;; Isn't a pair just a list anyway?
;; According to https://stackoverflow.com/questions/6006671/are-pair-and-list-different-in-scheme
;; they are different
;; But the book said they are pretty much the same
(define (append-multi-lists listA . listB)
  (cond [(or (null? listB) (empty? listB)) listA]
        [(or (null? listA) (empty? listA)) (apply append-multi-lists listB)] ;; could this be done with car and cdr?
        
        [(and (= (count listB) 1 ) (list? (car listB)) (empty? (car listB))) listA]
        [else  (apply append-multi-lists (my-append listA (car listB)) (cdr listB))]))

;; This works with lists, but not if some of the items are solo numbers
(define (append-m-lists-reduce list-a . list-b)
  (my-append list-a (reduce my-append list-b))
  ; (reduce append (cons list-a list-b))
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
        [(= 0 (count (keep list? the-list))) #f]
        [else #t])) 

(define (list-or-item the-item)
  (cond [(not (list? the-item)) (list the-item)]
        [(list-with-sublists the-item) null]
        [else the-item]))

(define (sentence-via-append list-a list-b)
  (append (list-or-item list-a) (list-or-item list-b)))

(define (multi-sentence-append list-a . list-b)
  (cond [(null? list-b) (sentence-via-append list-a '())]
        [(null? list-a) (apply multi-sentence-append list-b)]
        ;; again getting empty list in a list '(())
        [(and (= 1 (count list-b)) (list? (car list-b)) (empty? (car list-b)) ) list-a]
        [else (multi-sentence-append (sentence-via-append list-a (car list-b)) (cdr list-b)) ])) ; line 206

; once again, winnin' with the 'duce!!
(define (multi-sentence-reduce list-a . list-b)
  (reduce sentence-via-append (cons list-a list-b)))

;;  17.8  Write member.
;; I don't think you can use higher-order functions, because I think you have to go through the entire collection with HOF
(define (new-member item-a list-a)
  (cond [(null? list-a) #f]
        [(equal? item-a (car list-a)) list-a]
        [else (new-member item-a (cdr list-a))]))

(define (simply-member item-a list-a)
  (cond [(null? list-a) #f]
        [(equal? item-a (first list-a)) list-a]
        [else (new-member item-a (butfirst list-a))]))

;; 17.9  Write list-ref. 
;; The list equivalent of item is called list-ref (short for "reference")
; it's different in that it counts items from zero instead of from one and takes its arguments in the other order
;; okay, I looked at the other solutions, and yes, decrementing is more efficient
(define (my-list-ref list-a num-a)
  ;(list-ref-helper list-a num-a 0)
  (cond [(null? list-a) #f] 
        [(equal? num-a 0) (car list-a)]
        [else (my-list-ref (cdr list-a) (- num-a 1))]))

(define (list-ref-helper list-a num-a counter)
  (cond [(null? list-a) #f] 
        [(equal? num-a counter) (car list-a)]
        [else (list-ref-helper (cdr list-a) num-a (+ 1 counter))]))

;; 17.10  Write length. 
(define (my-length list-a . counter)
  (cond [(null? counter) (my-length list-a 0)]
        [(null? list-a) (car counter)]
        [else (my-length-helper (cdr list-a) (+ 1 (car counter)))])
  ;(my-length-helper list-a 0)
)

(define (my-length-helper list-a counter)
  (cond [(null? list-a) counter]
        [else (my-length-helper (cdr list-a) (+ 1 counter))]))

;; From https://github.com/buntine/Simply-Scheme-Exercises/blob/master/17-lists/17-10.scm
(define (length3 lst)
  (reduce +
          (map (lambda (n) 1) lst)))
; Simply Scheme's reduce does not take a starting value
; Nevertheless, very clever, sir.

;; 17.11  Write before-in-list?, which takes a list and two elements of the list. 
;; It should return #t if the second argument appears in the list argument before the third argument:
; > (before-in-list? '(back in the ussr) 'in 'ussr) returns #T
; > (before-in-list? '(back in the ussr) 'the 'back) returns #F
;; let's compare the length of our members
(define (before-in-list? list-a item-a item-b)
  (cond [(or (null? item-a) (null? item-b)) #f]
        [(or (not (new-member item-a list-a)) (not (new-member item-b list-a))) #f]
        ; earlier item will have longer list from member
        [(> (length (new-member item-a list-a))
            (length (new-member item-b list-a))) #t]
        [else #f]))

;; 17.12  Write a procedure called flatten that takes as its argument a list, possibly including sublists, 
; but whose ultimate building blocks are words (not Booleans or procedures). 
; It should return a sentence containing all the words of the list, in the order in which they appear in the original:

; > (flatten '(((a b) c (d e)) (f g) ((((h))) (i j) k)))

; (A B C D E F G H I J K)




(module+ test
  (require rackunit)
  (check-true #t)
  (define (check-three-things-equal? result their-append-rsl my-append-rsl)
  (unless (and (check-equal? result their-append-rsl)
               (check-equal? result my-append-rsl))
    (fail-check)))
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

  (check-three-things-equal? '(get back the word)      (append '(get back) '(the word))      (my-append '(get back) '(the word)))
  (check-three-things-equal? '(i am the walrus)        (append '(i am) '(the walrus))        (my-append '(i am) '(the walrus)))
  (check-three-things-equal? '(Rod Argent Chris White) (append '(Rod Argent) '(Chris White)) (my-append '(Rod Argent) '(Chris White)))
  ;; from Husk R7RS docs
  (check-three-things-equal? '(x y)                    (append '(x) '(y))                    (my-append '(x) '(y)) )
  (check-three-things-equal? '(a b c d)                (append '(a) '(b c d))                (my-append '(a) '(b c d))  )
  (check-three-things-equal? '(a (b) (c))              (append '(a (b)) '((c)))              (my-append '(a (b)) '((c))) )
  ;; orig from Husk R7RS: (append '(a b) '(c . d)) ==> (a b c . d)
  (check-three-things-equal? '(a b c d)                (append '(a b) '(c d))                (my-append '(a b) '(c d)) )
  (check-three-things-equal? 'a                        (append '() 'a)                       (my-append '() 'a) )
  (check-three-things-equal? '(1 2 3 4 5 '(10 11 12))  (append '(1 2 3) '(4 5 '(10 11 12)))  (my-append '(1 2 3) '(4 5 '(10 11 12))) )

  (check-three-things-equal? '(1 2 3 4 5 6 7 8 9) (their-multi-append '(1 2 3) '(4 5 6) '(7 8 9) ) (append-multi-lists '(1 2 3) '(4 5 6) '(7 8 9) ))
  (check-equal? '(1 2 3 4 5 6 7 8 9) (their-multi-append '(1 2 3) '(4 5 6) '(7 8 9) ) (append-multi-lists '(1 2 3) '(4 5 6) '(7 8 9) ))
  ; (check-three-appends-equal? '(1 2 3 4 5 6 7 8 9) (their-multi-append '(1 2 3) 4 5 6 '(7 8 9)) (append-multi-lists '(1 2 3) 4 5 6 '(7 8 9)))
  (check-equal? '(1 2 3 4 5 6 7 8 9)  (append-multi-lists '(1 2 3) 4 5 6 '(7 8 9)))

  (check-equal? (sentence 'hello 'world)                    (sentence-via-append 'hello 'world))
  (check-equal? (sentence 'hello '(this is a ren))          (sentence-via-append 'hello '(this is a ren)))
  (check-equal? (sentence '(what is this) '(this is a ren)) (sentence-via-append '(what is this) '(this is a ren)))
  (check-equal? (sentence '(what is this) 're)              (sentence-via-append '(what is this) 're))
  (check-equal? (sentence '(what is this) (list 're))       (sentence-via-append '(what is this) (list 're)))
  (check-equal? (sentence 'hello)                           (sentence-via-append 'hello '(this is a '(deep list) bro)))
  (check-equal? (sentence 'hello)                           (sentence-via-append '(this is a '(deep list) bro) 'hello))
  (check-equal? (sentence 'hello)                           (sentence-via-append 'hello '()))
  (check-equal? (sentence 'hello)                           (sentence-via-append '() 'hello))
  (check-equal? (sentence 'hello 4)                         (sentence-via-append 'hello 4))
  (check-equal? (sentence 'hello 4)                         (sentence-via-append 'hello '(4)))
  (check-three-things-equal? '(hello there bug guy) (sentence 'hello 'there 'bug 'guy)  (multi-sentence-append 'hello 'there 'bug 'guy) )
  (check-three-things-equal? '(hello there bug guy) (sentence 'hello 'there 'bug 'guy)  (multi-sentence-reduce 'hello 'there 'bug 'guy) )
  (check-three-things-equal? '(hello there bug guy) (sentence 'hello 'there '(bug guy)) (multi-sentence-reduce 'hello 'there '(bug guy)))
  (check-three-things-equal? '(hello there bug guy) (sentence 'hello 'there '(bug guy)) (multi-sentence-reduce 'hello '(there bug) 'guy))
  (check-three-things-equal? '(hello there bug guy) (sentence 'hello '(there bug) 'guy) (multi-sentence-reduce 'hello 'there '(bug guy)))
  (check-three-things-equal? '(hello there bug guy) (multi-sentence-append '(hello there) 'bug 'guy) (multi-sentence-reduce '(hello there) 'bug 'guy) )
  ;; multi-sentence-reduce goes a bit beyond sentence, ignoring deep lists
  ;; Winnin' with the 'duce!
  (check-equal? '(hello world this is a list okay) (multi-sentence-reduce 'hello 'world '(this is a list) 'okay '(this is a '(deep) list)) )
  (check-equal? '(hello world this is a list okay this is fine) (multi-sentence-reduce 'hello 'world '(this is a list) 'okay '(this is a '(deep) list) '(this is fine)))
  ; new-member, simply-member
  (check-three-things-equal? '(d e f g) (member 'd '(a b c d e f g))  (new-member 'd '(a b c d e f g)))
  (check-three-things-equal? '(d e f g) (member 'd '(a b c d e f g))  (simply-member 'd '(a b c d e f g)))
  (check-three-things-equal? #f         (member 'h '(a b c d e f g))  (new-member 'h '(a b c d e f g)))
  (check-three-things-equal? #f         (member 'h '(a b c d e f g))  (simply-member 'h '(a b c d e f g)))
  ;; from Husk docs
  (check-three-things-equal? '((a) c)   (member (list 'a) '(b (a) c)) (new-member (list 'a) '(b (a) c)))
  (check-three-things-equal? '((a) c)   (member (list 'a) '(b (a) c)) (simply-member (list 'a) '(b (a) c)))
  (check-three-things-equal? 'best (list-ref '(Lisp is the best language) 3) (my-list-ref '(Lisp is the best language) 3))
  (check-three-things-equal? 'c    (list-ref '(a b c d) 2)                   (my-list-ref '(a b c d) 2) ) 
  (check-three-things-equal? 3 (length '(a b c)) (my-length '(a b c)))
  (check-three-things-equal? 3 (length '(a (b) (c d e)))  (my-length '(a (b) (c d e))) )
  (check-three-things-equal? 0 (length '())  (my-length '()) )
  (check-equal? #t (before-in-list? '(back in the ussr) 'in 'ussr))
  (check-equal? #f (before-in-list? '(back in the ussr) 'the 'back))
  (check-equal? #f (before-in-list? '(back in the ussr) 'in 'usa))

) ;; end module+ test 
  ; (printf ": ~a \n"  )
  ; (check-equal?  "Error for: ")

