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
append flattens all

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

;;  17.5  Here's a procedure that takes two numbers as arguments and returns whichever number is larger:
(define (max2 a b)
  (if (> b a) 
      b 
      a))

#|
; application: not a procedure;
;  expected a procedure that can be applied to arguments
;   given: '(2 4)
;   arguments...: [none]
I had "rest-of-nums" in parens by mistake
|#

;; Use max2 to implement max, 
;; a procedure that takes one or more numeric arguments and returns the largest of them. 
;; tail recursion not working
;; If I get to the last element in the list, it sends it as a list to max2, not as a number.
;; I have been unable to figure it out.
;; ** A minute later ** -> caar seems to work
(define (my-max number . rest-of-nums)
  (printf "-- Calling my-max with number: ~a and rest-of-nums: ~a" number rest-of-nums)
  (printf " count of rest-of-nums is: ~a \n" (count rest-of-nums))
  (printf "(car rest-of-nums) is ~a \n" (car rest-of-nums))
  ; (printf "Here is (max2 number (car rest-of-nums)): ~a\n" (max2 number (car rest-of-nums)))
  (cond [(null? rest-of-nums) number]
        ; [(= (count rest-of-nums) 1) (apply max2 (append number (car rest-of-nums)) )]
        [(= (count rest-of-nums) 1) (begin
                                      (printf "count of rest-of-nums is 1\n")
                                      ; (apply max2 (append number (car rest-of-nums)) )
                                      (max2 number (caar rest-of-nums))
                                      )] ; okay
        ; [(= (count rest-of-nums) 1) (printf "(count rest-of-nums) is 1\n")]
        [(= (max2 number (car rest-of-nums)) number) 
         ; (= number (max2 number (car rest-of-nums))) 
         (begin (printf "(max2 ~a (car ~a)) is: ~a\n" number rest-of-nums (max2 number (car rest-of-nums)))
                (printf "here is (cdr rest-of-nums): ~a \n" (cdr rest-of-nums))
                ; (my-max number (cdr rest-of-nums))
                (my-max number (append '() (cdr rest-of-nums)))
                ; (apply my-max (list number (cdr rest-of-nums)))
           )]
        ; [(= (car rest-of-nums) (max2 number (car rest-of-nums))) (my-max (car rest-of-nums) (cdr rest-of-nums))]
        ; [(= (car rest-of-nums) (max2 number (car rest-of-nums))) (apply my-max rest-of-nums)]
        [(= (car rest-of-nums) (max2 number (car rest-of-nums))) ;
         (begin
           (printf "(car rest-of-nums) is the result of (max2 number (car rest-of-nums))\n" )
           (apply my-max rest-of-nums)
)] ;; okay
        [else number]
         
  )
)
; this seems to work
(define (reduce-max number . more-nums)
  (reduce max2 (append (list number) more-nums))
)

(define (simply-max number . more-nums)
  (cond [(null? more-nums) number]
)
)

(define (my-max2 number . more-nums)
  (printf "Hello: number is: ~a\n" number)
  (printf "Here is more-nums: ~a\n" more-nums)
  (cond [(null? more-nums) number]
)
)



(define (increasing? number . rest-of-numbers)
  (cond ((null? rest-of-numbers) #t)
	((> (car rest-of-numbers) number)
	 (apply increasing? rest-of-numbers))
	(else #f)))

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



  ; (printf ": ~a \n" )
  ; (check-equal?  "Error for: ")

) ;; end module+ test 
  ; (printf ": ~a \n"  )
  ; (check-equal?  "Error for: ")

