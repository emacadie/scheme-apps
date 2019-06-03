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
  ;(printf "-- Calling my-max with number: ~a and rest-of-nums: ~a" number rest-of-nums)
  ;(printf " count of rest-of-nums is: ~a \n" (count rest-of-nums))
  ;(printf "(car rest-of-nums) is ~a \n" (car rest-of-nums))
  ; (printf "Here is (max2 number (car rest-of-nums)): ~a\n" (max2 number (car rest-of-nums)))
  (cond [(null? rest-of-nums) number]
        ; [(= (count rest-of-nums) 1) (apply max2 (append number (car rest-of-nums)) )]
        [(= (count rest-of-nums) 1) (begin
                                      ;(printf "count of rest-of-nums is 1\n")
                                      ; (apply max2 (append number (car rest-of-nums)) )
                                      (max2 number (caar rest-of-nums))
                                      )] ; okay
        ; [(= (count rest-of-nums) 1) (printf "(count rest-of-nums) is 1\n")]
        [(= (max2 number (car rest-of-nums)) number) 
         ; (= number (max2 number (car rest-of-nums))) 
         (begin 
           ; (printf "(max2 ~a (car ~a)) is: ~a\n" number rest-of-nums (max2 number (car rest-of-nums)))
                ; (printf "here is (cdr rest-of-nums): ~a \n" (cdr rest-of-nums))
                ; (my-max number (cdr rest-of-nums))
                (my-max number (append '() (cdr rest-of-nums)))
                ; (apply my-max (list number (cdr rest-of-nums)))
           )]
        ; [(= (car rest-of-nums) (max2 number (car rest-of-nums))) (my-max (car rest-of-nums) (cdr rest-of-nums))]
        ; [(= (car rest-of-nums) (max2 number (car rest-of-nums))) (apply my-max rest-of-nums)]
        [(= (car rest-of-nums) (max2 number (car rest-of-nums))) ;
         (begin
           ; (printf "(car rest-of-nums) is the result of (max2 number (car rest-of-nums))\n" )
           (apply my-max rest-of-nums)
)] ;; okay
        [else number]
         
  )
)
; this seems to work
(define (reduce-max number . more-nums)
  (reduce max2 (append (list number) more-nums)))

(define (simply-max number . more-nums)
  ;(printf "-- calling simply-max with number ~a and more-nums ~a\n" number more-nums)
  ;(printf "count of more-nums: ~a length of more-nums: ~a emptiness of more-nums: ~a\n" (count more-nums) (length more-nums) (empty? more-nums))
  ;(printf "first of more-nums: ~a \n" (first more-nums))
  (cond [(null? more-nums) 
         (begin
           (printf "more-nums is null\n")
                    number)]
        ;; after it got through the list of nums, Racket sent an empty list inside a list: '(())
        [(not (number? (first more-nums))) number]
        [(= number (max2 number (first more-nums))) 
         (begin
           ;(printf "number is result of (max2 number (first more-nums))\n")
           (simply-max number (butfirst more-nums))
           )
         ]
        [(= (first more-nums) (max2 number (first more-nums))) 
         (begin
           ;(printf "(first more-nums is result of (max2 number (first more-nums)))\n")
           (apply simply-max more-nums))]
        [else number]))

;; I admit, I looked at some of the other answers for this one.
;; Use max2 to create a list that apply sends to the function
(define (split-max number . more-nums)
  (printf "split-max: number is: ~a more-nums is ~a \n" number more-nums)
  (cond [(null? more-nums) number]
        [else (apply split-max (cons (max2 number (car more-nums)) (cdr more-nums)))] ))
  
;; line 150
(module+ test
  (require rackunit)
  (check-true #t)
  ;; they said the dot is not part of the actual list, so I guess this is okay
  



  ; (printf ": ~a \n" )
  ; (check-equal?  "Error for: ")

) ;; end module+ test 
  ; (printf ": ~a \n"  )
  ; (check-equal?  "Error for: ")

