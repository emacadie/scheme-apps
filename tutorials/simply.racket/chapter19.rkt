#lang simply-scheme

; Chapter 19 Higher Order Functions

(require "more-simply.rkt")
(require (prefix-in ch17: "chapter17.rkt"))


(butfirst '(This is chapter 19 Higher Order Functions))

;; Chapter 19 Higher Order Functions

;; from chapter 14, tail-recursive
;; (every-something-r (lambda (x) (* 2 x)) '(1 2 3) '())
(define (my-every func sent)
  (every-something-r func sent '()))
(define (every-something-r func sent outp)
  (cond [(null? sent) outp]
        [(empty? outp) (every-something-r func 
                                          (cdr sent) 
                                          (list (func (car sent))))]
        [else (every-something-r func 
                                 (cdr sent) 
                                 (ch17:my-append outp 
                                                 (func (car sent))))]))
; from chapter 14 - tail recursive
(define (my-keep predicate sent)
  (keep-r predicate sent '()))
(define (keep-r predicate sent outp)
  (cond [(empty? sent) outp] 
        [(and (predicate (car sent)) (empty? outp)) (keep-r predicate 
                                                            (cdr sent)
                                                            (list (car sent)))]
	    [(predicate (car sent)) (keep-r predicate 
                                        (cdr sent) 
                                        (ch17:my-append outp 
                                                        (car sent)))]
	    [else (keep-r predicate 
                      (cdr sent) 
                      outp)]))

; from chapter 14, tail-recursive
(define (my-accumulate func sent start)
  (cond [(empty? sent) start]
        [else (my-accumulate func 
                             (cdr sent) 
                             (func start (car sent)))]))

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


(module+ test
  (require rackunit)
  (check-true #t)
  (define (check-three-things-equal? result their-append-rsl my-append-rsl)
  (unless (and (check-equal? result their-append-rsl)
               (check-equal? result my-append-rsl))
    (fail-check)))
  (check-equal? (my-every (lambda (x) (* 2 x)) '(1 2 3) ) '(2 4 6))
  (check-equal? (my-keep even? '(1 2 3 4 5 6)) '(2 4 6))
  (check-equal? (my-accumulate max '(23 54 45 85 65) 0) 85)

) ;; end module+ test 
  ; (printf ": ~a \n"  )
  ; (check-equal?  "Error for: ")

