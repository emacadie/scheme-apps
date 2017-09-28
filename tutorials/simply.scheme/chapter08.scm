;; Chapter Eight
;; Higher-order functions
;; every is like map
;; "every transforms each element of a word or sentence individually. The result sentence usually contains as many elements as the argument"

;; keep is like filter
;; "keep selects certain elements of a word or sentence and discards the others. 
;; The elements of the result are elements of the argument, without transformation, 
;; but the result may be smaller than the original. 

;; accumulate is like reduce
;; "accumulate transforms the entire word or sentence into a single result by combining all of the elements in some way. ""

;; another way of thinking about it:
;; every      -> transform
;; keep       -> select
;; accumulate -> combine

(define (add-numbers sent)
  (accumulate + (keep number? sent)))
 
(add-numbers '(4 calling birds 3 french hens 2 turtle doves))

 
(add-numbers '(1 for the money 2 for the show 3 to get ready
		   and 4 to go))

(define (always-one arg)
  1)

(define (count sent)
  (accumulate + (every always-one sent)))

(define (real-word? wd)
  (not (member? wd '(a the an in of and for to with))))

(define (acronym phrase)
  (accumulate word (every first (keep real-word? phrase))))

;; ((repeated plural 4) 'computer)
;; ((repeated #function #number-of-times) args-to-function)

;; 8.1  What does Scheme return as the value of each of the following expressions? Figure it out for yourself before you try it on the computer.

(every last '(algebra purple spaghetti tomato gnu))
;; Guess: aeiou
;; actual: (a e i o u)
;; same thing, right?
(keep number? '(one two three four))
;; guess: empty list
;; actual: ()
(accumulate * '(6 7 13 0 9 42 17))
;; guess: 0
;; actual: 0
(member? 'h (keep vowel? '(t h r o a t)))
;; guess: false
;; actual: #f
(every square (keep even? '(87 4 7 12 0 5)))
;; guess: list with 16 and 144
;; actual: 16, 144, 0
;; I did not know 0 was even
;; I thought it was nothing
(accumulate word (keep vowel? (every first '(and i love her))))
;; guess: ai
;; actual: ai
((repeated square 0) 25)
;; guess: 0, or empty list
;; actual: 25 
(every (repeated bl 2) '(good day sunshine))
;; guess: good or go d sunshi
;; actual: (go d sunshi)

;; 8.2  Fill in the blanks in the following Scheme interactions:

;; (______ vowel? 'birthday)
;; IA
;; my guess: keep
(keep vowel? 'birthday)
;; chicken gave "ia" lower case

;; (______ first '(golden slumbers))
;; (G S)
;; my guess: every
(every first '(golden slumbers))
;; chicken gave lower case again (g s)

;; (______ '(golden slumbers))
GOLDEN

;; (______ ______ '(little child))
;; (E D)
;; my guess: every last

(every last '(little child))
;; chicken gives (e d)

;; (______ ______ (______ ______ '(little child)))
;; ED
;; my guess:
(every se (every last '(little child)))
;; that was wrong
;; this worked:
(accumulate word (every last '(little child)))

 
;; (______ + '(2 3 4 5))
;; (2 3 4 5)
;; my guess: keep
(keep + '(2 3 4 5))
;; chicken gave (2 3 4 5)
;; (______ + '(2 3 4 5))
;; 14
;; my guess: accumulate
(accumulate + '(2 3 4 5))
;; chicken gave 14

;;  8.3  Describe each of the following functions in English. Make sure to include a description of the domain and range of each function. Be as precise as possible; for example, "the argument must be a function of one numeric argument" is better than "the argument must be a function."

(define (f a)
  (keep even? a))
;; takes a list of numbers

(define (g b)
  (every b '(blue jay way)))
;; takes a function that acts on words or sentences
;; first last butfirst butlast

(define (h c d)
  (c (c d)))
;; c is a function that acts on a collection, d is a collection
;; like butfirst, butlast
(define (i e)
  (/ (accumulate + e) (count e)))
;; takes a list of numbers, divides the total by the count

accumulate
;; like reduce in clojure
;; takes a function and a collection, and it applies the function to each member of the collection
;; returns a single value as a result

sqrt
;; take a number and squares it, returning a number

repeated

(repeated sqrt 3)

(repeated even? 2)

(repeated first 2)

(repeated (repeated bf 3) 2)

