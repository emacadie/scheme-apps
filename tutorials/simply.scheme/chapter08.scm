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

;; accumulate
;; like reduce in clojure
;; takes a function and a collection, and it applies the function to each member of the collection
;; returns a single value as a result

;; sqrt
;; take a number and squares it, returning a number

;; repeated
;; takes a function and a number, and then, in an outer parentheses, args to the first function. The first function mentioned is called the specified number of times
;; example:
((repeated plural 4) 'computer)
;; Like let or cond, you see two parentheses here, but for a different reason.
;; also, I don't think the following examples quite work as intended.
(repeated sqrt 3)

(repeated even? 2)

(repeated first 2)
;; These three just print the generic status string:
;; #<procedure (? x)>
;; Kind of what I thought
(repeated (repeated bf 3) 2)
;; I think this will print three lists, or a nested list
;; also prints ;; #<procedure (? x)>

;; 8.4  Write a procedure choose-beatles that takes a predicate function as its argument and returns a sentence of just those Beatles (John, Paul, George, and Ringo) that satisfy the predicate. 
;; so this sounds like filter/keep
;; For example:
(define (ends-vowel? wd) (vowel? (last wd)))

(define (even-count? wd) (even? (count wd)))

;; > 
(choose-beatles ends-vowel?)
;; (GEORGE RINGO)

;; > 
(choose-beatles even-count?)
;; (JOHN PAUL GEORGE)

(define (choose-beatles func)
  (keep func (se 'john 'paul 'george 'ringo)))
;; because "sentence" makes it a list

;; 8.5  Write a procedure transform-beatles that takes a procedure as an argument, applies it to each of the Beatles, and returns the results in a sentence:
;; this sounds like map, aka every

(define (transform-beatles func)
  (every func (se 'john 'paul 'george 'ringo)))

(define (amazify name)
  (word 'the-amazing- name))

(transform-beatles amazify)
;; (THE-AMAZING-JOHN THE-AMAZING-PAUL THE-AMAZING-GEORGE
;;                   THE-AMAZING-RINGO)

(transform-beatles butfirst)
;; (OHN AUL EORGE INGO)

;; 8.6  When you're talking to someone over a noisy radio connection, you sometimes have to spell out a word in order to get the other person to understand it. 
;; But names of letters aren't that easy to understand either, 
;; so there's a standard code in which each letter is represented by a particular word that starts with the letter. For example, instead of "B" you say "bravo."

;; Write a procedure words that takes a word as its argument and returns a sentence of the names of the letters in the word:

> (words 'cab)
;; (CHARLIE ALPHA BRAVO)

;; (You may make up your own names for the letters or look up the standard ones if you want.)

;; Hint: Start by writing a helper procedure that figures out the name for a single letter.
;; a map would come in handy here

;; to get the letters of a word:
;; (define (single letter) (word letter))
(define (nato-letter letter)
  (cond ((equal? letter 'a) 'alpha)
        ((equal? letter 'b) 'bravo)
        ((equal? letter 'c) 'charlie)
        ((equal? letter 'd) 'delta)
        ((equal? letter 'e) 'echo)        
        (else 'null)))
;; I could go to the end, but what's the point? 
(define (words the-word)
  (every nato-letter the-word))

;;  8.7  [14.5][9] Write a procedure letter-count that takes a sentence as its argument and returns the total number of letters in the sentence:

;; > 
(letter-count '(fixing a hole))
;; 11

;; sounds like reduce, aka accumulate
(define (letter-count the-sentence) 
  (accumulate + (every count the-sentence)))

;; 8.8  [12.5] Write an exaggerate procedure which exaggerates sentences:

(exaggerate '(i ate 3 potstickers))
;; (I ATE 6 POTSTICKERS)

(exaggerate '(the chow fun is good here))
;; (THE CHOW FUN IS GREAT HERE)
(exaggerate '(but the egg drop soup is bad))

;; It should double all the numbers in the sentence, and it should replace "good" with "great," "bad" with "terrible," and anything else you can think of.
;; sounds like every with a dash of cond

;; only works with lower-case
(define (do-great-stuff the-word)
  (cond ((equal? the-word 'good) 'great)
        ((equal? the-word 'bad) 'terrible)
        ((number? the-word) (* 2 the-word))
        (else the-word)))

(define (exaggerate sntnc)
  (every do-great-stuff sntnc))

;; 8.9  What procedure can you use as the first argument to every so that for any sentence used as the second argument, every returns that sentence?
;; I tried "word" and it worked
;; What procedure can you use as the first argument to keep so that for any sentence used as the second argument, keep returns that sentence?
;; I tried "word?" and it worked
;; What procedure can you use as the first argument to accumulate so that for any sentence used as the second argument, accumulate returns that sentence?
;; I tried "sentence" and it worked

;; 8.10  Write a predicate true-for-all? that takes two arguments, a predicate procedure and a sentence. It should return #t if the predicate argument returns true for every word in the sentence.

(true-for-all? even? '(2 4 6 8))
;; #T

(true-for-all? even? '(2 6 3 4))
;; #F
(define (true-for-all? pred func)
  )

