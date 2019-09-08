#lang simply-scheme

; Chapter 08: Higher-Order Functions
(require (prefix-in more: "more-simply.rkt"))
(require "simply-constants.rkt")
(butfirst '(This is chapter 8))

;; 8.4  Write a procedure choose-beatles that takes a predicate function as its argument and returns a sentence of just those Beatles (John, Paul, George, and Ringo) that satisfy the predicate. 
;; so this sounds like filter/keep
(define (choose-beatles func)
  (keep func (se 'john 'paul 'george 'ringo)))
;; because "sentence" makes it a list
;; For example:

;; 8.5  Write a procedure transform-beatles that takes a procedure as an argument, applies it to each of the Beatles, and returns the results in a sentence:
;; this sounds like map, aka every
(define (transform-beatles func)
  (every func (se 'john 'paul 'george 'ringo)))


;; 8.6  When you're talking to someone over a noisy radio connection, you sometimes have to spell out a word in order to get the other person to understand it. 
;; But names of letters aren't that easy to understand either, 
;; so there's a standard code in which each letter is represented by a particular word that starts with the letter. For example, instead of "B" you say "bravo."

;; Write a procedure words that takes a word as its argument and returns a sentence of the names of the letters in the word:

;; (You may make up your own names for the letters or look up the standard ones if you want.)

;; Hint: Start by writing a helper procedure that figures out the name for a single letter.
;; a map would come in handy here

;; to get the letters of a word:
;; (define (single letter) (word letter))
(define (nato-letter letter)
  (cond [(equal? letter 'a) 'alpha]
        [(equal? letter 'b) 'bravo]
        [(equal? letter 'c) 'charlie]
        [(equal? letter 'd) 'delta]
        ((equal? letter 'e) 'echo)        
        [else 'null]))
;; I could go to the end, but what's the point? 
(define (words the-word)
  (every nato-letter the-word))

;;  8.7  [14.5][9] Write a procedure letter-count that takes a sentence as its argument and returns the total number of letters in the sentence:
;; sounds like reduce, aka accumulate
(define (letter-count the-sentence) 
  (accumulate + (every count the-sentence)))

;; 8.8  [12.5] Write an exaggerate procedure which exaggerates sentences:

;; It should double all the numbers in the sentence, and it should replace "good" with "great," "bad" with "terrible," and anything else you can think of.
;; sounds like every with a dash of cond
;; only works with lower-case
;; do-great-stuff in more-simply
(define (exaggerate sntnc)
  (every more:do-great-stuff sntnc))

;; 8.9  What procedure can you use as the first argument to every so that for any sentence used as the second argument, every returns that sentence?
;; I tried "word" and it worked
;; What procedure can you use as the first argument to keep so that for any sentence used as the second argument, keep returns that sentence?
;; I tried "word?" and it worked
;; What procedure can you use as the first argument to accumulate so that for any sentence used as the second argument, accumulate returns that sentence?
;; I tried "sentence" and it worked

;; 8.10  Write a predicate true-for-all? that takes two arguments, a predicate procedure and a sentence. It should return #t if the predicate argument returns true for every word in the sentence.
(define (true-for-all? pred sntnc)
  (cond [(equal? (count sntnc) (count (keep pred sntnc)))]
        [else #f]))

;; 8.11  [12.6] Write a GPA procedure. It should take a sentence of grades as its argument and return the corresponding grade point average:
;; Hint: write a helper procedure base-grade that takes a grade as argument and returns 0, 1, 2, 3, or 4, 
;; and another helper procedure grade-modifier that returns −.33, 0, or .33, depending on whether the grade has a minus, a plus, or neither.
;; base-grade and modify-grade in more-simply

(define (convert-grade-to-num grade)
  (+ (more:base-grade grade) (more:modify-grade grade)))

(define (gpa grades)
  (/ (accumulate + (every convert-grade-to-num grades)) (count grades)))

;; 8.12  [11.2] When you teach a class, people will get distracted if you say "um" too many times. 
;; Write a count-ums that counts the number of times "um" appears in a sentence:
(define (is-um wd)
  (equal? 'um wd))

(define (count-ums sntnc)
  (count (keep is-um sntnc)))

;; 8.13  [11.3] Write a procedure phone-unspell that takes a spelled version of a phone number, such as POPCORN, 
;; and returns the real phone number, in this case 7672676. 
;; You will need to write a helper procedure that uses an 8-way cond expression to translate a single letter into a digit.

; phone-letter in more-simply, also used in chapter 11
(define (phone-unspell wd)
  (every more:phone-letter wd))

;; 8.14  Write the procedure subword that takes three arguments: a word, a starting position number, and an ending position number. 
;; It should return the subword containing only the letters between the specified positions:
(define (subword wd start fin)
  ((repeated butfirst (- start 1)) ((repeated butlast (- (count wd) fin)) wd)))


(module+ test
  (require rackunit)
  (check-true #t)

  ;; 8.04
  (define (ends-vowel? wd) (more:vowel? (last wd)))
  (printf "(choose-beatles ends-vowel?): ~a \n" (choose-beatles ends-vowel?))
  (check-equal? (choose-beatles ends-vowel?) '(george ringo) "Error for (choose-beatles ends-vowel?)")
  (define (even-count? wd) (even? (count wd)))
  (printf "(choose-beatles even-count?): ~a \n" (choose-beatles even-count?))
  (check-equal? (choose-beatles even-count?) '(john paul george) "Error for (choose-beatles even-count?)")

  ;; 8.05
  (define (amazify name)
    (word 'the-amazing- name))
  (printf "(transform-beatles amazify): ~a \n" (transform-beatles amazify))
  (check-equal? (transform-beatles amazify) '(the-amazing-john the-amazing-paul the-amazing-george the-amazing-ringo) "Error on (transform-beatles amazify)")
  (printf "(transform-beatles butfirst) ~a \n" (transform-beatles butfirst))
  (check-equal? (transform-beatles butfirst) '(ohn aul eorge ingo) "Error on (transform-beatles butfirst)")

  ;; 8.06
  (printf "(words 'cab): ~a \n" (words 'cab))
  (check-equal? (words 'cab) '(charlie alpha bravo) "Error for (words 'cab)")

  ;; 8.07
  (printf "(letter-count '(fixing a hole)): ~a \n" (letter-count '(fixing a hole)))
  (check-equal? (letter-count '(fixing a hole)) 11 "Error for (letter-count '(fixing a hole))")

  ;; 8.08
  (printf "(exaggerate '(i ate 3 potstickers)): ~a \n" (exaggerate '(i ate 3 potstickers)))
  (check-equal? (exaggerate '(i ate 3 potstickers)) '(i ate 6 potstickers) "Error for (exaggerate '(i ate 3 potstickers))")
  (printf "(exaggerate '(the chow fun is good here)): ~a \n" (exaggerate '(the chow fun is good here)))
  (check-equal? (exaggerate '(the chow fun is good here)) '(the chow fun is great here) "Error for (exaggerate '(the chow fun is good here))")
  (printf "(exaggerate '(but the egg drop soup is bad)): ~a \n" (exaggerate '(but the egg drop soup is bad)))
  (check-equal? (exaggerate '(but the egg drop soup is bad)) '(but the egg drop soup is terrible) "Error for (exaggerate '(but the egg drop soup is bad))")

  ;; 8.10
  (printf "(true-for-all? even? '(2 4 6 8)): ~a \n" (true-for-all? even? '(2 4 6 8)))
  (check-equal? (true-for-all? even? '(2 4 6 8)) #t "Error for (true-for-all? even? '(2 4 6 8))")
  (printf "(true-for-all? even? '(2 6 3 4)): ~a \n" (true-for-all? even? '(2 6 3 4)))
  (check-equal? (true-for-all? even? '(2 6 3 4)) #f "Error for (true-for-all? even? '(2 6 3 4))")

  ;; 8.11
  (printf "(gpa '(A A+ B+ B)): ~a \n" (gpa '(A A+ B+ B)))
  (check-equal? (gpa '(A A+ B+ B)) 3.665 "Error for (gpa '(A A+ B+ B))")

  ;; 8.12
  (printf "(count-ums '(today um we are going to um talk about functional um programming)): ~a \n" 
          (count-ums '(today um we are going to um talk about functional um programming)))
  (check-equal? (count-ums '(today um we are going to um talk about functional um programming))
                3 
                "Error for (count-ums '(today um we are going to um talk about functional um programming))")

  ;; 8.13
  (printf "(phone-unspell 'popcorn): ~a \n" (phone-unspell 'popcorn))
  (check-equal? (phone-unspell 'popcorn) '(7 6 7 2 6 7 6) "Error for (phone-unspell 'popcorn)")

  ;; 8.14
  (printf "(subword 'polythene 5 8): ~a \n" (subword 'polythene 5 8))
  (check-equal? (subword 'polythene 5 8) 'then "Error for (subword 'polythene 5 8)")
)

