;; Simply Scheme Chapter 5
;; Must load simply.scm for this chapter

;; 5.1  What values are printed when you type these expressions to Scheme? (Figure it out in your head before you try it on the computer.)
(sentence 'I '(me mine))
;; My guess: (I me mine)
;; correct
(sentence '() '(is empty))
;; my guess: (() is empty)
;; incorrect: (is empty)
(word '23 '45)
;; my guess: 2345
;; correct
(se '23 '45)
;; my guess: (23 45)
;; correct
(bf 'a)
; my guess: ()
;; incorrect: ""
(bf '(aye))
;; my guess: ye
;; incorrect: ()
(count (first '(maggie mae)))
;; my guess: 6 (or 0?)
;; 6 it is
(se "" '() "" '())
;; my guess: "'() "" '()
;; incorrect: ("" "")
(count (se "" '() "" '()))
;; my guess: 2
;; correct

;; 5.2 For each of the following examples, write a procedure of two arguments that, when applied to the sample arguments, 
;; returns the sample result. Your procedures may not include any quoted data.

;; > (f1 '(a b c) '(d e f))
;; (B C D E)
(define (f1 first-arg second-arg)
  (se (bf first-arg) (bf second-arg)))
;; I am not going to worry about case right now

;; > (f2 '(a b c) '(d e f))
;; (B C D E AF)
(define (f2 first-arg second-arg)
  (se (bf first-arg) (bl second-arg) (word (first first-arg) (last second-arg))))

;; > (f3 '(a b c) '(d e f))
;; (A B C A B C)
(define (f3 first-arg second-arg)
  (se first-arg first-arg))

;; Scheme already has a "second" procedure
(define (simply-second thing)
  (first (butfirst thing)))
(define (f4 first-arg second-arg)
  (word (simply-second first-arg) (simply-second second-arg)))
;; > (f4 '(a b c) '(d e f))
;; BE

;; 5.3 Explain the difference in meaning between (first 'mezzanine) and (first '(mezzanine)).
;; (first 'mezzanine) will give the first letter of that word
;; (first '(mezzanine)) gives the whole word, since it turns the word into a sentence

;; 5.4  Explain the difference between the two expressions (first (square 7)) and (first '(square 7)).
(first (square 7))
;; will give first digit/character of the result of calling (square 7)
(first '(square 7))
;; will give the word "square" since the quote makes it treat "(square 7)" as a sentence

;; 5.5 Explain the difference between (word 'a 'b 'c) and (se 'a 'b 'c). 
(word 'a 'b 'c)
;; should give abc
(se 'a 'b 'c)
;; should give (a b c)

;; 5.6 Explain the difference between (bf 'zabadak) and (butfirst 'zabadak).
;; I think they do the same thing
(bf 'zabadak)
(butfirst 'zabadak)
;; abadak

;; 5.7 Explain the difference between (bf 'x) and (butfirst '(x)).
(bf 'x) 
;; gives ""
(butfirst '(x))
;; give ()

;;  5.8 Which of the following are legal Scheme sentences?

;; (here, there and everywhere) ;; no; has a comma
;;(help!) ;; yes
;; (all i've got to do) ;; no; has a quote in it
;; (you know my name (look up the number)) ;; no; has another sentence inside it

;; 5.9 Figure out what values each of the following will return before you try them on the computer:

(se (word (bl (bl (first '(make a))))
          (bf (bf (last '(baseball mitt)))))
    (word (first 'with) (bl (bl (bl (bl 'rigidly))))
          (first 'held) (first (bf 'stitches))))
;; guess: Matt Wright; actually, it gives (matt wright)

(se (word (bl (bl 'bring)) 'a (last 'clean))
    (word (bl (last '(baseball hat))) (last 'for) (bl (bl 'very))
            (last (first '(sunny days)))))
;; guess: (brian harvey)

;; 5.10 What kinds of argument can you give butfirst so that it returns a word? A sentence?
;; who cares? I am getting tired of all this "word" and "sentence" garbage. Yeah, it's great to "get the computer to think like you do",
;; but if those concepts are so great, why aren't words and sentences part of standard scheme?
;; bf will return a word for a two-word sentence, and I was not able to get it to return a sentence.
;; Alright, (bf '(you know my name (look up the number))) will return
;; (know my name (look up the number)), so maybe it's possible
;; I didn't think you could have a sentence with a sentence in it. Hemingway would not approve.

;; 5.11 What kinds of argument can you give last so that it returns a word? A sentence? 
;; I am going to go with the same answer as 5.10

;; 5.12 Which of the functions first, last, butfirst, and butlast can return an empty word? For what arguments? What about returning an empty sentence? 
;; I think I am going to skip this

;; 5.13  What does ' 'banana stand for?
;; a quote, a space, a quote and the word banana
;; What is (first ' 'banana) and why?
;; it is supposed to be (first ''banana), no space betweent the quotes
;; that would return a quote

;; 5.14 Write a procedure third that selects the third letter of a word (or the third word of a sentence).
(define (third x)
  (first (bf (bf x))))
;; or use item

;;  5.15   Write a procedure first-two that takes a word as its argument, returning a two-letter word containing the first two letters of the argument.
;; > (first-two 'ambulatory)
;; AM
(define (first-two arg-word)
  (word (first arg-word) (first (bf arg-word))))

;; alright, it finally hit me: word is symbol, sentence is list. I got it.

;; 5.16 Write a procedure two-first that takes two words as arguments, returning a two-letter word containing the first letters of the two arguments.
;; > (two-first 'brian 'epstein)
;; BE
;; Now write a procedure two-first-sent that takes a two-word sentence as argument, returning a two-letter word containing the first letters of the two words.
;; > (two-first-sent '(brian epstein))
;; BE
(define (two-first first-arg second-arg)
  (word (first first-arg) (first second-arg)))

(define (two-first-sent first-arg second-arg)
  (word (first first-arg) (first second-arg)))

;; 5.17 Write a procedure knight that takes a person's name as its argument and returns the name with "Sir" in front of it.
;; > (knight '(david wessel))
;; (SIR DAVID WESSEL)
(define (knight arg-name)
  (se 'Sir arg-name))

;; 5.18 Try the following and explain the result:
;; using "word" causes an error, see the pitfall
(define (ends word-arg)
  (word (first word-arg) (last word-arg)))
;; got to make it "word-arg"
(ends 'john)

;; 5.19 Write a procedure insert-and that takes a sentence of items and returns a new sentence with an "and" in the right place:
;; > (insert-and '(john bill wayne fred joey))
;; (JOHN BILL WAYNE FRED AND JOEY)
(define (insert-and arg-sentence)
  (se (butlast arg-sentence) 'and (last arg-sentence)))

;; 5.20 Define a procedure to find somebody's middle names:
(define (middle-names name-arg)
  (se (butfirst (butlast name-arg))))
;; > (middle-names '(james paul mccartney))
;; (PAUL)
;; > (middle-names '(john ronald raoul tolkien))
;; (RONALD RAOUL)
;; > (middle-names '(bugs bunny))
;; ()
;; > (middle-names '(peter blair denis bernard noone))
;; (BLAIR DENIS BERNARD)

;; 5.21  Write a procedure query that turns a statement into a question by swapping the first two words and adding a question mark to the last word:
(define (query arg)
  (sentence (second arg) (first arg) (butfirst (butfirst arg))'(?)))
;; I can't do it without a space in between last word and question mark

;; > (query '(you are experienced))
;; (ARE YOU EXPERIENCED?)

;; > (query '(i should have known better))
;; (SHOULD I HAVE KNOWN BETTER?)


