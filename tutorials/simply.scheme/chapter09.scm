;; Chapter Nine

;; two functions that do the same thing
;; one with lambda, one without
(define (add-two-small x y) (+ x y) )
(define add-two-big (lambda (x y) (+ x y)))

;;  9.1  What will Scheme print? Figure it out yourself before you try it on the computer.
(lambda (x) (+ (* x 3) 4))
;; prints nothing; this is the definition
((lambda (x) (+ (* x 3) 4)) 10)
;; 34
(every (lambda (wd) (word (last wd) (bl wd)))
         '(any time at all))
;; it puts the last letter at the end
;; I thought it just took the second letter or something
;; you could do it like this:
(define (last-to-first the-line)
  (every (lambda (wd) (word (last wd) (bl wd))) the-line))
 ((lambda (x) (+ x 3)) 10 15)
;; I think it ignores the 15
;; I got an error 
;; I knew that

;;  9.2  Rewrite the following definitions so as to make the implicit lambda explicit.
;; SRFI 1 has a "second", so I will call this one "second-ss" for "Simply Scheme"
(define (second-ss stuff)
  (first (bf stuff)))

(define (make-adder num)
  (lambda (x) (+ num x)))

;;  9.3  What does this procedure do?
(define (let-it-be sent)
  (accumulate (lambda (x y) y) sent))
;; it returns the last thing it was sent
;; or last part of collection
;; I would not have guessed that

;; 9.4  The following program doesn't work. Why not? Fix it.
(define (who sent)
  (every describe '(pete roger john keith)))

(define (describe person)
  (se person sent))
;; "sent" is never sent to describe

;; It's supposed to work like this:

(who '(sells out))
(pete sells out roger sells out john sells out keith sells out)

;; here you go:
(define (who sent)
  (every (lambda (x) (se x sent)) '(pete roger john keith)))

;; In each of the following exercises, write the procedure in terms of lambda and higher-order functions. 
;; Do not use named helper procedures. If you've read Part IV, don't use recursion, either.
;; 9.5  Write prepend-every:

;; > (prepend-every 's '(he aid he aid))
;; (SHE SAID SHE SAID)

;; > (prepend-every 'anti '(dote pasto gone body))
;; (ANTIDOTE ANTIPASTO ANTIGONE ANTIBODY)
(define (prepend-every letter the-sentence)
  (every (lambda (wd) (word letter wd)) the-sentence))



;; 9.6  Write a procedure sentence-version that takes a function f as its argument and returns a function g. 
;; f should take a single word as argument. g should take a sentence as argument and return the sentence formed by applying f to each word of that argument.
;; This is just a convoluted "every", right? Can I use "every"?
;; > ((sentence-version first) '(if i fell))
;; (I I F)

;; > ((sentence-version square) '(8 2 4 6))
;; (64 4 16 36)

;; this is like
; (define (make-adder num)
;  (lambda (x) (+ x num)))

;> ((make-adder 4) 7)
;11
;> (every (make-adder 6) '(2 4 8))
;(8 10 14)
;and
;(define (same-arg-twice fn)
;  (lambda (arg) (fn arg arg)))

;> ((same-arg-twice word) 'hello)
;HELLOHELLO

;> ((same-arg-twice *) 4)
;16
;so (same-arg-twice word) becomes
;(lambda (arg) (word arg arg))
;(define (flip fn)
;  (lambda (a b) (fn b a)))
;> ((flip -) 5 8)
;3
;> ((flip se) 'goodbye 'hello)
;(HELLO GOODBYE)
;It's a bit odd calling a function with two parens, but this is Scheme and not Clojure, so I guess we're good to go.

(define (sentence-version fn)
  (lambda (g) (every fn g)))
;; it works, but is it the best way?


; 9.7  Write a procedure called letterwords that takes as its arguments a letter and a sentence. 
; It returns a sentence containing only those words from the argument sentence that contain the argument letter:

; > (letterwords 'o '(got to get you into my life))
; (GOT TO YOU INTO)
; This sounds like keep

 (define (letterwords letter the-sntnc)
  (keep (lambda (x) (member? letter x)) the-sntnc))

; 9.8  Suppose we're writing a program to play hangman. 
; In this game one player has to guess a secret word chosen by the other player, one letter at a time. 
; You're going to write just one small part of this program: 
; a procedure that takes as arguments the secret word and the letters guessed so far, 
; returning the word in which the guessing progress is displayed by including all the guessed letters along with underscores for the not-yet-guessed ones:

; > (hang 'potsticker 'etaoi)
; _OT_TI__E_
 
; Hint: You'll find it helpful to use the following procedure that determines how to display a single letter: 
(define (hang-letter letter guesses)
  (if (member? letter guesses)
      letter
      '_))

;; this sounds like we will need "every"
(define (hang the-word the-guesses)
  (lambda ()))


