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
;; from chapter 8:
;; (define (double letter) (word letter letter))
;; > (every double 'girl)
;; (GG II RR LL)
(define (hang the-word the-guesses)
  (every (lambda (x) (hang-letter x the-guesses) ) the-word))
; okay, so it returns a sentence with spaces, but I will take it

;; 9.9  Write a procedure common-words that takes two sentences as arguments 
;; and returns a sentence containing only those words that appear both in the first sentence and in the second sentence.
;; keep and every? How heavy!!
;; from chapter 8:
;; For instance, the keep function takes a predicate and a sentence as arguments. 
;; It returns a sentence containing only the words of the argument sentence for which the predicate is true.
;; every takes a function, and a collection (or sentence)

(define (common-words first-sen second-sen)
  (keep (lambda (x) (member? x second-sen)) first-sen))

(common-words '(this is good thing) '(what good can this little thing do ))
;; so no need for every

;; 9.10  In Chapter 2 we used a function called appearances that returns the number of times its first argument appears as a member of its second argument. 
;; Implement appearances.
;; this would be accumulate?
;; no this is keep again
(define (my-appearances first-arg second-arg)
  (count (keep (lambda (x) (equal? x first-arg)) second-arg)))

;; I keep wanting to accumulate every time!
;; What can I do with bad Scheme jokes? Perhaps reduce the number?
;; car car car, you're so funny, you're such a cadr

;; 9.11  Write a procedure unabbrev that takes two sentences as arguments. 
;; It should return a sentence that's the same as the first sentence, 
;; except that any numbers in the original sentence should be replaced with words from the second sentence. 
;; A number 2 in the first sentence should be replaced with the second word of the second sentence, a 6 with the sixth word, and so on.

;; > (unabbrev '(john 1 wayne fred 4) '(bill hank kermit joey))
;; (JOHN BILL WAYNE FRED JOEY)

;; > (unabbrev '(i 3 4 tell 2) '(do you want to know a secret?))
;; (I WANT TO TELL YOU)

;; every something-with-second-sen first-sen
;; use item somehow: (item 4 '(this is a sentence))
;; he said not to use helper functions, but in 9.8 he did, so I will too
;; or we will have a nasty lambda
(define (unabbrev first-sen second-sen)
  (every (lambda (x) (if (number? x)
                         (item x second-sen)
                         x) ) first-sen)) 

;;  9.12  Write a procedure first-last whose argument will be a sentence. 
;; It should return a sentence containing only those words in the argument sentence whose first and last letters are the same:
; > (first-last '(california ohio nebraska alabama alaska massachusetts))
; (OHIO ALABAMA ALASKA)
(define (first-last first-sen)
  (keep (lambda (x) (equal? (first x) (last x))) first-sen))

;;  9.13  Write a procedure compose that takes two functions f and g as arguments. 
;; It should return a new function, the composition of its input functions, which computes f(g(x)) when passed the argument x.
;> ((compose sqrt abs) -25)
; 5
; > (define secondf (compose first bf))
; > (secondf '(higher order function))
; ORDER

(define (compose first-func second-func )
  (lambda (the-arg) (first-func (second-func the-arg))))
;; maybe I am getting the hang of this

;; 9.14  Write a procedure substitute that takes three arguments, two words and a sentence. 
;; It should return a version of the sentence, but with every instance of the second word replaced with the first word:

;> (substitute 'maybe 'yeah '(she loves you yeah yeah yeah))
; (SHE LOVES YOU MAYBE MAYBE MAYBE)
(define (substitute first-word second-word the-sent)
  (every (lambda (x)
           (if (equal? x second-word)
               first-word
               x)) the-sent))

;; 9.15 Many functions are applicable only to arguments in a certain domain and result in error messages if given arguments outside that domain. 
;; For example, sqrt may require a nonnegative argument in a version of Scheme that doesn't include complex numbers. 
;; (In any version of Scheme, sqrt will complain if its argument isn't a number at all!) 
;; Once a program gets an error message, it's impossible for that program to continue the computation.

; Write a procedure type-check that takes as arguments a one-argument procedure f and a one-argument predicate procedure pred. 
;; Type-check should return a one-argument procedure that first applies pred to its argument 
; if that result is true, the procedure should return the value computed by applying f to the argument; 
; if pred returns false, the new procedure should also return #f:
; > (define safe-sqrt (type-check sqrt number?))
; > (safe-sqrt 16)
; 4
; > (safe-sqrt 'sarsaparilla)
; #F

(define (type-check the-func the-pred)
  (lambda (the-arg)
    (if (the-pred the-arg)
        (the-func the-arg)
        #f)))

;;  9.16  In the language APL, most arithmetic functions can be applied either to a number, 
; with the usual result, or to a vector—the APL name for a sentence of numbers—in which case 
; the result is a new vector in which each element is the result of applying the function to the corresponding element of the argument. 
; For example, the function sqrt applied to 16 returns 4 as in Scheme, 
; but sqrt can also be applied to a sentence such as (16 49) and it returns (4 7).

; Write a procedure aplize that takes as its argument a one-argument procedure whose domain is numbers or words. 
; It should return an APLized procedure that also accepts sentences:

; > (define apl-sqrt (aplize sqrt))
; > (apl-sqrt 36)
; 6
; > (apl-sqrt '(1 100 25 16))
; (1 10 5 4)

(define (aplize the-func)
  (lambda (the-arg)
    (if (sentence? the-arg)
        (every the-func the-arg)
        (the-func the-arg))))

;; 9.17  Write keep in terms of every and accumulate. 
(define (my-keep the-pred the-collection)
  (every (lambda (x)
           (if (the-pred x)
               x
               'false)) the-collection))
;; I admit, I looked at another repo; I was kind of going towards this anyway
;; I thought I would have to make a separate function; it's not as clunky as I thought
(define (my-keep the-pred the-collection)
  (accumulate se (every (lambda (x)
              (if (the-pred x)
                  x
                  '())) the-collection)) )

(define (my-keep-02 the-pred the-collection)
  (every (lambda (x)
           (if (the-pred x)
               x
               '())) the-collection))
;; so you do not need the "accumulate"
;; All I needed was the empty element at the end.

