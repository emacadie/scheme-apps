#lang simply-scheme

; Chapter 09: Lambda

(require (prefix-in more: "more-simply.rkt"))
(butfirst '(This is chapter 9))

;; two functions that do the same thing
;; one with lambda, one without
; (define (add-two-small x y) (+ x y) )
; (define add-two-big (lambda (x y) (+ x y)))

;; 9.4  The following program doesn't work. Why not? Fix it.
;; here you go:
(define (who sent)
  (every (lambda (x) (se x sent)) '(pete roger john keith)))

;; In each of the following exercises, write the procedure in terms of lambda and higher-order functions. 
;; Do not use named helper procedures. If you've read Part IV, don't use recursion, either.
;; 9.5  Write prepend-every:
(define (prepend-every letter the-sentence)
  (every (lambda (wd) (word letter wd)) the-sentence))

;; 9.6  Write a procedure sentence-version that takes a function f as its argument and returns a function g. 
;; f should take a single word as argument. g should take a sentence as argument and return the sentence formed by applying f to each word of that argument.
;; This is just a convoluted "every", right? Can I use "every"?
(define (sentence-version fn)
  (lambda (g) (every fn g)))
;; it works, but is it the best way?

; 9.7  Write a procedure called letterwords that takes as its arguments a letter and a sentence. 
; It returns a sentence containing only those words from the argument sentence that contain the argument letter:
 (define (letterwords letter the-sntnc)
  (keep (lambda (x) (member? letter x)) the-sntnc))

; 9.8  Suppose we're writing a program to play hangman. 
; In this game one player has to guess a secret word chosen by the other player, one letter at a time. 
; You're going to write just one small part of this program: 
; a procedure that takes as arguments the secret word and the letters guessed so far, 
; returning the word in which the guessing progress is displayed by including all the guessed letters along with underscores for the not-yet-guessed ones:
; Hint: You'll find it helpful to use the following procedure that determines how to display a single letter: 
(define (hang-letter letter guesses)
  (if (member? letter guesses)
      letter
      '_))

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
(define (first-last first-sen)
  (keep (lambda (x) (equal? (first x) (last x))) first-sen))

;;  9.13  Write a procedure compose that takes two functions f and g as arguments. 
;; It should return a new function, the composition of its input functions, which computes f(g(x)) when passed the argument x.
(define (compose first-func second-func )
  (lambda (the-arg) (first-func (second-func the-arg))))
;; maybe I am getting the hang of this

;; 9.14  Write a procedure substitute that takes three arguments, two words and a sentence. 
;; It should return a version of the sentence, but with every instance of the second word replaced with the first word:
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
(define (aplize the-func)
  (lambda (the-arg)
    (if (sentence? the-arg)
        (every the-func the-arg)
        (the-func the-arg))))

;; 9.17  Write keep in terms of every and accumulate. 
#|
(define (my-keep the-pred the-collection)
  (every (lambda (x)
           (if (the-pred x)
               x
               'false)) the-collection))
|#
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

(module+ test
  (require rackunit)
  (check-true #t)
  (define (ends-vowel? wd) (more:vowel? (last wd)))
  (printf "(who '(sells out)): ~a \n" (who '(sells out)))
  (check-equal? (who '(sells out)) '(pete sells out roger sells out john sells out keith sells out) "Error for (who '(sells out))")

  ; 9.05
  (printf "(prepend-every 's '(he aid he aid)): ~a \n" (prepend-every 's '(he aid he aid)))
  (check-equal? (prepend-every 's '(he aid he aid)) '(she said she said) "Error for (prepend-every 's '(he aid he aid))")
  (printf "(prepend-every 'anti '(dote pasto gone body)) ~a \n" (prepend-every 'anti '(dote pasto gone body)))
  (check-equal? (prepend-every 'anti '(dote pasto gone body)) '(antidote antipasto antigone antibody) "Error for (prepend-every 'anti '(dote pasto gone body))")

  ; 9.06
  (printf "((sentence-version first) '(if i fell)): ~a \n" ((sentence-version first) '(if i fell)))
  (check-equal? ((sentence-version first) '(if i fell)) '(i i f) "Error for ((sentence-version first) '(if i fell))")
  (printf "((sentence-version more:square) '(8 2 4 6)): ~a \n" ((sentence-version more:square) '(8 2 4 6)))
  (check-equal? ((sentence-version more:square) '(8 2 4 6)) '(64 4 16 36) "Error for ((sentence-version square) '(8 2 4 6))")

  ; 9.07
  (printf "(letterwords 'o '(got to get you into my life)): ~a \n" (letterwords 'o '(got to get you into my life)))
  (check-equal? (letterwords 'o '(got to get you into my life)) '(got to you into) "Error for (letterwords 'o '(got to get you into my life))")

  ;; 9.08
  (printf "(hang 'potsticker 'etaoi): ~a \n" (hang 'potsticker 'etaoi))
  (check-equal? (hang 'potsticker 'etaoi) '(_ o t _ t i _ _ e _) "Error for (hang 'potsticker 'etaoi)")

  ; 9.09
  (printf "(common-words '(this is good thing) '(what good can this little thing do )): ~a \n" (common-words '(this is good thing) '(what good can this little thing do )))
  (check-equal? (common-words '(this is good thing) '(what good can this little thing do )) '(this good thing) "Error for (common-words '(this is good thing) '(what good can this little thing do ))")

  ; 9.10
  (printf "(my-appearances 'e 'feelings): ~a \n" (my-appearances 'e 'feelings))
  (check-equal? (my-appearances 'e 'feelings) 2 "Error for (my-appearances 'e 'feelings)")
  (printf "(my-appearances 'hello '(hello goodbye hello again goodbye again nothing again)): ~a \n" (my-appearances 'hello '(hello goodbye hello again goodbye again nothing again)))
  (check-equal? (my-appearances 'hello '(hello goodbye hello again goodbye again nothing again)) 2 "Error for (my-appearances 'hello '(hello goodbye hello again goodbye again nothing again))")

  ; 9.11
  (printf "(unabbrev '(john 1 wayne fred 4) '(bill hank kermit joey)): ~a \n" (unabbrev '(john 1 wayne fred 4) '(bill hank kermit joey)))
  (check-equal? (unabbrev '(john 1 wayne fred 4) '(bill hank kermit joey)) '(john bill wayne fred joey) "Error for (unabbrev '(john 1 wayne fred 4) '(bill hank kermit joey))")
  (printf "(unabbrev '(i 3 4 tell 2) '(do you want to know a secret?)): ~a \n" (unabbrev '(i 3 4 tell 2) '(do you want to know a secret?)))
  (check-equal? (unabbrev '(i 3 4 tell 2) '(do you want to know a secret?)) '(i want to tell you) "Error for (unabbrev '(i 3 4 tell 2) '(do you want to know a secret?))")

  ; 9.12
  (printf "(first-last '(california ohio nebraska alabama alaska massachusetts)): ~a \n" (first-last '(california ohio nebraska alabama alaska massachusetts)))
  (check-equal? (first-last '(california ohio nebraska alabama alaska massachusetts)) '(ohio alabama alaska) "Error for (first-last '(california ohio nebraska alabama alaska massachusetts))")

  ; 9.13
  (printf "compose: ((compose sqrt abs) -25): ~a \n" ((compose sqrt abs) -25))
  (check-equal? ((compose sqrt abs) -25) 5 "Error for ((compose sqrt abs) -25)")
  (define secondf (compose first bf))
  (printf "compose: (secondf '(higher order function)): ~a \n" (secondf '(higher order function)))
  (check-equal? (secondf '(higher order function)) 'order "Error for (secondf '(higher order function))")

  ; 9.14
  (printf "(substitute 'maybe 'yeah '(she loves you yeah yeah yeah)): ~a \n" (substitute 'maybe 'yeah '(she loves you yeah yeah yeah)))
  (check-equal? (substitute 'maybe 'yeah '(she loves you yeah yeah yeah)) '(she loves you maybe maybe maybe) "Error for (substitute 'maybe 'yeah '(she loves you yeah yeah yeah))")
  
  ; 9.15
  (define safe-sqrt (type-check sqrt number?))
  (printf "type-check: (safe-sqrt 16): ~a \n" (safe-sqrt 16))
  (check-equal? (safe-sqrt 16) 4 "Error for (safe-sqrt 16)")
  (printf "type-check: (safe-sqrt 'sarsaparilla): ~a \n" (safe-sqrt 'sarsaparilla))
  (check-equal? (safe-sqrt 'sarsaparilla) #f "Error for (safe-sqrt 'sarsaparilla)")

  ; 9.16
  (define apl-sqrt (aplize sqrt))
  (printf "aplize: (apl-sqrt 36): ~a \n" (apl-sqrt 36))
  (check-equal? (apl-sqrt 36) 6 "Error for (apl-sqrt 36)")
  (printf "aplize: (apl-sqrt '(1 100 25 16)): ~a \n" (apl-sqrt '(1 100 25 16)))
  (check-equal? (apl-sqrt '(1 100 25 16)) '(1 10 5 4) "Error for (apl-sqrt '(1 100 25 16))")

  ; 9.17
  (printf "(my-keep even? '(1 2 3 4 5 6 7)): ~a \n" (my-keep even? '(1 2 3 4 5 6 7)))
  (check-equal? (my-keep even? '(1 2 3 4 5 6 7)) '(2 4 6) "Error for (my-keep even? '(1 2 3 4 5 6 7))") 

)


