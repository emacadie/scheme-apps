#lang simply-scheme

; Chapter 12: Recursion again: The leap of faith

(require (prefix-in more: "more-simply.rkt"))
(require "simply-constants.rkt")
(butfirst '(This is chapter 12 recursion))

;; The Leap of Faith
;; https://old.reddit.com/r/compsci/comments/44syr6/understanding_the_recursive_leap_of_faith/
;; Maybe I am not very good at this yet, but the "Leap Of Faith" sounds like it can become a Death March

;; 12.2  Fix the bug in the following definition:
(define (acronym sent)                       ;; wrong
  (if (= (count sent) 1)
      (first sent)
      (word (first (first sent))
	    (acronym (bf sent)))))

(define (my-acronym sent)
  (if (= (count sent) 1)
      (first (first sent))
      (word (first (first sent)) (my-acronym (bf sent)))))

;;  12.4  Here's the definition of a function f:
;; math display
;; Implement f as a Scheme procedure. What does f do? 
;; it reverses a list, I think
(define (fintwelve sent)
  (if (empty? sent) 
      '()
      (sentence (fintwelve (butfirst sent)) (first sent))))

;; 12.5  [8.8] Write an exaggerate procedure which exaggerates sentences:

;; > (exaggerate '(i ate 3 potstickers))
; (I ATE 6 POTSTICKERS)
;; > (exaggerate '(the chow fun is good here))
;; (THE CHOW FUN IS GREAT HERE)
;; It should double all the numbers in the sentence, and it should replace "good" with "great," "bad" with "terrible," and anything else you can think of.
;; copied from chapter 8
;; do-great-stuff in more-simply
(define (exaggerate12 the-sent)
  (if (= (count the-sent) 1)
      (more:do-great-stuff (first the-sent))
      (sentence (more:do-great-stuff (first the-sent)) 
                (exaggerate12 (butfirst the-sent)))))

;; 12.6  [8.11] Write a GPA procedure. It should take a sentence of grades as its argument and return the corresponding grade point average:
;; > (gpa '(A A+ B+ B))
;; 3.67
;; Hint: write a helper procedure base-grade that takes a grade as argument and returns 0, 1, 2, 3, or 4, 
;; and another helper procedure grade-modifier that returns −.33, 0, or .33, depending on whether the grade has a minus, a plus, or neither.
;; base-grade and modify-grade in more-simply
;; how to keep the number in a recursive function that takes one arg?
;; no idea right now; just make another function

(define (get-grade-total grades) 
  (if (= (count grades) 1)
      (+ (more:base-grade (first grades)) (more:modify-grade (first grades)))
      (+ (more:base-grade (first grades)) (more:modify-grade (first grades)) (get-grade-total (butfirst grades)))))

(define (gpa12 grades)
  (/ (get-grade-total grades) (count grades)))
;; so it rounds a bit
;; the other two solutions did the same thing with another function in there

;; 12.7  Write a procedure spell-number that spells out the digits of a number:
;; Use this helper procedure:
(define (spell-digit digit)
  (item (+ 1 digit)
	'(zero one two three four five six seven eight nine)))

(define (spell-number nums)
  (cond [(empty? nums) '()]
        [(= (count nums) 1) (spell-digit nums)]
        [else (sentence (spell-digit (first nums)) (spell-number (butfirst nums)))]))

;; 12.8  Write a procedure numbers that takes a sentence as its argument and returns another sentence containing only the numbers in the argument:
;; this is filter/keep
(define (numbers the-sent)
  (cond [(empty? the-sent) '()]
        [(number? (first the-sent)) (sentence (first the-sent) (numbers (butfirst the-sent)))]
        [(not (number? (first the-sent))) (sentence (numbers (butfirst the-sent)))]
        [else '()]))

;; I will include an answer from https://github.com/pongsh/simply-scheme-exercises/blob/master/12-the-leap-of-faith/12.8.scm
(define (numbers2 sent)
  (if (empty? sent)
      '()
      (if (number? (first sent))
                    (se (first sent) (numbers2 (bf sent)))
                    (se (numbers2 (bf sent))))))

;; Simpler than mine, but I think it could be better. Replace compound "if" with "cond"
(define (numbers3 sent)
  (cond [(empty? sent) '()]
        [(number? (first sent)) (se (first sent) (numbers3 (bf sent)))]
        [else (se (numbers3 (bf sent)))]))

;; 12.9  Write a procedure real-words that takes a sentence as argument and returns all the "real" words of the sentence, 
;; using the same rule as the real-word? procedure from Chapter 1
;; from chapter 1
(define (real-word? wd)
  (not (member? wd '(a the an in of and for to with))))

(define (real-words the-words)
  (cond [(empty? the-words)  '()]
        [(real-word? (first the-words)) (sentence (first the-words) (real-words (butfirst the-words)))]
        [(not (real-word? (first the-words))) (sentence (real-words (butfirst the-words)))]
        [else '()]))

;; 12.10  Write a procedure remove that takes a word and a sentence as arguments and returns the same sentence, 
;;but with all copies of the given word removed:
(define (remove-word the-word the-sent)
  (cond [(empty? the-sent) '()]
        [(equal? the-word (first the-sent)) (sentence (remove-word the-word (butfirst the-sent)))]
        [(not (equal? the-word (first the-sent))) (sentence (first the-sent) 
                                                            (remove-word the-word 
                                                                         (butfirst the-sent)))]
        [else '()]))

;; 12.12  Write a procedure arabic which converts Roman numerals into Arabic numerals:
;; You will probably find the roman-value procedure from Chapter 6 helpful. 
;; Don't forget that a letter can reduce the overall value if the letter that comes after it has a larger value, such as the C in MCM.

(define (roman-value letter)
  (cond [(equal? letter 'I) 1]
        [(equal? letter 'V) 5]
        [(equal? letter 'X) 10]
        [(equal? letter 'L) 50]
        [(equal? letter 'C) 100]
        [(equal? letter 'D) 500]
        [(equal? letter 'M) 1000]
        [else 'huh?]))

(define (first-less-than-second the-nums)
  (cond [(equal? (count the-nums) 1) #f]
        [(< (roman-value (first the-nums)) (roman-value (first (butfirst the-nums)))) #t]
        [else #f]))

(define (subtract-first-from-second the-nums)
  (- (roman-value (first (butfirst the-nums))) 
     (roman-value (first the-nums))))

(define (arabic the-nums)
  (cond [(empty? the-nums) '()]
        [(equal? (count the-nums) 1) (roman-value the-nums)]
        [(first-less-than-second the-nums) (+ (subtract-first-from-second the-nums) 
                                              (arabic (butfirst (butfirst the-nums))))]
        [(not (first-less-than-second the-nums)) (+ (roman-value (first the-nums)) 
                                                    (arabic (butfirst the-nums)))]))

;;  12.13  Write a new version of the describe-time procedure from Exercise . Instead of returning a decimal number, it should behave like this:
; > (describe-time 22222)
; (6 HOURS 10 MINUTES 22 SECONDS)
; > (describe-time 4967189641)
; (1 CENTURIES 57 YEARS 20 WEEKS 6 DAYS 8 HOURS 54 MINUTES 1 SECONDS)
; Can you make the program smart about saying 1 CENTURY instead of 1 CENTURIES? 

(define (get-time-measure time)
  (cond [(>= time CENTURY-SEC) 'CENTURIES]
        [(>= time YEAR-SEC)    'YEARS]
        [(>= time WEEK-SEC)    'WEEKS]
        [(>= time DAY-SEC)     'DAYS]
        [(>= time HOUR-SEC)    'HOURS]
        [(>= time MINUTE-SEC)  'MINUTES]
        [else 'SECONDS]))

; quotient is like floor
(define (get-time-floor time)
  (cond [(>= time CENTURY-SEC) (quotient time CENTURY-SEC)]
        [(>= time YEAR-SEC)    (quotient time YEAR-SEC)] 
        [(>= time WEEK-SEC)    (quotient time WEEK-SEC)]
        [(>= time DAY-SEC)     (quotient time DAY-SEC)]
        [(>= time HOUR-SEC)    (quotient time HOUR-SEC)]
        [(>= time MINUTE-SEC)  (quotient time MINUTE-SEC)]
        [else 'SECONDS]))

(define (get-time-multiplier time)
  (cond [(>= time CENTURY-SEC) CENTURY-SEC] 
        [(>= time YEAR-SEC)    YEAR-SEC] 
        [(>= time WEEK-SEC)    WEEK-SEC] 
        [(>= time DAY-SEC)     DAY-SEC]
        [(>= time HOUR-SEC)    HOUR-SEC]
        [(>= time MINUTE-SEC)  MINUTE-SEC]
        [else 'SECONDS]))

(define (describe-time12 time)
  (cond [(not (number? time)) time]
        [(not (positive? time)) time]
        [(= time 1) (sentence time 'SECOND)]
        [(< time 60)  (sentence time 'SECONDS)]
        ; ((>= time 60) )
        [else (sentence (get-time-floor time) 
                        (get-time-measure time) 
                        (describe-time12 (- time 
                                            (* (get-time-floor time) 
                                               (get-time-multiplier time)))))]))
;; I did not do tail recursion. But then again, the authors did not mention tail-recursion.
;; I don't think I got it when I did the other Scheme tutorials.
;; But I was able to use loop-recur in Clojure, and I think that is similar to tail-recursion:
;; from https://clojure.org/reference/special_forms#recur
;; "recur in other than a tail position is an error."
;; The page also says it is not tail-call optimized, and the only thing to look out for is that you are not using an infinite sequence:
;; "There is no tail-call optimization and the use of self-calls for looping of unknown bounds is discouraged."
;; I wonder if there is a library that can check your recursive calls.

;; In the Shido tutorial, Shido will have a wrapper procedure that calls the tail-recursive procedure.
;; Also, look here: https://stackoverflow.com/questions/13664639/tail-recursive-functions-in-scheme
#|
(define (factorial X)
      (cond
            ((eqv? X 1) 1)
            ((number? X)(* X (factorial (- X 1))))))
Not tail-recursive since the inner call to factorial is used in a call to *

Tail-recursive:
(define (factorial x acc)
  (if (zero? x)
      acc
      (factorial (sub1 x) (* x acc))))
The last call to factorial is not tail-recursive because it is on the last line, 
but because its result is not fed into another function.
|#
;; https://stackoverflow.com/questions/33923/what-is-tail-recursion

;; this one references the Scheme standard:
;; https://stackoverflow.com/questions/18817102/how-do-i-detect-functions-that-i-can-apply-tail-call-optimisation-to

;; Apparently DR Racket can check for tail-recursion:
;; https://stackoverflow.com/questions/12925369/racket-identifying-tail-recursion

;; A few bits of advice from the text:
;;  If your function is supposed to return a number, it must return a number all the time, even in the base case. 
;; You can use this idea to help you check the correctness of the base case expression.

;; If your base case doesn't make sense in its own right, it probably means that you're trying to compensate for a mistake in the recursive case. 

(module+ test
  (require rackunit)
  (check-true #t)

  ;; 12.02
  (printf "(my-acronym '(hello this is my sentence)): ~a \n" (my-acronym '(hello this is my sentence)))
  (check-equal? (my-acronym '(hello this is my sentence)) 'htims "Error for: (my-acronym '(hello this is my sentence)")
  (printf "(fintwelve '(this is a sentence)): ~a \n" (fintwelve '(this is a sentence)))

  ;; 12.04
  (check-equal? (fintwelve '(this is a sentence)) '(sentence a is this) "Error for: (fintwelve '(this is a sentence))")

  ;; 12.05
  (printf "(exaggerate12 '(i ate 3 potstickers)): ~a \n" (exaggerate12 '(i ate 3 potstickers)))
  (check-equal? (exaggerate12 '(i ate 3 potstickers)) '(i ate 6 potstickers) "Error for (exaggerate12 '(i ate 3 potstickers))")
  (printf "(exaggerate12 '(the chow fun is good here)): ~a \n" (exaggerate12 '(the chow fun is good here)))
  (check-equal? (exaggerate12 '(the chow fun is good here)) '(the chow fun is great here) "Error for (exaggerate12 '(the chow fun is good here))")
  (printf "(exaggerate12 '(but the egg drop soup is bad)): ~a \n" (exaggerate12 '(but the egg drop soup is bad)))
  (check-equal? (exaggerate12 '(but the egg drop soup is bad)) '(but the egg drop soup is terrible) "Error for (exaggerate12 '(but the egg drop soup is bad))")  
  ;; 12.06
  (printf "(gpa12 '(A A+ B+ B)): ~a \n" (gpa12 '(A A+ B+ B)))
  (check-equal? (gpa12 '(A A+ B+ B)) 3.665 "Error for (gpa12 '(A A+ B+ B))")

  ;; 12.07
  (printf "(spell-number 1971): ~a \n" (spell-number 1971))
  (check-equal? (spell-number 1971) '(one nine seven one) "Error for: (spell-number 1971)")

  ;; 12.08
  (printf "(numbers '(76 trombones and 110 cornets)): ~a \n" (numbers '(76 trombones and 110 cornets)))
  (check-equal? (numbers '(76 trombones and 110 cornets)) '(76 110) "Error for: (numbers '(76 trombones and 110 cornets))")
  (printf "(numbers3 '(76 trombones and 110 cornets)): ~a \n" (numbers3 '(76 trombones and 110 cornets)))
  (check-equal? (numbers3 '(76 trombones and 110 cornets)) '(76 110) "Error for: (numbers3 '(76 trombones and 110 cornets))")

  ;; 12.09
  (printf "(real-words '(the end of the world in a day and a half)): ~a \n"  (real-words '(the end of the world in a day and a half)))
  (check-equal? (real-words '(the end of the world in a day and a half)) '(end world day half) "Error for: (real-words '(the end of the world in a day and a half))")

  ;; 12.10 
  (printf "(remove-word 'the '(the song love of the loved by the beatles)): ~a \n" (remove-word 'the '(the song love of the loved by the beatles)))
  (check-equal? (remove-word 'the '(the song love of the loved by the beatles)) '(song love of loved by beatles) "Error for: (remove-word 'the '(the song love of the loved by the beatles))")

  ;; 12.12
  (printf "(arabic 'MCMLXXI): ~a \n" (arabic 'MCMLXXI))
  (check-equal? (arabic 'MCMLXXI) 1971  "Error for: (arabic 'MCMLXXI)")
  (printf "(arabic 'MLXVI): ~a \n" (arabic 'MLXVI))
  (check-equal? (arabic 'MLXVI) 1066  "Error for: (arabic 'MLXVI)")

  ;; 12.13
  (printf "(describe-time12 22222): ~a \n" (describe-time12 22222))
  (check-equal? (describe-time12 22222) '(6 HOURS 10 MINUTES 22 SECONDS) "Error for: (describe-time12 22222)")
  (printf "(describe-time12 4967189641): ~a \n" (describe-time12 4967189641))
  (check-equal? (describe-time12 4967189641) '(1 CENTURIES 57 YEARS 20 WEEKS 6 DAYS 8 HOURS 54 MINUTES 1 SECOND) "Error for: (describe-time12 4967189641)")

) 
  
  ; (printf " : ~a \n"  )
  ; (check-equal?  "Error for: ")

