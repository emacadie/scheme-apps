;; The Leap of Faith
;; https://old.reddit.com/r/compsci/comments/44syr6/understanding_the_recursive_leap_of_faith/
;; Maybe I am not very good at this yet, but the "Leap Of Faith" sounds like it can become a Death March

;; 12.1  Here is a definition of a procedure that returns the sum of the numbers in its argument sentence:

(define (addup nums)
  (if (empty? (bf nums))
      (first nums)
      (+ (first nums) (addup (bf nums)))))

;; Although this works, it could be simplified by changing the base case. Do that.
(define (my-addup nums)
  (if (empty? nums)
      0
      (+ (first nums) (my-addup (bf nums)))))

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

;; 12.3  Can we reduce the factorial base case argument from 0 to -1? 
;; If so, show the resulting procedure. If not, why not?
;; multiplying by 0 would result in 0

;;  12.4  Here's the definition of a function f:
;; math display
;; Implement f as a Scheme procedure. What does f do? 
;; it reverses a list, I think
(define (ffunc sent)
  (if (empty? sent) 
      '()
      (sentence (ffunc (butfirst sent)) (first sent))))

;; 12.5  [8.8] Write an exaggerate procedure which exaggerates sentences:

;; > (exaggerate '(i ate 3 potstickers))
; (I ATE 6 POTSTICKERS)
;; > (exaggerate '(the chow fun is good here))
;; (THE CHOW FUN IS GREAT HERE)
;; It should double all the numbers in the sentence, and it should replace "good" with "great," "bad" with "terrible," and anything else you can think of.

;; copied from chapter 8
(define (do-great-stuff the-word)
  (cond ((equal? the-word 'good) 'great)
        ((equal? the-word 'bad) 'terrible)
        ((number? the-word) (* 2 the-word))
        (else the-word)))

(define (exaggerate12 the-sent)
  (if (= (count the-sent) 1)
      (do-great-stuff (first the-sent))
      (sentence (do-great-stuff (first the-sent)) (exaggerate12 (butfirst the-sent)))))

;; 12.6  [8.11] Write a GPA procedure. It should take a sentence of grades as its argument and return the corresponding grade point average:
;; > (gpa '(A A+ B+ B))
;; 3.67
;; Hint: write a helper procedure base-grade that takes a grade as argument and returns 0, 1, 2, 3, or 4, 
;; and another helper procedure grade-modifier that returns −.33, 0, or .33, depending on whether the grade has a minus, a plus, or neither.
;; from chapter 8
(define (base-grade grade)
  (cond ((equal? (first grade) 'A) 4)
        ((equal? (first grade) 'B) 3)
        ((equal? (first grade) 'C) 2)
        ((equal? (first grade) 'D) 1)
        (else 0)))

;; from chapter 8
(define (grade-modifier grade)
  (cond ((equal? (last grade) '+) 0.33)
        ((equal? (last grade) '-) -0.33)
        (else 0)))

;; how to keep the number in a recursive function that takes one arg?
;; no idea right now; just make another function

(define (get-grade-total grades) 
  (if (= (count grades) 1)
      (+ (base-grade (first grades)) (grade-modifier (first grades)))
      (+ (base-grade (first grades)) (grade-modifier (first grades)) (get-grade-total(butfirst grades)))))

(define (gpa12 grades)
  (/ (get-grade-total grades) (count grades)))
;; so it rounds a bit
;; the other two solutions did the same thing with another function in there

;; 12.7  Write a procedure spell-number that spells out the digits of a number:
;; > (spell-number 1971)
;; (ONE NINE SEVEN ONE)
;; Use this helper procedure:
(define (spell-digit digit)
  (item (+ 1 digit)
	'(zero one two three four five six seven eight nine)))

(define (spell-number nums)
  (cond ((empty? nums) '())
        ((= (count nums) 1) (spell-digit nums))
        (else (word (spell-digit (first nums)) (spell-number (butfirst nums))))))


;; 12.8  Write a procedure numbers that takes a sentence as its argument and returns another sentence containing only the numbers in the argument:
;; > (numbers '(76 trombones and 110 cornets))
;; (76 110)
;; this is filter/keep
(define (numbers the-sent)
  (cond ((empty? the-sent) '())
        ; ((and (equal? (count the-sent) 1) (number? (first the-sent))) (first the-sent))
        ; ((and (equal? (count the-sent) 1) (not (number? (first the-sent)))) '())
        ; ((and (> (count the-sent) 1) (number? (first the-sent))) (sentence (first the-sent) (numbers (butfirst the-sent))))
        ; ((and (> (count the-sent) 1) (not (number? (first the-sent)))) (numbers (butfirst the-sent)))
        ;; added better conditions below
        ((number? (first the-sent)) (sentence (first the-sent) (numbers (butfirst the-sent))))
        ((not (number? (first the-sent))) (sentence (numbers (butfirst the-sent))))
        (else '())))

;; I will include an answer from https://github.com/pongsh/simply-scheme-exercises/blob/master/12-the-leap-of-faith/12.8.scm
(define (numbers2 sent)
  (if (empty? sent)
      '()
      (if (number? (first sent))
                    (se (first sent) (numbers2 (bf sent)))
                    (se (numbers2 (bf sent))))))

;; Simpler than mine, but I think it could be better. Replace compound "if" with "cond"
(define (numbers3 sent)
  (cond ((empty? sent) '())
        ((number? (first sent)) (se (first sent) (numbers3 (bf sent))))
        (else (se (numbers3 (bf sent))))))

;; 12.9  Write a procedure real-words that takes a sentence as argument and returns all the "real" words of the sentence, 
;; using the same rule as the real-word? procedure from Chapter 1
;; from chapter 1
(define (real-word? wd)
  (not (member? wd '(a the an in of and for to with))))

(define (real-words the-words)
  (cond ((empty? the-words) '())
        ((real-word? (first the-words)) (sentence (first the-words) (real-words (butfirst the-words))))
        ((not (real-word? (first the-words))) (sentence (real-words (butfirst the-words))))
        (else '())))

;; 12.10  Write a procedure remove that takes a word and a sentence as arguments and returns the same sentence, 
;;but with all copies of the given word removed:
;; > (remove 'the '(the song love of the loved by the beatles))
;; (SONG LOVE OF LOVED BY BEATLES)

(define (remove-word the-word the-sent)
  (cond ((empty? the-sent) '())
        ((equal? the-word (first the-sent)) (sentence (remove-word the-word (butfirst the-sent))))
        ((not (equal? the-word (first the-sent))) (sentence (first the-sent) (remove-word the-word (butfirst the-sent))))
        (else '())))

;; I kept getting: "Error: call of non-procedure: the"
;; because I had: (equal? (the-word) (first the-sent))

;; 12.12  Write a procedure arabic which converts Roman numerals into Arabic numerals:
; > (arabic 'MCMLXXI)
;1971
;> (arabic 'MLXVI)
;1066
;; You will probably find the roman-value procedure from Chapter 6 helpful. 
;; Don't forget that a letter can reduce the overall value if the letter that comes after it has a larger value, such as the C in MCM.

(define (roman-value letter)
  (cond ((equal? letter 'I) 1)
        ((equal? letter 'V) 5)
        ((equal? letter 'X) 10)
        ((equal? letter 'L) 50)
        ((equal? letter 'C) 100)
        ((equal? letter 'D) 500)
        ((equal? letter 'M) 1000)
        (else 'huh?)))

(define (first-less-than-second the-nums)
  (cond ((equal? (count the-nums) 1) #f)
        ((< (roman-value (first the-nums)) (roman-value (first (butfirst the-nums)))) #t)
        (else #f)))

(define (subtract-first-from-second the-nums)
  (- (roman-value (first (butfirst the-nums))) (roman-value (first the-nums))))

(define (arabic the-nums)
  (cond ((empty? the-nums) '())
        ((equal? (count the-nums) 1) (roman-value the-nums))
        ((first-less-than-second the-nums) (+ (subtract-first-from-second the-nums) (arabic (butfirst (butfirst the-nums)))))
        ((not (first-less-than-second the-nums)) (+ (roman-value (first the-nums)) (arabic (butfirst the-nums))))))

;;  12.13  Write a new version of the describe-time procedure from Exercise . Instead of returning a decimal number, it should behave like this:
; > (describe-time 22222)
; (6 HOURS 10 MINUTES 22 SECONDS)
; > (describe-time 4967189641)
; (1 CENTURIES 57 YEARS 20 WEEKS 6 DAYS 8 HOURS 54 MINUTES 1 SECONDS)
; Can you make the program smart about saying 1 CENTURY instead of 1 CENTURIES? 


(define (get-time-measure time)
  (cond ((>= time 3155760000) 'CENTURIES)
        ((>= time 315576000) 'DECADES)
        ((>= time 31557600) 'YEARS)
        ((>= time 86400) 'DAYS)
        ((>= time 3600) 'HOURS)
        ((>= time 60) 'MINUTES)
        (else 'SECONDS)))

; quotient is like floor
(define (get-time-floor time)
  (cond ((>= time 3155760000) (quotient time 3155760000)) 
        ((>= time 315576000) (quotient time 315576000)) 
        ((>= time 31557600) (quotient time 31557600)) 
        ((>= time 86400) (quotient time 86400))
        ((>= time 3600) (quotient time 3600))
        ((>= time 60) (quotient time 60))
        (else 'SECONDS)))

(define (get-time-multiplier time)
  (cond ((>= time 3155760000) 3155760000) 
        ((>= time 315576000) 315576000) 
        ((>= time 31557600) 31557600) 
        ((>= time 86400) 86400)
        ((>= time 3600) 3600)
        ((>= time 60) 60)
        (else 'SECONDS)))

(define (describe-time12 time)
  (cond ((not (number? time)) time)
        ((not (positive? time)) time)
        ((< time 60)  (sentence time 'SECONDS))
        ; ((>= time 60) )
        (else (sentence (get-time-floor time) (get-time-measure time) (describe-time12 (- time (* (get-time-floor time) (get-time-multiplier time))))))))
;; I did not do tail recursion. But then again, the authors did not mention tail-recursion.
;; I don't think I got it when I did the other Scheme tutorials.
;; But I was able to use loop-recur in Clojure, and I think that is similar to tail-recursion:
;; from https://clojure.org/reference/special_forms#recur
;; "recur in other than a tail position is an error."
;; The page also says it is not tail-call optimized, and the only thing to look out for is that you are not using an infinite sequence:
;; "There is no tail-call optimization and the use of self-calls for looping of unknown bounds is discouraged."
;; I wonder if there is a library that can check your recursive calls.

