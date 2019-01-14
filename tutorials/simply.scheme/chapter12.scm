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
        ((and (equal? (count the-sent) 1) (number? (first the-sent))) (first the-sent))
        ((and (equal? (count the-sent) 1) (not (number? (first the-sent)))) '())
        ((and (> (count the-sent) 1) (number? (first the-sent))) (sentence (first the-sent) (numbers (butfirst the-sent))))
        ((and (> (count the-sent) 1) (not (number? (first the-sent)))) (numbers (butfirst the-sent)))
        (else '())))

;; 12.9  Write a procedure real-words that takes a sentence as argument and returns all the "real" words of the sentence, 
;; using the same rule as the real-word? procedure from Chapter 1
;; from chapter 1
(define (real-word? wd)
  (not (member? wd '(a the an in of and for to with))))

(define (real-words the-words)
  
)


