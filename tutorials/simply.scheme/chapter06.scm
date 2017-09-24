;; chapter 6
;; A function that returns either #t or #f is called a predicate
;; these may come up later:
(define (vowel? letter)
  (member? letter 'aeiou))

(define (positive? number)
  (> number 0))

(define (buzz num)
  (if (or (divisible? num 7) (member? 7 num))
      'buzz
      num))

(define (divisible? big little)
  (= (remainder big little) 0))

(define (plural wd)
  (if (equal? (last wd) 'y)
      (word (bl wd) 'ies)
      (word wd 's)))
;; Cond takes any number of arguments, each of which is two expressions inside a pair of parentheses. Each argument is called a cond clause.
(define (roman-value letter)
  (cond ((equal? letter 'i) 1)
        ((equal? letter 'v) 5)
        ((equal? letter 'x) 10)
        ((equal? letter 'l) 50)
        ((equal? letter 'c) 100)
        ((equal? letter 'd) 500)
        ((equal? letter 'm) 1000)
        (else 'huh?)))

;; exercises
;; 6.1  What values are printed when you type these expressions to Scheme? (Figure it out in your head before you try it on the computer.)

(cond ((= 3 4) '(this boy))
      ((< 2 5) '(nowhere man))
      (else '(two of us)))
;; prints (nowhere man)
(cond (empty? 3)
      (square 7)
      (else 9))
;; prints 3, I guessed 9

(define (third-person-singular verb)
  (cond ((equal? verb 'be) 'is)
        ((equal? (last verb) 'o) (word verb 'es))
        (else (word verb 's))))

(third-person-singular 'go)
;; prints goes

;; 6.2  What values are printed when you type these expressions to Scheme? (Figure it out in your head before you try it on the computer.)

(or #f #f #f #t)
;; true
(and #f #f #f #t)
;; false
(or (= 2 3) (= 4 3))
;; false
(not #f)
;; true
(or (not (= 2 3)) (= 4 3))
;; true
(or (and (= 2 3) (= 3 3)) (and (< 2 3) (< 3 4)))
;; true

;; 6.3  Rewrite the following procedure using a cond instead of the ifs:

(define (sign number)
  (if (< number 0)
      'negative
      (if (= number 0)
            'zero
              'positive)))

(define (sign number)
  (cond ((< number 0) 'neg)
        ((= number 0) 'zero)
        (else 'pos)))

;;  6.4  Rewrite the following procedure using an if instead of the cond:

(define (utensil meal)
  (cond ((equal? meal 'chinese) 'chopsticks)
        (else 'fork)))

(define (if-utensil meal)
  (if (equal? meal 'chinese)
      'chopsticks
      'fork))

;; Real Exercises

;; Note: Writing helper procedures may be useful in solving some of these problems.

;;6.5  Write a procedure european-time to convert a time from American AM/PM notation into European 24-hour notation. Also write american-time, which does the opposite:

;;> (european-time '(8 am))
;;8

;;> (european-time '(4 pm))
;;16

;;> (american-time 21)
;;(9 PM)

;;> (american-time 12)
;;(12 PM)

;;> (european-time '(12 am))
;;24

(define (display-all . vs)
  (for-each display vs)
  (newline))
;; Getting noon and midnight right is tricky. 
(define (american-time number)
  (cond ((equal? number 0) (display-all "12 AM"))
        ((and (>= number 1) (<= number 12)) (display-all number " AM"))
        ((and (>= number 13) (<= number 23)) (display-all (- number 12) " PM"))
        ((equal? number 24) (display-all "12 AM"))))

(define (european-time time)
  ;; (display-all "first time: " (first time))
  (cond ((equal? (first time) "12")
         (if (equal? (second time) "am")
             (display-all "0")
             (display-all "12")))
   ((equal? (second time) "am") (display-all (first time)))
   ((equal? (second time) "pm") (display-all (+ (first time) 12)))))

;; 6.6  Write a predicate teen? that returns true if its argument is between 13 and 19.
(define (teen? number)
  (cond ((number? number)
         (if (and (>= number 13) (<= number 19))
             #t
             #f))
        (else #f)))

;; 6.7  Write a procedure type-of that takes anything as its argument and returns one of the words word, sentence, number, or boolean:
(define (type-of arg)
  (cond ((boolean? arg) 'boolean)
        ((number? arg) 'number)
        ((sentence? arg) 'sentence)
        ((word? arg) 'word)
        (else 'nothing)))

;; 6.8  Write a procedure indef-article that works like this:
;; ** examples omitted **
;; I only deal w/ words, not sentences
(define (vowel? arg)
  (cond ((or (equal? arg "a") (equal? arg "e") (equal? arg "i") (equal? arg "o") (equal? arg "u")) #t)
        ((or (equal? arg "a") (equal? arg "E") (equal? arg "I") (equal? arg "O") (equal? arg "U")) #t)
      (else #f)))
      
(define (indef-article arg)
  (cond ((and (word? arg) (vowel? (first arg))) (display-all "an " arg))
        ((word? arg) (display-all "a " arg))
        (else arg)))

;; 6.9  Sometimes you must choose the singular or the plural of a word: 1 book but 2 books.
;; Write a procedure thismany that takes two arguments, a number and a singular noun, and combines them appropriately:
;; load plural before loading this
(define (thismany num thing)
  (cond ((not (number? num)) (display-all "args " num " " thing " does not start with number"))
        ((equal? num 1) (display-all "1 " thing))
        (else (display-all num " " (plural thing)))))

;; 6.10  Write a procedure sort2 that takes as its argument a sentence containing two numbers. It should return a sentence containing the same two numbers, but in ascending order:
(define (sort2 nums)
  (cond ((or (not (number? (first nums))) (not (number? (second nums)))) (display-all "one of your args is not a number: " nums))
        ;; ((< num1 num2) (list num1 num2))
        ((< (second nums) (first nums)) (display-all (list (second nums) (first nums))))
        (else (nums))))

;; Write a predicate valid-date? that takes three numbers as arguments, representing a month, a day of the month, and a year.
;; Your procedure should return #t if the numbers represent a valid date (e.g., it isn't the 31st of September).
;; February has 29 days if the year is divisible by 4, except that if the year is divisible by 100 it must also be divisible by 400. 
;; uses divisible? defined above
(define (valid-month? month day)
  (cond ((and (not (= month 2)) (positive? day) (<= day 30) (member? month '(4 6 9 11))) #t)
        ((and (not (= month 2)) (positive? day) (<= day 31) (member? month '(1 3 5 7 8 10 12))) #t)
        (else #f)))

(define (valid-feb-date? day year)
  (cond ((<= day 28) #t)
        ((and (= day 29) (divisible? year 100) (divisible? year 400)) #t)
        ((and (= day 29) (divisible? year 100) (not (divisible? year 400)) ) #f)
        ((and (= day 29) (divisible? year 4)) #t)
        (else #f)))

(define (valid-date? month day year)
  (cond ((or (not (number? month)) (not (number? day)) (not (number? year)))
         ;; (display-all "one of your args is not a number: " month " or " day " or " year )
         #f)
        ((> month 12) ;; (display-all "Month cannot be greater than 12, you entered " month)
         #f)
        ((and (not (= month 2)) (valid-month? month day)) #t)
        ((and (= month 2) (valid-feb-date? day year)) #t)
        (else #f)))

