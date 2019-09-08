#lang simply-scheme

; Chapter 06: True and false
(require (prefix-in more: "more-simply.rkt"))
(require "simply-constants.rkt")
(butfirst '(This is chapter 6))

; I decided to start putting [ and ] around the conds instead of ( and )
; I just found out about this, and I think it makes things clearer.
; I do not know if that is a Racket thing or a Scheme thing.

;; 6.5  Write a procedure european-time to convert a time from American AM/PM notation into European 24-hour notation. 
;; Also write american-time, which does the opposite:
;; Getting noon and midnight right is tricky. 
(define (american-time number)
  (cond [(equal? number 0) (sentence "12 AM")]
        [(equal? number 12) (sentence 12 'PM)]
        [(and (>= number 1) (< number 12)) (sentence number 'AM)]
        [(and (> number 12) (<= number 23)) (sentence (- number 12) 'PM)]
        [(equal? number 24) (sentence 12 'AM)]))

(define (european-time time)
  ;; (display-all "first time: " (first time))
  (cond [(equal? (first time) "12")
         (if (equal? (more:simply-second time) "am")
             (word "24")
             (word "12"))]
        [(equal? (more:simply-second time) "am") (word (first time))]
        [(equal? (more:simply-second time) "pm") (word (+ (first time) 12))]))

;; 6.6  Write a predicate teen? that returns true if its argument is between 13 and 19.
(define (teen? number)
  (cond [(number? number)
         (if (and (>= number 13) (<= number 19))
             #t
             #f)]
        [else #f]))

;; 6.7  Write a procedure type-of that takes anything as its argument and returns one of the words word, sentence, number, or boolean:
(define (type-of arg)
  (cond [(boolean? arg) 'boolean]
        [(number? arg) 'number]
        [(sentence? arg) 'sentence]
        [(word? arg) 'word]
        [else 'nothing]))

;; 6.8  Write a procedure indef-article that works like this:
;; I only deal w/ words, not sentences
;; there is a better vowel up above
(define (indef-article arg)
  (cond [(and (word? arg) (more:vowel? (first arg))) (sentence 'an arg)]
        [(word? arg) (sentence 'a arg)]
        [else arg]))

;; 6.9  Sometimes you must choose the singular or the plural of a word: 1 book but 2 books.
;; Write a procedure thismany that takes two arguments, a number and a singular noun, and combines them appropriately:
;; load plural before loading this
(define (thismany num thing)
  (cond [(not (number? num)) (sentence "args " num " " thing " does not start with number")]
        [(equal? num 1) (sentence '1 thing)]
        [else (sentence num (more:plural thing))]))

;; 6.10  Write a procedure sort2 that takes as its argument a sentence containing two numbers. 
;; It should return a sentence containing the same two numbers, but in ascending order:
(define (sort2 nums)
  (cond [(or (not (number? (first nums))) 
             (not (number? (more:simply-second nums)))) 
             (sentence "one of your args is not a number: " nums)]
        [(< (more:simply-second nums) (first nums)) 
           (sentence (list (more:simply-second nums) (first nums)))]
        [else nums]))

;; 6.11 Write a predicate valid-date? that takes three numbers as arguments, representing a month, a day of the month, and a year.
;; Your procedure should return #t if the numbers represent a valid date (e.g., it isn't the 31st of September).
;; February has 29 days if the year is divisible by 4, except that if the year is divisible by 100 it must also be divisible by 400. 
;; uses divisible? defined above
(define (valid-month? month day)
  (cond [(and (not (= month 2)) 
              (positive? day) 
              (<= day 30) 
              (member? month '(4 6 9 11))) #t]
        [(and (not (= month 2)) 
              (positive? day) 
              (<= day 31) 
              (member? month '(1 3 5 7 8 10 12))) #t]
        [else #f]))

(define (valid-feb-date? day year)
  (cond [(<= day 28) #t]
        [(and (= day 29) 
              (more:divisible? year 100) 
              (more:divisible? year 400)) #t]
        [(and (= day 29) 
              (more:divisible? year 100) 
              (not (more:divisible? year 400)) ) #f]
        [(and (= day 29) 
              (more:divisible? year 4)) #t]
        [else #f]))

(define (valid-date? month day year)
  (cond [(or (not (number? month)) (not (number? day)) (not (number? year))) #f]
        [(> month 12) #f]
        [(and (not (= month 2)) (valid-month? month day)) #t]
        [(and (= month 2) (valid-feb-date? day year)) #t]
        [else #f]))

;; 6.12  Make plural handle correctly words that end in y but have a vowel before the y, such as boy.
;; Then teach it about words that end in x (box). What other special cases can you find?
;; load vowel? from above
;(define (plural wd)
;  (if (equal? (last wd) 'y)
;      (word (bl wd) 'ies)
;      (word wd 's)))
; now in more-simply

;; 6.13  Write a better greet procedure that understands as many different kinds of names as you can think of:
;; maybe I am a bad person, but I am skipping this one.

;; 6.14  Write a procedure describe-time that takes a number of seconds as its argument and returns a more useful description of that amount of time:
(define (time-sentence time unit-constant unit)
  (sentence (+ (quotient time unit-constant) (exact->inexact (/ (remainder time unit-constant) unit-constant))) unit))

(define (describe-time time)
  (cond [(not (number? time)) time]
        [(not (positive? time)) time]
        [(>= time CENTURY-SEC) (time-sentence time CENTURY-SEC 'CENTURIES)]
        [(>= time YEAR-SEC)    (time-sentence time YEAR-SEC 'YEARS)]
        [(>= time DAY-SEC)     (time-sentence time DAY-SEC 'DAYS)]
        [(>= time HOUR-SEC)    (time-sentence time HOUR-SEC 'HOURS)]
        [(>= time MINUTE-SEC)  (time-sentence time MINUTE-SEC 'MINUTES)]
        [else (sentence time 'SECONDS)]))
;; it seems to have an issue with large numbers

(module+ test
  (require rackunit)
  (check-true #t)

  ;; 6.05
  (printf "(european-time '(8 am)): ~a \n" (european-time '(8 am)))
  (check-equal? (european-time '(8 am)) 8 "Error with (european-time '(8 am)")
  (printf "(european-time '(4 pm)): ~a \n" (european-time '(4 pm)))
  (check-equal? (european-time '(4 pm)) 16 "Error with (european-time '(4 pm))")
  (printf "(american-time 21): ~a \n" (american-time 21))
  (check-equal? (american-time 21) '(9 PM) "Error with (american-time 21)")
  (printf "(american-time 12) ~a \n" (american-time 12))
  (check-equal? (american-time 12) '(12 PM) "Error with (american-time 12)")
  (printf "(european-time '(12 am)) ~a \n" (european-time '(12 am)))
  (check-equal? (european-time '(12 am)) 24 "Error with (european-time '(12 am))")

  ;; 6.06
  (printf "(teen? 12): ~a \n" (teen? 12))
  (check-equal? (teen? 12) #f "Error with (teen? 12)")
  (printf "(teen? 20): ~a \n" (teen? 20))
  (check-equal? (teen? 20) #f "Error with (teen? 20)")
  (printf "(teen? 13): ~a \n" (teen? 13))
  (check-equal? (teen? 13) #t "Error with (teen? 13)")

  ;; 6.08
  (printf "(indef-article 'beatle): ~a \n" (indef-article 'beatle))
  (check-equal? (indef-article 'beatle) '(a beatle) "Error with (indef-article 'beatle)")
  (printf "(indef-article 'album): ~a \n" (indef-article 'album))
  (check-equal? (indef-article 'album) '(an album) "Error with (indef-article 'album)")

  ;; 6.09
  (printf "(thismany 1 'car): ~a \n" (thismany 1 'car))
  (check-equal? (thismany 1 'car) '(1 car) "Error for (thismany 1 'car)")
  (printf "(thismany 2 'car): ~a \n" (thismany 2 'car))
  (check-equal? (thismany 2 'car) '(2 cars) "Error for (thismany 2 'car)")

  ;; 6.10
  (printf "(sort2 '(5 7)): ~a \n" (sort2 '(5 7)))
  (check-equal? (sort2 '(5 7)) '(5 7) "Error for (sort2 '(5 7))")
  (printf "(sort2 '(7 5)): ~a \n" (sort2 '(7 5)))
  (check-equal? (sort2 '(7 5)) '(5 7) "Error for (sort2 '(7 5))")

  ;; 6.11
  (printf "(valid-date? 10 4 1949): ~a \n" (valid-date? 10 4 1949))
  (check-equal? (valid-date? 10 4 1949) #t "Error for (valid-date? 10 4 1949)")
  (printf "(valid-date? 20 4 1776): ~a \n" (valid-date? 20 4 1776))
  (check-equal? (valid-date? 20 4 1776) #f "Error for (valid-date? 20 4 1776)")
  (printf "(valid-date? 5 0 1992): ~a \n" (valid-date? 5 0 1992))
  (check-equal? (valid-date? 5 0 1992) #f "Error for (valid-date? 5 0 1992)")
  (printf "(valid-date? 2 29 1900): ~a \n" (valid-date? 2 29 1900))
  (check-equal? (valid-date? 2 29 1900) #f "Error for (valid-date? 2 29 1900)")
  (printf "(valid-date? 2 29 2000): ~a \n" (valid-date? 2 29 2000))
  (check-equal? (valid-date? 2 29 2000) #t "Error for (valid-date? 2 29 2000)")

  ;; 6.14
  (printf "(describe-time 45): ~a \n" (describe-time 45) )
  (check-equal? (describe-time 45) '(45 SECONDS) "Error for (describe-time 45)")
  (printf "(describe-time 930): ~a \n" (describe-time 930))
  (check-equal? (describe-time 930) '(15.5 MINUTES) "Error for (describe-time 930)")
  (printf "(describe-time 30000000000): ~a \n" (describe-time 30000000000))
  (check-equal? (describe-time 30000000000) '(9.506426344208686 CENTURIES) "Error for (describe-time 30000000000)")

)
