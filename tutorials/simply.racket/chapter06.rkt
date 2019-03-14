#lang simply-scheme

(butfirst '(This is chapter 6))

;;6.5  Write a procedure european-time to convert a time from American AM/PM notation into European 24-hour notation. Also write american-time, which does the opposite:

;; Getting noon and midnight right is tricky. 
(define (american-time number)
  (cond ((equal? number 0) (sentence "12 AM"))
        ((and (>= number 1) (<= number 12)) (sentence number " AM"))
        ((and (>= number 13) (<= number 23)) (sentence (- number 12) " PM"))
        ((equal? number 24) (sentence "12 AM"))))

(define (european-time time)
  ;; (display-all "first time: " (first time))
  (cond ((equal? (first time) "12")
         (if (equal? (simply-second time) "am")
             (word "0")
             (word "12")))
   ((equal? (simply-second time) "am") (word (first time)))
   ((equal? (simply-second time) "pm") (word (+ (first time) 12)))))

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
;> (indef-article 'beatle)
;(A BEATLE)

;> (indef-article 'album)
;(AN ALBUM)
;; I only deal w/ words, not sentences
;; there is a better vowel up above
(define (vowel? arg)
  (cond ((or (equal? arg "a") (equal? arg "e") (equal? arg "i") (equal? arg "o") (equal? arg "u")) #t)
        ((or (equal? arg "a") (equal? arg "E") (equal? arg "I") (equal? arg "O") (equal? arg "U")) #t)
      (else #f)))
      
(define (indef-article arg)
  (cond ((and (word? arg) (vowel? (first arg))) (sentence "an " arg))
        ((word? arg) (sentence "a " arg))
        (else arg)))

;; 6.9  Sometimes you must choose the singular or the plural of a word: 1 book but 2 books.
;; Write a procedure thismany that takes two arguments, a number and a singular noun, and combines them appropriately:
;; load plural before loading this
(define (thismany num thing)
  (cond ((not (number? num)) (sentence "args " num " " thing " does not start with number"))
        ((equal? num 1) (sentence "1 " thing))
        (else (sentence num " " (plural thing)))))

;; 6.10  Write a procedure sort2 that takes as its argument a sentence containing two numbers. It should return a sentence containing the same two numbers, but in ascending order:
(define (sort2 nums)
  (cond ((or (not (number? (first nums))) (not (number? (simply-second nums)))) (sentence "one of your args is not a number: " nums))
        ;; ((< num1 num2) (list num1 num2))
        ((< (simply-second nums) (first nums)) (sentence (list (simply-second nums) (first nums))))
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
         ;; (sentence "one of your args is not a number: " month " or " day " or " year )
         #f)
        ((> month 12) ;; (sentence "Month cannot be greater than 12, you entered " month)
         #f)
        ((and (not (= month 2)) (valid-month? month day)) #t)
        ((and (= month 2) (valid-feb-date? day year)) #t)
        (else #f)))

;; 6.12  Make plural handle correctly words that end in y but have a vowel before the y, such as boy.
;; Then teach it about words that end in x (box). What other special cases can you find?
;; load vowel? from above
(define (plural wd)
  (if (equal? (last wd) 'y)
      (word (bl wd) 'ies)
      (word wd 's)))
(define (plural-y wd)
  (cond ((vowel? (last (butlast wd))) (word wd 's))
        (else (word (butlast wd) 'ies))))
(define (plurals wd)
  (cond ((not (word? wd)) word)
        ((equal? (last wd) 'y) (plural-y wd))
        ((equal? (last wd) 'x) (word wd 'es) )
        (else (word wd 's))))

;; 6.13  Write a better greet procedure that understands as many different kinds of names as you can think of:
;; maybe I am a bad person, but I am skipping this one.

;; 6.14  Write a procedure describe-time that takes a number of seconds as its argument and returns a more useful description of that amount of time:
(define (describe-time time)
  (cond ((not (number? time)) time)
        ((not (positive? time)) time)
        
        ((>= time 31557600) (sentence (+ (quotient time 31557600) (exact->inexact (/ (remainder time 31557600) 31557600))) 'YEARS))
        ((>= time 86400)    (sentence (+ (quotient time 86400)    (exact->inexact (/ (remainder time 86400) 86400))) 'DAYS))
        ((>= time 3600)     (sentence (+ (quotient time 3600)     (exact->inexact (/ (remainder time 3600) 3600))) 'HOURS))
        ((>= time 60)       (sentence (+ (quotient time 60)       (exact->inexact (/ (remainder time 60) 60))) 'MINUTES))
        (else (sentence time 'SECONDS))))
;; it seems to have an issue with large numbers

(module+ test
  (require rackunit)
  (check-true #t)
  ; (printf "(f1 '(a b c) '(d e f)): ~a \n" (f1 '(a b c) '(d e f)))
  ; (check-equal? (f1 '(a b c) '(d e f)) '(b c d e) "Error for (f1 '(a b c) '(d e f))")
  (printf "(european-time '(8 am)): ~a \n" (european-time '(8 am)))
  (check-equal? (european-time '(8 am)) 8 "Error with (european-time '(8 am)")
;;> 
;;8

;;> (european-time '(4 pm))
;;16

;;> (american-time 21)
;;(9 PM)

;;> (american-time 12)
;;(12 PM)

;;> (european-time '(12 am))
;;24

)
