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

;; Getting noon and midnight right is tricky. 
(define (american-time 'number))
