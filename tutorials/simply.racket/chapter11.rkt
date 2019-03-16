#lang simply-scheme

; Chapter 11: Recursion

(require "more-simply.rkt")
; (require "simply-constants.rkt")
(butfirst '(This is chapter 11 recursion))

;; Recursion
;; Recursive function must have at least two possibilities:
;; 1. Calling itself
;; 2. Base case: "one that can be solved without calling the procedure recursively"

;; they start with a case of 1, then 2, then 3, then 4, and then go backwards
;; although for one function, they make their base case up to 3 letters, and use 4 and 5 to generalize

;; from book: "Every recursive procedure must include two parts: 
;; one or more recursive cases, in which the recursion reduces the size of the problem, 
;; and one or more base cases, in which the result is computable without recursion."
;; So you could have multiple base cases

;; Boring problems
;;  11.2  [8.12][4] When you teach a class, people will get distracted if you say "um" too many times. 
;; Write a count-ums that counts the number of times "um" appears in a sentence:
;; Write count-ums recursively.
;; remember, cond needs an extra parens, and a parens around else
;; there is no function "else"
;; Not anymore
;; this uses two recursive cases
(define (my-count-ums sent)
  (cond [(equal? 0  (length sent)) 0]
        [(equal? 'um (first sent)) (+ 1 (my-count-ums (bf sent)))]
        [else (my-count-ums (bf sent))]))

;;  11.3  [8.13] Write a procedure phone-unspell that takes a spelled version of a phone number, 
;; such as POPCORN, and returns the real phone number, in this case 7672676. 
;; You will need a helper procedure that translates a single letter into a digit:
;; phone-letter from more-simply
;; Write phone-unspell recursively.
(define (my-phone-unspell wd)
  (cond [(equal? 1 (count wd) ) (phone-letter wd)]
        [else (word (phone-letter (first wd)) (my-phone-unspell (bf wd)))]))

;; 11.4  Who first said "use what you have to get what you need"? 
;; Sussman and Abelson

;;  11.5  Write a procedure initials that takes a sentence as its argument and returns a sentence of the first letters in each of the sentence's words:
(define (initials-r the-sent)
  (cond [(equal? 0 (length the-sent)) '()]
        [(equal? 1 (length the-sent)) (first (first the-sent))]
        [else (sentence (first (first the-sent)) (initials-r (butfirst the-sent)))]))

;;  11.6  Write a procedure countdown that works like this:
; ** EXAMPLES OMITTED
(define (countdown number)
  (if (equal? 0 number) 
      (sentence 'BLASTOFF!)
      (sentence number (countdown (- number 1)))))

;;  11.7  Write a procedure copies that takes a number and a word as arguments and returns a sentence containing that many copies of the given word:
;; Examples Omitted, see tests below
;; It is tedious typing them out, but then it's obvious
(define (copies number the-word)
  (if (equal? 0 number) 
      (sentence '())
      (sentence the-word (copies (- number 1) the-word))))


(module+ test
  (require rackunit)
  (check-true #t)
  ; (define (ends-vowel? wd) (vowel? (last wd)))
  ; (printf "(who '(sells out)): ~a \n" (who '(sells out)))
  ; (check-equal? (who '(sells out)) '(pete sells out roger sells out john sells out keith sells out) "Error for (who '(sells out))")
  (printf "(my-count-ums '(today um we are going to um talk about the combining um method)) : ~a \n" (my-count-ums '(today um we are going to um talk about the combining um method)))
  (check-equal? (my-count-ums '(today um we are going to um talk about the combining um method)) 3 "Error for: (my-count-ums '(today um we are going to um talk about the combining um method))")
  (printf "(my-phone-unspell 'popcorn): ~a \n" (my-phone-unspell 'popcorn))
  (check-equal? (my-phone-unspell 'popcorn) 7672676 "Error for (my-phone-unspell 'popcorn)")
  (printf "(initials-r '(if i needed someone)) : ~a \n" (initials-r '(if i needed someone)))
  (check-equal? (initials-r '(if i needed someone)) '(i i n s)  "Error for: (initials-r '(if i needed someone))")
  (printf "(countdown 10) : ~a \n" (countdown 10))
  (check-equal? (countdown 10) '(10 9 8 7 6 5 4 3 2 1 BLASTOFF!) "Error for: (countdown 10)")
  (printf "(countdown 3): ~a \n" (countdown 3))
  (check-equal? (countdown 3) '(3 2 1 BLASTOFF!) "Error for: (countdown 3)")
  (printf "(copies 8 'spam): ~a \n" (copies 8 'spam))
  (check-equal? (copies 8 'spam) '(spam spam spam spam spam spam spam spam) "Error for: (copies 8 'spam)")

)
  ; (printf " : ~a \n"  )
  ; (check-equal?  "Error for: ")
