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
;; 11.1  Write downup4 using only the word and sentence primitive procedures. 
;; 
;; I think they mean do something like this:
(define (downup3 wd)
  (se wd
      (bl wd)
      (first wd)
      (bl wd)
      wd))

(define (my-downup4 wd)
  (se wd
      (bl wd)
      (bl (bl wd))
      (first wd)
      (bl (bl wd))
      (bl wd)
      wd))

;;  11.2  [8.12][4] When you teach a class, people will get distracted if you say "um" too many times. 
;; Write a count-ums that counts the number of times "um" appears in a sentence:
; (count-ums '(today um we are going to um talk about the combining um method))
; 3

;; Here are some special-case count-ums procedures for sentences of particular lengths:
(define (count-ums0 sent)
  0)
(define (count-ums1 sent)
  (if (equal? 'um (first sent))
      1
      0))
(define (count-ums2 sent)
  (if (equal? 'um (first sent))
      (+ 1 (count-ums1 (bf sent)))
      (count-ums1 (bf sent))))
(define (count-ums3 sent)
  (if (equal? 'um (first sent))
      (+ 1 (count-ums2 (bf sent)))
      (count-ums2 (bf sent))))

;; Write count-ums recursively.
;; remember, cond needs an extra parens, and a parens around else
;; there is no function "else"
;; this uses two recursive cases
(define (my-count-ums sent)
  (cond ((equal? 0  (length sent)) 0) 
        ((equal? 'um (first sent)) (+ 1 (my-count-ums (bf sent))))
        (else (my-count-ums (bf sent)))))

;;  11.3  [8.13] Write a procedure phone-unspell that takes a spelled version of a phone number, 
;; such as POPCORN, and returns the real phone number, in this case 7672676. 
;; You will need a helper procedure that translates a single letter into a digit:

(define (unspell-letter letter)
  (cond ((member? letter 'abc) 2)
	((member? letter 'def) 3)
	((member? letter 'ghi) 4)
	((member? letter 'jkl) 5)
	((member? letter 'mno) 6)
	((member? letter 'prs) 7)
	((member? letter 'tuv) 8)
	((member? letter 'wxy) 9)
	(else 0)))

;; Here are some some special-case phone-unspell procedures:

(define (phone-unspell1 wd)
  (unspell-letter wd))

(define (phone-unspell2 wd)
  (word (unspell-letter (first wd)) (phone-unspell (bf wd) )))

(define (phone-unspell3 wd)
  (word (unspell-letter (first wd)) (phone-unspell2 (bf wd))))

(define (phone-unspell4 wd)
  (word (unspell-letter (first wd)) (phone-unspell3 (bf wd))))

;; Write phone-unspell recursively.
(define (phone-unspell wd)
  (cond ((equal? 1 (count wd) ) (unspell-letter wd) )
        (else (word (unspell-letter (first wd)) (phone-unspell (bf wd))))))

;; 11.4  Who first said "use what you have to get what you need"? 
;; Sussman and Abelson

;;  11.5  Write a procedure initials that takes a sentence as its argument and returns a sentence of the first letters in each of the sentence's words:
; > (initials '(if i needed someone))
; (I I N S)
(define (initials1 the-sent)
  (first (first the-sent)) 
)

(define (initials2 the-sent)
  (sentence (first (first the-sent)) (initials1 (butfirst the-sent))))

(define (initials3 the-sent)
  (sentence (first (first the-sent)) (initials2 (butfirst the-sent)))
)

(define (initials4 the-sent)
  (sentence (first (first the-sent)) (initials3 (butfirst the-sent)))
)

;; Am i missing something? It seems like the same thing.
;; Unless I am also supposed to check for 0-length?

(define (initials the-sent)
  (cond ((equal? 0 (length the-sent)) '())
        ((equal? 1 (length the-sent)) (first (first the-sent)))
        (else (sentence (first (first the-sent)) (initials (butfirst the-sent))))))

;;  11.6  Write a procedure countdown that works like this:
; > (countdown 10)
; (10 9 8 7 6 5 4 3 2 1 BLASTOFF!)
; > (countdown 3)
; (3 2 1 BLASTOFF!)

(define (cd0 num)
  (sentence 'BLASTOFF!))

(define (cd1 num)
  (sentence '1 (cd0 (- num 1))))

(define (cd2 num)
  (sentence '2 (cd1 (- num 1))))

(define (cd3 num)
  (sentence '3 (cd2 (- num 1))))

(define (cd4 num)
  (sentence '4 (cd3 (- num 1))))

(define (countdown number)
  (if (equal? 0 number) 
      (sentence 'BLASTOFF!)
      (sentence number (countdown (- number 1)))))

;;  11.7  Write a procedure copies that takes a number and a word as arguments and returns a sentence containing that many copies of the given word:
; > (copies 8 'spam)
; (SPAM SPAM SPAM SPAM SPAM SPAM SPAM SPAM)
(define (copies0 num the-word)
  (sentence '()))

(define (copies1 num the-word)
  (sentence the-word))

(define (copies2 num the-word)
  (sentence the-word (copies1 (- num 1) the-word)))

(define (copies3 num the-word)
  (sentence the-word (copies2 (- num 1) the-word)))

(define (copies4 num the-word)
  (sentence the-word (copies3 (- num 1) the-word)))

;; It is tedious typing them out, but then it's obvious
(define (copies number the-word)
  (if (equal? 0 number) 
      (sentence '())
      (sentence the-word (copies (- number 1) the-word))))
 

