;; Chapter Nine

;;  9.1  What will Scheme print? Figure it out yourself before you try it on the computer.

(lambda (x) (+ (* x 3) 4))
;; prints nothing; this is the definition
((lambda (x) (+ (* x 3) 4)) 10)
;; 34
(every (lambda (wd) (word (last wd) (bl wd)))
         '(any time at all))
;; it puts the last letter at the end
;; I thought it just took the second letter or something
 ((lambda (x) (+ x 3)) 10 15)
;; I think it ignores the 15
;; I got an error 
;; I knew that

;;  9.2  Rewrite the following definitions so as to make the implicit lambda explicit.
;; SRFI 1 has a "second", so I will call this one "second-ss" for "Simply Scheme"
(define (second-ss stuff)
  (first (bf stuff)))

(define (make-adder num)
  (lambda (x) (+ num x)))

;;  9.3  What does this procedure do?
(define (let-it-be sent)
  (accumulate (lambda (x y) y) sent))
;; it returns the last thing it was sent
;; or last part of collection
;; I would not have guessed that

;; 9.4  The following program doesn't work. Why not? Fix it.

(define (who sent)
  (every describe '(pete roger john keith)))

(define (describe person)
  (se person sent))
;; "sent" is never sent to describe

;; It's supposed to work like this:

(who '(sells out))
(pete sells out roger sells out john sells out keith sells out)

;; here you go:
(define (who sent)
  (every (lambda (x) (se x sent)) '(pete roger john keith)))

;; In each of the following exercises, write the procedure in terms of lambda and higher-order functions. 
;; Do not use named helper procedures. If you've read Part IV, don't use recursion, either.



