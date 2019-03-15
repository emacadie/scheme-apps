#lang simply-scheme

; Chapter 05: Words and Sentences
(require "more-simply.rkt")


(butfirst '(this is chapter 5))
;; 5.2 For each of the following examples, write a procedure of two arguments that, when applied to the sample arguments, 
;; returns the sample result. Your procedures may not include any quoted data.

;; > (f1 '(a b c) '(d e f))
;; (B C D E)
(define (f1 first-arg second-arg)
  (se (butfirst first-arg) (butlast second-arg)))
;; I am not going to worry about case right now

;; > (f2 '(a b c) '(d e f))
;; (B C D E AF)
(define (f2 first-arg second-arg)
  (se (bf first-arg) (bl second-arg) (word (first first-arg) (last second-arg))))

;; > (f3 '(a b c) '(d e f))
;; (A B C A B C)
(define (f3 first-arg second-arg)
  (se first-arg first-arg))

;; Scheme already has a "second" procedure
(define (f4 first-arg second-arg)
  (word (simply-second first-arg) (simply-second second-arg)))
;; > (f4 '(a b c) '(d e f))
;; BE

;; 5.14 Write a procedure third that selects the third letter of a word (or the third word of a sentence).
(define (simply-third x)
  (first (bf (bf x))))

;;  5.15   Write a procedure first-two that takes a word as its argument, returning a two-letter word containing the first two letters of the argument.
;; > (first-two 'ambulatory)
;; AM
(define (first-two arg-word)
  (word (first arg-word) (first (butfirst arg-word))))

;; 5.16 Write a procedure two-first that takes two words as arguments, returning a two-letter word containing the first letters of the two arguments.
;; > (two-first 'brian 'epstein)
;; BE
;; Now write a procedure two-first-sent that takes a two-word sentence as argument, returning a two-letter word containing the first letters of the two words.
;; > (two-first-sent '(brian epstein))
;; BE
(define (two-first first-arg second-arg)
  (word (first first-arg) (first second-arg)))

(define (two-first-sent first-arg)
  (word (first (first first-arg)) (first (first (butfirst first-arg)))))

;; 5.17 Write a procedure knight that takes a person's name as its argument and returns the name with "Sir" in front of it.
;; > (knight '(david wessel))
;; (SIR DAVID WESSEL)
(define (knight arg-name)
  (se 'Sir arg-name))

;; 5.19 Write a procedure insert-and that takes a sentence of items and returns a new sentence with an "and" in the right place:
;; > (insert-and '(john bill wayne fred joey))
;; (JOHN BILL WAYNE FRED AND JOEY)
(define (insert-and arg-sentence)
  (se (butlast arg-sentence) 'and (last arg-sentence)))

;; 5.20 Define a procedure to find somebody's middle names:
(define (middle-names name-arg)
  (se (butfirst (butlast name-arg))))

;; 5.21  Write a procedure query that turns a statement into a question by swapping the first two words and adding a question mark to the last word:
(define (simply-query arg)
  (sentence (simply-second arg) (first arg) (butfirst (butfirst (butlast arg))) (word (last arg) '? )))
;; I can't do it without a space in between last word and question mark

(module+ test
  (require rackunit)
  (check-true #t)
  (printf "(f1 '(a b c) '(d e f)): ~a \n" (f1 '(a b c) '(d e f)))
  (check-equal? (f1 '(a b c) '(d e f)) '(b c d e) "Error for (f1 '(a b c) '(d e f))")
  (printf "(f2 '(a b c) '(d e f)): ~a \n" (f2 '(a b c) '(d e f)))
  (check-equal? (f2 '(a b c) '(d e f)) '(b c d e af) "Error for (f2 '(a b c) '(d e f))")
  (printf "(f3 '(a b c) '(d e f)): ~a \n" (f3 '(a b c) '(d e f)))
  (check-equal? (f3 '(a b c) '(d e f)) '(a b c a b c) "Error for (f3 '(a b c) '(d e f))" )
  (printf "(f4 '(a b c) '(d e f)): ~a \n" (f4 '(a b c) '(d e f)))
  (check-equal? (f4 '(a b c) '(d e f)) 'be "Error for (f4 '(a b c) '(d e f))")
  (printf "(simply-third 'racket): ~a \n" (simply-third 'racket))
  (check-equal? (simply-third 'racket) 'c "Error for (simply-third 'racket)")
  (printf "(simply-third '(We love all kinds of Lisp)): ~a \n" (simply-third '(We love all kinds of Lisp)))
  (check-equal? (simply-third '(We love all kinds of Lisp)) 'all "Error for (simply-third '(We love all kinds of Lisp))")
  (printf "(first-two 'ambulatory): ~a \n" (first-two 'ambulatory))
  (check-equal? (first-two 'ambulatory) 'am "Error for (first-two 'ambulatory)")
  (printf "(two-first 'hello 'there): ~a \n" (two-first 'hello 'there))
  (check-equal? (two-first 'hello 'there) 'ht "Error for (two-first 'hello 'there)")
  (printf "(two-first-sent '(hello there)): ~a \n" (two-first-sent '(hello there)))
  (check-equal? (two-first-sent '(hello there)) 'ht "Error for (two-first-sent '(hello there))")
  (printf "(knight '(david wessel)): ~a \n" (knight '(david wessel)))
  (check-equal? (knight '(david wessel)) '(Sir david wessel) "Error for (knight '(david wessel))")
  (printf "(insert-and '(john bill wayne fred joey)): ~a \n" (insert-and '(john bill wayne fred joey)))
  (check-equal? (insert-and '(john bill wayne fred joey)) '(john bill wayne fred and joey) "Error for (insert-and '(john bill wayne fred joey))")
  (printf "(insert-and '(kirk picard sisko janeway archer others)): ~a \n" (insert-and '(kirk picard sisko janeway archer others)))
  (check-equal? (insert-and '(kirk picard sisko janeway archer others)) '(kirk picard sisko janeway archer and others) 
                "Error for (insert-and '(kirk picard sisko janeway archer others))")

  (printf "(middle-names '(james paul mccartney)): ~a \n" (middle-names '(james paul mccartney)))
  (check-equal? (middle-names '(james paul mccartney)) '(paul) "Error for (middle-names '(james paul mccartney))")
  (printf "(middle-names '(john ronald raoul tolkien)): ~a \n" (middle-names '(john ronald raoul tolkien)))
  (check-equal? (middle-names '(john ronald raoul tolkien)) '(ronald raoul) "Error: (middle-names '(john ronald raoul tolkien))")
  (printf "(middle-names '(bugs bunny)): ~a \n" (middle-names '(bugs bunny)))
  (check-equal? (middle-names '(bugs bunny)) '() "Error for (middle-names '(bugs bunny))")
  (printf "(middle-names '(peter blair denis bernard noone)): ~a \n" (middle-names '(peter blair denis bernard noone)))
  (check-equal? (middle-names '(peter blair denis bernard noone)) '(blair denis bernard) "Error for (middle-names '(peter blair denis bernard noone))")

  (printf "(simply-query '(you are experienced)): ~a \n" (simply-query '(you are experienced)))
  (check-equal? (simply-query '(you are experienced)) '(are you experienced?) "Error for (simply-query '(you are experienced))")
  (printf "(simply-query '(i should have known better)): ~a \n" (simply-query '(i should have known better)))
  (check-equal? (simply-query '(i should have known better)) '(should i have known better?) "Error for (simply-query '(i should have known better))")
)


