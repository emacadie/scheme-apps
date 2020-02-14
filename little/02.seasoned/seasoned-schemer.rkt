#lang racket/base

(require (prefix-in rb6:  rnrs/base-6)
         (prefix-in ris6: rnrs/io/simple-6)
         ; (prefix-in rd:   racket/date)
         (prefix-in srfi-19: srfi/19))

(provide atom?          ; preface
         display-date
         display-all
         member?       ; chapter 11
         my-add1       ; chapter 04
         my-sub1       ; chapter 04
         two-in-a-row-page7 ; chapter 11
)

; in preface
(define (atom? x)
  (rb6:and (rb6:not (pair? x))
           (rb6:not (null? x))))

(define (display-all . vs)
  (for-each ris6:display vs)
  (ris6:newline))

(define (display-date)
  (let ([the-date (seconds->date (current-seconds))] )
    (srfi-19:date->string the-date "~Y-~m-~d ~H:~M:~S")))

;; racket/base has a function "add1", but I will "add one" of my own.
(define (my-add1 x)
  (+ 1 x))

; sub1 is another that is in racket/base, but not R6RS.
; I assume they want us to write these ourselves.
; Should I check for positivity?
(define (my-sub1 x)
  (- x 1))

; Mine is tail-recursive.
(define (member? the-atom the-list)
  (cond [(null? the-list) #f]
        [(equal5? the-atom (car the-list)) #t]
        [else (member? the-atom (cdr the-list))]))

;; functions for member?
; to do the mutual calls, I need their equal, not mine.
(define (equal5? a b)
  (cond [(rb6:and (atom? a) (atom? b)) (eqan? a b)]
        [(rb6:or (atom? a) (atom? b)) #f] 
        [else (eqlist5? a b)]))

(define (eqlist5? l1 l2)
  (cond [(rb6:and (null? l1) (null? l2)) #t]
        [(rb6:or (null? l1) (null? l2)) #f]
        [else (rb6:and (equal5? (car l1) (car l2))
                       (eqlist5? (cdr l1) (cdr l2)))]))


; I assume we were supposed to use the "=" function we made in chapter 04
(define (eqan? a1 a2)
  (cond [(rb6:and (number? a1) (number? a2) (my-eq a1 a2)) #t]
        [(rb6:and (not (number? a1)) (not (number? a2)) (eq? a1 a2)) #t]
        [else #f]))

; equal, for numbers. eq? is for other atoms
(define (my-eq x y)
  (cond [(my-lt x y) #f]
        [(my-gt x y) #f]
        [else #t]))

; using "gt" for greater than instead of ">" 
; since that can be an arrow for conversion, like "string->number"
(define (my-gt x y)
  (cond [(rb6:zero? x) #f]
        [(rb6:zero? y) #t]
        [else (my-gt (sub1 x) (sub1 y))]))

; less than
(define (my-lt x y)
  (cond [(rb6:zero? y) #f]
        [(rb6:zero? x) #t]        
        [else (my-lt (sub1 x) (sub1 y))]))

;; end functions for member?

;; chapter 11
(define (is-first? a lat)
  (cond [(null? lat) #f]
        [else (equal5? (car lat) a)]))

(define (two-in-a-row? lat)
  (cond [(null? lat) #f]
        [else (or (is-first? (car lat) (cdr lat)) 
                  (two-in-a-row? (cdr lat)))]))

(define (two-in-a-row2? lat)
  (cond [(null? lat) #f]
        [else (is-first-b? (car lat) (cdr lat))]))

(define (is-first-b? a lat)
  (cond [(null? lat) #f]
        [else (or (equal5? (car lat) a)
                  (two-in-a-row2? lat))]))

(define (two-in-a-row-b? preceding lat)
  (cond [(null? lat) #f]
        [else (or (equal5? (car lat) preceding)
                  (two-in-a-row-b? (car lat) (cdr lat)))]))

(define (two-in-a-row-page7 lat)
  (cond [(null? lat) #f]
        [else (two-in-a-row-b? (car lat) (cdr lat))]
)
)

(module+ test
  (require (prefix-in runit: rackunit))
  (runit:check-true #t)
  (runit:check-equal? (rb6:rational-valued? 6/10) #t)
  (display-all "testing " "testing")
  (runit:check-equal? (atom? (quote ())) #f)
  (runit:check-equal? (atom? (rb6:quote ())) #f)
  (runit:check-equal? (atom? '()) #f)
  (newline)
  (display-all "Done with preface tests at " (display-date))
) ; end module+ test

  


