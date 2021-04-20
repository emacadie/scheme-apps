#lang racket/base

(require (prefix-in rb6:  rnrs/base-6)
         (prefix-in ris6: rnrs/io/simple-6)
         (prefix-in ss-sc: "seasoned-schemer.rkt")
         (prefix-in uuid: uuid))

(define x
  (cons 'chicago (cons 'pizza '())))

(ss-sc:display-all "Here is x: " x)
(set! x 'gone)
(ss-sc:display-all "Here is x after calling set!: " x)
; setq doesn't have a value. It changes a value
(set! x 'skins)
; I am not clear why they are using externally defined values in their functions
(define (gourmet food)
  (cons food (cons x '())))
(ss-sc:display-all "Here is (gourmet onion): " (gourmet 'onion))


(define (gourmand food)
  (set! x food)
  (cons food (cons x '())))
(ss-sc:display-all "Here is (gourmand 'potato): " (gourmand 'potato))
(ss-sc:display-all "And x is: " x)

(define (diner food)
  (cons 'milkshake (cons food '())))

; this will store the last thing in x
(define (dinerR food)
  (set! x food)
  (cons 'milkshake (cons food '())))

(define omnivore
  (let ([x 'ministrone])
    (lambda (food)
      (set! x food)
    (cons food (cons x '())))))

(ss-sc:display-all "Here is (omnivore 'bouillabaisse): " (omnivore 'bouillabaisse))
(ss-sc:display-all "And x is: " x)
; they could use a name other than "x", so I am not clear what they are getting at
; and why have the let outside the lambda?

; The Sixteeth Commandment: Use (set! ... ) only with names defined in (let ... ) blocks

; The Seventeenth Commandment: Use (set! x ...) for (let ((x ...)) ... )
; only if there is at least one (lambda ... ) between it and the "let"

(define (nibbler food)
  (let ([x 'donut])
    (set! x food)
    (cons food (cons x '()))))
(ss-sc:display-all "Here is (nibbler 'cheese): " (nibbler 'cheese))
(ss-sc:display-all "And x is: " x)

(define food 'none)
(ss-sc:display-all "here is food: " food)
(define (glutton x)
  (set! food x)
  (cons 'more (cons x (cons 'more (cons x '())))))
; changes food
(ss-sc:display-all "here is (glutton 'milk): " (glutton 'milk))
(ss-sc:display-all "here is food: " food)
; So the "new" scheme style of (define (f-name arg)...) 
; is like putting a lambda in between set and let

; write a function swapping x and food
; would need a third variable

; The Eighteenth Commandment: Use (set! x ...) only when the value that x
; refers to is no longer needed.

(define (chex-nous (a food))
  (let ([a food])
    (set! food x)
    (set! x a)))



(module+ test
  (require (prefix-in runit: rackunit))
  (runit:check-true #t)
  (ss-sc:display-all "----------------------------------------------\n"
                     "testing chapter 15 of 'The Seasoned Schemer'")
  ; (runit:check-equal? (leftmost '(((a) b) (c d))) 'a)

 

  (newline)
  (ss-sc:display-all "Here is a UUID w/gensym: " (gensym (uuid:uuid-string)))
  (ss-sc:display-all "Done with chapter 15 tests at " (ss-sc:display-date)
                     "\n---------------------------------------------------"))

