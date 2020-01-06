#lang racket/base

(require (prefix-in rb6:  rnrs/base-6)
         (prefix-in ris6: rnrs/io/simple-6)
         (prefix-in lt-sc: "little-schemer.rkt"))

(module+ test
  (require (prefix-in runit: rackunit))
  (runit:check-true #t)
  (lt-sc:display-all "testing chapter 9 of 'The Little Schemer'")

  (runit:check-equal? (lt-sc:looking 'caviar '(6 2 4 caviar 5 7 3)) #t)
  (runit:check-equal? (lt-sc:looking 'caviar '(6 2 grits caviar 5 7 3)) #f)
  ; couldn't keep-looking result in an infinite loop if lat is all numbers?
  (runit:check-equal? (lt-sc:keep-looking 'caviar 3 '(6 2 4 caviar 5 7 3)) #t)

  (runit:check-equal? (lt-sc:shift '((a b) c)) '(a (b c)))
  (runit:check-equal? (lt-sc:shift '((a b) (c d))) '(a (b (c d))))
  ; shift takes a pair, and moves the second sub-element of the first list
  ; and adds it to the second list
  ; Is a partial function a function that could never return? 
  ; or never return for some arguments?
  ; page 153


  (newline)
  (lt-sc:display-all "Done with chapter 09 tests at " (lt-sc:display-date))
)

