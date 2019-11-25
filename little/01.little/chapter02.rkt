#lang racket/base

(require (prefix-in rb6:  rnrs/base-6)
         (prefix-in ris6: rnrs/io/simple-6)
         (prefix-in lt-sc: "little-schemer.rkt"))

(module+ test
  (require (prefix-in runit: rackunit))
  (runit:check-true #t)
  (lt-sc:display-all "testing chapter 2 of 'The Little Schemer'")

  (runit:check-equal? (lt-sc:lat? '(Jack Sprat could eat no chicken fat)) #t)
  ; true, because each s-expr is an atom
  (runit:check-equal? (lt-sc:lat? '((Jack) Sprat could eat no chicken fat)) #f)
  ; false, since (car l) is a list
  ; so lat? is false if you get a list inside a list.
  (runit:check-equal? (lt-sc:lat? '(Jack (Sprat could) eat no chicken fat)) #f)
  (runit:check-equal? (lt-sc:lat? '()) #t) ; no list in the list
  ; I guess they want us to write/derive the function. I skipped ahead.
  (runit:check-equal? (lt-sc:lat? '(bacon and eggs)) #t)
  (runit:check-equal? (lt-sc:lat? '(bacon (and eggs))) #f)
  ; page 19

  (newline)
  (lt-sc:display-all "Done with chapter 02 tests at " (lt-sc:display-date))
#|

|#
)


