#lang racket/base

(require (prefix-in rb6:  rnrs/base-6)
         (prefix-in ris6: rnrs/io/simple-6)
         (prefix-in lt-sc: "little-schemer.rkt"))


(module+ test
  (require (prefix-in runit: rackunit))
  (runit:check-true #t)
  (lt-sc:display-all "testing chapter 3 of 'The Little Schemer'")

  (runit:check-equal? (lt-sc:rember 'mint '(lamb chops and mint jelly)) 
                      '(lamb chops and jelly))
  (runit:check-equal? (lt-sc:rember 'mint '(lamb chops and mint flavored mint jelly)) 
                      '(lamb chops and flavored mint jelly))
  (runit:check-equal? (lt-sc:rember 'toast '(bacon lettuce and tomato))
                      '(bacon lettuce and tomato))
  (runit:check-equal? (lt-sc:rember 'cup '(coffee cup tea cup hick cup))
                      '(coffee tea cup hick cup))
  (runit:check-equal? (lt-sc:rember 'and '(bacon lettuce and tomato))
                      '(bacon lettuce tomato))

  ; up to page 42

  ; This one I had trouble with.
  ; I was never able to do it w/tail recursion in Simply Scheme
  ; "remove-once" in chs 14 and 19
  ; Harder to do here with only the functions they have introduced.


  (lt-sc:display-all "The Second Commandment: Use cons to build lists")

 
  


  (newline)
  (lt-sc:display-all "Done with chapter 03 tests at " (lt-sc:display-date))
#|

|#
)
