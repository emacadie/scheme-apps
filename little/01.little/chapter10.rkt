#lang racket/base

(require (prefix-in rb6:  rnrs/base-6)
         (prefix-in ris6: rnrs/io/simple-6)
         (prefix-in lt-sc: "little-schemer.rkt"))

; entry: a pair of lists, whose first list is a set
; A table is also called an environment. I did not know that.
; A table is a list of entries.

; Page 181: six types: *const, *quote, *identifier, *lambda, *cond, *application
; we can represent types as functions, which they call "actions"
; I think they are expanding "value" from chapter 6



(module+ test
  (require (prefix-in runit: rackunit))
  (runit:check-true #t)
  (lt-sc:display-all "testing chapter 10 of 'The Little Schemer'")

  (runit:check-equal? (lt-sc:looking 'caviar '(6 2 4 caviar 5 7 3)) #t)
  (runit:check-equal? (lt-sc:looking 'caviar '(6 2 grits caviar 5 7 3)) #f)

  (define p176-entry (lt-sc:new-entry '(entree dessert)
                                      '(spaghetti spu moni)))
  (lt-sc:display-all "p176-entry: " p176-entry)
  (define p176-table-02 
    (lt-sc:extend-table (lt-sc:new-entry '(appetizer entree beverage)
                                         '(food tastes good)) 
                        p176-entry))
  (lt-sc:display-all "p176-table-02: " p176-table-02)
  (runit:check-equal? (lt-sc:lookup-in-table 'entree p176-table-02 lt-sc:first)
                      'tastes)
  (newline)
  (lt-sc:display-all "Done with chapter 10 tests at " (lt-sc:display-date))
)

