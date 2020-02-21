#lang racket/base

(require (prefix-in rb6:  rnrs/base-6)
         (prefix-in ris6: rnrs/io/simple-6)
         (prefix-in ss-sc: "seasoned-schemer.rkt"))


(module+ test
  (require (prefix-in runit: rackunit))
  (runit:check-true #t)
  (ss-sc:display-all "testing chapter 11 of 'The Seasoned Schemer'")

  ; We will only be dealing with positive integers in this chapter.
  (runit:check-equal? (ss-sc:member? 'sardines '(Italian sardines spaghetti parslet)) 
                      #t)
  (runit:check-equal? (ss-sc:two-in-a-row-page7 '(wash adams jeff monroe)) 
                      #f)
  (runit:check-equal? (ss-sc:two-in-a-row-page7 '(wash adams jeff jeff monroe)) 
                      #t)
  ;; first it's "jeff" (2 f's) then "jefff" (3 f's)
  (runit:check-equal? (ss-sc:two-in-a-row-page7 '(wash adams jeff jefff monroe)) 
                      #f)
  
  ;; their example to walk through:
  (runit:check-equal? (ss-sc:two-in-a-row-page7 '(b d e i i a g))
                      #t)

  (runit:check-equal? (ss-sc:my-sum-of-prefixes '(2 1 9 17 0))
                      '(2 3 12 29 29))
  (runit:check-equal? (ss-sc:my-sum-of-prefixes '(1 1 1 1 1))
                      '(1 2 3 4 5))
  (runit:check-equal? (ss-sc:sum-of-prefixes '(2 1 9 17 0))
                      '(2 3 12 29 29))
  (runit:check-equal? (ss-sc:sum-of-prefixes '(1 1 1 1 1))
                      '(1 2 3 4 5))

  (runit:check-equal? (ss-sc:scramble '(1 1 1 3 4 2 1 1 9 2))
                      '(1 1 1 1 1 4 1 1 1 9))
  (runit:check-equal? (ss-sc:scramble '(1 2 3 4 5 6 7 8 9))
                      '(1 1 1 1 1 1 1 1 1))
  (runit:check-equal? (ss-sc:scramble '(1 2 3 1 2 3 4 1 8 2 10))
                      '(1 1 1 1 1 1 1 1 1 2 8 2))
    
  (newline)
  (ss-sc:display-all "Done with chapter 11 tests at " (ss-sc:display-date))
)

