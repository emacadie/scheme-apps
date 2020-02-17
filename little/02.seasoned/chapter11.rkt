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
  ;; left off page 7
  ;; their example to walk throught: 
  ;; (ss-sc:two-in-a-row-page7 '(b d e i i a g))
  

   (newline)
   (ss-sc:display-all "Done with chapter 04 tests at " (ss-sc:display-date))
)

