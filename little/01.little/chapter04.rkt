#lang racket/base

(require (prefix-in rb6:  rnrs/base-6)
         (prefix-in ris6: rnrs/io/simple-6)
         (prefix-in lt-sc: "little-schemer.rkt"))


(module+ test
  (require (prefix-in runit: rackunit))
  (runit:check-true #t)
  (lt-sc:display-all "testing chapter 4 of 'The Little Schemer'")

  ; We will only be dealing with positive integers in this chapter.
  (runit:check-equal? (lt-sc:my-add1 67) 68)
  (runit:check-equal? (lt-sc:my-sub1 5) 4)
  (runit:check-equal? (rb6:zero? 0) #t)
  (runit:check-equal? (rb6:zero? 1492) #f)
  (runit:check-equal? (lt-sc:my+ 46 12) 58)
  ; treating zero? like null? and add1 like cons,
  ; so not violating commandments.
  (runit:check-equal? (lt-sc:my- 14 3) 11)
  (runit:check-equal? (lt-sc:my- 17 9) 8)




  (newline)
  (lt-sc:display-all "Done with chapter 04 tests at " (lt-sc:display-date))
#|

|#
)
