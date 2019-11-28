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
  (runit:check-equal? (lt-sc:rember 'sauce '(soy sauce and tomato sauce))
                      '(soy and tomato sauce))
  ; I had trouble with rember.
  ; I was never able to do it w/tail recursion in Simply Scheme
  ; "remove-once" in chs 14 and 19
  ; Harder to do here with only the functions they have introduced.

  (runit:check-equal? (lt-sc:firsts '((apple peach pumpkin)
                                      (plum pear cherry)
                                      (grape raisin pea)
                                      (bean carrot eggplant)))
                      '(apple plum grape bean))
  (runit:check-equal? (lt-sc:firsts '((a b) (c d) (e f))) '(a c e))
  (runit:check-equal? (lt-sc:firsts '()) '())
  (runit:check-equal? (lt-sc:firsts '((five plums) 
                                      (four) 
                                      (eleven green oranges)))
                      '(five four eleven))
    (runit:check-equal? (lt-sc:firsts '(((five plums) four)
                                        (eleven green oranges)
                                        ((no) more)))
                      '((five plums) eleven (no)))
    ; firsts takes a list of lists, and returns a list consisting of
    ; the first item from each input list.
    ; Yo dawg, I heard you like lists and cars, 
    ; so I made a function that returns a list of the cars of your lists.


  (lt-sc:display-all "The Second Commandment: Use cons to build lists")

 
  


  (newline)
  (lt-sc:display-all "Done with chapter 03 tests at " (lt-sc:display-date))
#|

|#
)
