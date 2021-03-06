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
  (runit:check-equal? (lt-sc:rember 'cup '(coffee cup tea cup and hick cup))
                      '(coffee tea cup and hick cup))
  (runit:check-equal? (lt-sc:rember 'and '(bacon lettuce and tomato))
                      '(bacon lettuce tomato))
  (runit:check-equal? (lt-sc:rember 'sauce '(soy sauce and tomato sauce))
                      '(soy and tomato sauce))
  ; I had trouble with rember.
  ; I was never able to do it w/tail recursion in Simply Scheme
  ; "remove-once" in chs 14 and 19
  ; Harder to do here with only the functions they have introduced.

  (lt-sc:display-all "The Second Commandment: Use cons to build lists")

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


  (lt-sc:display-all "The Third Commandment: When building a list, ")
  (lt-sc:display-all "describe the first typical element, "
                     "and then cons it onto the natural recursion.")
  ; I do not really like "cons". It is not natural to me. 
  ; If you use "cons" with tail recursion, you need "reverse". 
  ; Will we see "reverse"? (I searched; we won't.)
  
  (runit:check-equal? (lt-sc:insertR 'topping 
                                     'fudge 
                                     '(ice cream with fudge for dessert))
                      '(ice cream with fudge topping for dessert))
  (runit:check-equal? (lt-sc:insertR 'jalapeno 
                                     'and
                                     '(tacos tamales and salsa))
                      '(tacos tamales and jalapeno salsa))
  (runit:check-equal? (lt-sc:insertR 'e 'd '(a b c d f g d h))
                      '(a b c d e f g d h))
  ; insertR takes 3 args: new string, old string, and a list of atoms.
  ; It replaces the first instance of the old string with the new and the old.
  
  (runit:check-equal? (lt-sc:insertL 'topping 
                                     'fudge 
                                     '(ice cream with fudge for dessert))
                      '(ice cream with topping fudge for dessert))
  (runit:check-equal? (lt-sc:insertL 'jalapeno 
                                     'and
                                     '(tacos tamales and salsa))
                      '(tacos tamales jalapeno and salsa))
  (runit:check-equal? (lt-sc:insertL 'e 'd '(a b c d f g d h))
                      '(a b c e d f g d h))

  (runit:check-equal? (lt-sc:subst 'topping 
                                   'fudge 
                                   '(ice cream with fudge for dessert))
                      '(ice cream with topping for dessert))
  (runit:check-equal? (lt-sc:subst 'jalapeno 
                                   'and
                                   '(tacos tamales and salsa))
                      '(tacos tamales jalapeno salsa))
  (runit:check-equal? (lt-sc:subst 'e 'd '(a b c d f g d h))
                      '(a b c e f g d h))

  (runit:check-equal? (lt-sc:subst2 'vanilla 'chocolate 'banana
                                    '(banana ice cream with chocolate topping))
                      '(vanilla ice cream with chocolate topping))
 
  (lt-sc:display-all "Now for multirember")
  (runit:check-equal? (lt-sc:multirember 'mint '(lamb chops and mint jelly)) 
                      '(lamb chops and jelly))
  (runit:check-equal? (lt-sc:multirember 'mint '(lamb chops and mint flavored mint jelly)) 
                      '(lamb chops and flavored jelly))
  (runit:check-equal? (lt-sc:multirember 'toast '(bacon lettuce and tomato))
                      '(bacon lettuce and tomato))
  (runit:check-equal? (lt-sc:multirember 'cup '(coffee cup tea cup and hick cup))
                      '(coffee tea and hick))
  (runit:check-equal? (lt-sc:multirember 'and '(bacon lettuce and tomato))
                      '(bacon lettuce tomato))
  (runit:check-equal? (lt-sc:multirember 'sauce '(soy sauce and tomato sauce))
                      '(soy and tomato))

  (lt-sc:display-all "Now for multiinsertR")
  (runit:check-equal? (lt-sc:multiinsertR 'topping 
                                          'fudge 
                                          '(ice cream with fudge for dessert))
                      '(ice cream with fudge topping for dessert))
  (runit:check-equal? (lt-sc:multiinsertR 'jalapeno 
                                          'and
                                          '(tacos tamales and salsa))
                      '(tacos tamales and jalapeno salsa))
  (runit:check-equal? (lt-sc:multiinsertR 'e 'd '(a b c d f g d h))
                      '(a b c d e f g d e h))
  (runit:check-equal? (lt-sc:multiinsertR 'fried
                                          'fish
                                          '(chips and fish or fish and fried))
                      '(chips and fish fried or fish fried and fried))
  (lt-sc:display-all "Now for multiinsertL")
  (runit:check-equal? (lt-sc:multiinsertL 'topping 
                                          'fudge 
                                          '(ice cream with fudge for dessert))
                      '(ice cream with topping fudge for dessert))
  (runit:check-equal? (lt-sc:multiinsertL 'jalapeno 
                                          'and
                                          '(tacos tamales and salsa))
                      '(tacos tamales jalapeno and salsa))
  (runit:check-equal? (lt-sc:multiinsertL 'e 'd '(a b c d f g d h))
                      '(a b c e d f g e d h))
  (runit:check-equal? (lt-sc:multiinsertL 'fried
                                          'fish
                                          '(chips and fish or fish and fried))
                      '(chips and fried fish or fried fish and fried))

  (lt-sc:display-all "The Fourth Commandment (preliminary): ")
  (lt-sc:display-all "Always change at least one argument while recurring. ")
  (lt-sc:display-all "It must be changed to be closer to termination." )
  (lt-sc:display-all "The changing argument must be tested in the termination condition: ")
  (lt-sc:display-all "when using cdr, test termination with null?.")

  (runit:check-equal? (lt-sc:multisubst 'topping 
                                        'fudge 
                                        '(ice cream with fudge for dessert))
                      '(ice cream with topping for dessert))
  (runit:check-equal? (lt-sc:multisubst 'jalapeno 
                                        'and
                                        '(tacos tamales and salsa))
                      '(tacos tamales jalapeno salsa))
  (runit:check-equal? (lt-sc:multisubst 'e 'd '(a b c d f g d h))
                      '(a b c e f g e h))

  (newline)
  (lt-sc:display-all "Done with chapter 03 tests at " (lt-sc:display-date))
#|

|#
)
