#lang racket/base

(require (prefix-in rb6:  rnrs/base-6)
         (prefix-in ris6: rnrs/io/simple-6)
         (prefix-in lt-sc: "little-schemer.rkt"))

(module+ test
  (require (prefix-in runit: rackunit))
  (runit:check-true #t)
  (lt-sc:display-all "testing chapter 8 of 'The Little Schemer'")

  ; rember is like filter (w/pre-set function)
  ; so is rember-f, but now we can send a function
  (runit:check-equal? (lt-sc:rember-f rb6:= 5 '(6 2 5 3))
                      '(6 2 3))
  (runit:check-equal? (lt-sc:rember-f eq? 'jelly '(jelly beans are good))
                      '(beans are good))
  (runit:check-equal? (lt-sc:rember-f equal? 
                                      '(pop corn) 
                                      '(lemonade (pop corn) and (cake)))
                      '(lemonade and (cake)))

  (lt-sc:display-all "rember-f filters out, not keeps in")
  ; They use lambda in functions for function definition.
  ; I use the short version.

  ; Now for some currying: Defining a function as a wrapper around another
  ; function with a default argument
  ; eq?-salad is a "wrapper" around eq?-c, which takes 2 args
  ; With eq?-salad, we pre-set one to 'salad
  (runit:check-equal? (lt-sc:eq?-salad 'salad) #t)
  (runit:check-equal? (lt-sc:eq?-salad 'bagel) #f)
  (runit:check-equal? ((lt-sc:eq?-c 'salad) 'salad) #t)
  (runit:check-equal? ((lt-sc:eq?-c 'salad) 'bagel) #f)
  ; Notice two left-parens around lt-sc:eq?-c, 
  ; one right paren around 'salad, and one right paren around 'bagel
  ; eq?-c has a lambda inside a lambda
  ; "lambda" is a Scheme function that returns a function
  ; and "define" just gives that function a label
  ; I have used the alternate syntax
  (lt-sc:display-all "About to do rember-f2")

  (runit:check-equal? ((lt-sc:rember-f2 equal?) 5 '(6 2 5 3))
                      '(6 2 3))
  (lt-sc:display-all "About to do rember-f2 02")
  (runit:check-equal? ((lt-sc:rember-f2 eq?) 'jelly '(jelly beans are good))
                      '(beans are good))
  (lt-sc:display-all "About to do rember-f2 03")
  (runit:check-equal? ((lt-sc:rember-f2 equal?) 
                       '(pop corn) 
                       '(lemonade (pop corn) and (cake)))
                      '(lemonade and (cake)))

  (runit:check-equal? (lt-sc:rember-eq? 'tuna '(tuna salad is good))
                      '(salad is good))
  (runit:check-equal? ((lt-sc:rember-f2 eq?) 'tuna '(tuna salad is good))
                      '(salad is good))
  (runit:check-equal? ((lt-sc:rember-f2 eq?) 
                       'tuna '(shrimp salad and tuna salad))
                      '(shrimp salad and salad))
  (runit:check-equal? ((lt-sc:rember-f2 eq?) 
                       'eq? '(equal? eq ? eqan? eqlist? eqpair?))
                      '(equal? eq ? eqan? eqlist? eqpair?))

  (runit:check-equal? (lt-sc:insertL2 'topping 
                                      'fudge 
                                      '(ice cream with fudge for dessert))
                      '(ice cream with topping fudge for dessert))
  (runit:check-equal? (lt-sc:insertL2 'jalapeno 
                                      'and
                                      '(tacos tamales and salsa))
                      '(tacos tamales jalapeno and salsa))
  (runit:check-equal? (lt-sc:insertL2 'e 'd '(a b c d f g d h))
                      '(a b c e d f g d h))

  
  (runit:check-equal? (lt-sc:subst8 'topping 
                                   'fudge 
                                   '(ice cream with fudge for dessert))
                      '(ice cream with topping for dessert))
  (runit:check-equal? (lt-sc:subst8 'jalapeno 
                                   'and
                                   '(tacos tamales and salsa))
                      '(tacos tamales jalapeno salsa))
  (runit:check-equal? (lt-sc:subst8 'e 'd '(a b c d f g d h))
                      '(a b c e f g d h))

  (runit:check-equal? (lt-sc:rember8 'mint '(lamb chops and mint jelly)) 
                      '(lamb chops and jelly))
  (runit:check-equal? (lt-sc:rember8 'mint '(lamb chops and mint flavored mint jelly)) 
                      '(lamb chops and flavored mint jelly))
  (runit:check-equal? (lt-sc:rember8 'toast '(bacon lettuce and tomato))
                      '(bacon lettuce and tomato))
  (runit:check-equal? (lt-sc:rember8 'cup '(coffee cup tea cup and hick cup))
                      '(coffee tea cup and hick cup))
  (runit:check-equal? (lt-sc:rember8 'and '(bacon lettuce and tomato))
                      '(bacon lettuce tomato))
  (runit:check-equal? (lt-sc:rember8 'sauce '(soy sauce and tomato sauce))
                      '(soy and tomato sauce))

  ; they made a function insert-g
  ; since insertL, insertR and subst from chapter 3 were so similar
  ; instead of if/cond to select, you just make a function that sends a 
  ; different function to insert-g
  (lt-sc:display-all "up to page 133")

  (lt-sc:display-all "up to page 131")
  (lt-sc:display-all "All hail the Ninth Commandment")

  (runit:check-equal? (lt-sc:value8 '13) 13)
  (runit:check-equal? (lt-sc:value8 '(+ 1 3)) 4)
  ; (runit:check-equal? (lt-sc:value8 '(1 + (3 raise-power 4))) 82)
  ; (runit:check-equal? (lt-sc:value8 'cookie) '())


  (newline)
  (lt-sc:display-all "Done with chapter 08 tests at " (lt-sc:display-date))
)

