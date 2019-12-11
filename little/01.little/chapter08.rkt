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

  (lt-sc:display-all "up to page 131")

  (newline)
  (lt-sc:display-all "Done with chapter 08 tests at " (lt-sc:display-date))
)

