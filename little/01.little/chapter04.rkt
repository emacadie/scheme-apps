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

  ; my- works by decrementing both arguments using sub1 and recursion 
  ; until the second one is equal to 0.
  ; https://en.wikipedia.org/wiki/Tuple says:
  ; "In mathematics, a tuple is a finite ordered list (sequence) of elements.
  ; Mathematicians usually write 'tuples' by listing the elements within parentheses "()" and separated by commas
  ; for example, (2,7,4,1,7) denotes a 5-tuple."
  ; I wonder how a "tuple" in Scheme/Lisp will be different than a list.
  ; I guess it is a flat list of numbers.
  ; We will presumably make a function called addtup, which adds the numbers in a list
  (runit:check-equal? (lt-sc:addtup '(3 5 2 8)) 18)
  (runit:check-equal? (lt-sc:addtup '(15 6 7 12 3)) 43)

  (lt-sc:display-all "They revised the First Commandment")

  ; From text: cons builds lists, addtup builds a number from a tup.
  ; So addtup is like "reduce".
  ; last line of addtup:  [else (my+ (car tup) (addtup (cdr tup)))]))
  ; last line of rember:  [else (cons (car lat) (rember a (cdr lat)))]
  
  (newline)
  (lt-sc:display-all "They revised the Fourth Commandment")
  
  ; now multiplication
  (runit:check-equal? (lt-sc:my-x 5 3) 15)
  (runit:check-equal? (lt-sc:my-x 13 4) 52)
  (runit:check-equal? (lt-sc:my-x 12 3) 36)

  (newline)
  (lt-sc:display-all "They introduced the Fifth Commandment")

  (runit:check-equal? (lt-sc:tup+ '(3 6 9 11 4) '(8 5 2 0 7))
                      '(11 11 11 11 11))
  (runit:check-equal? (lt-sc:tup+ '(2 3) '(4 6)) '(6 9))
  ; tup+ adds two tups giving a tup with the sum of the corresponding elements 
  ; of the input tups
  ; It recurs on two tups, which so far they assume are the same length.
  (runit:check-equal? (lt-sc:tup+ '(3 7) '(4 6)) '(7 13))
  ; Ask, and ye shall receive, page 70.
  (runit:check-equal? (lt-sc:tup+ '(3 7) '(4 6 8 1)) '(7 13 8 1))
  ; Now we are getting into multiple terminating conditions.
  (runit:check-equal? (lt-sc:tup+ '(3 7 8 1) '(4 6)) '(7 13 8 1))

  ; using "gt" instead of ">" since that can be an arrow for conversion
  ; like "string->number"
  (runit:check-equal? (lt-sc:my-gt 12 133) #f)
  (runit:check-equal? (lt-sc:my-gt 120 11) #t)
  (runit:check-equal? (lt-sc:my-gt 3 3) #f)
  (runit:check-equal? (lt-sc:my-lt 4 6) #t)
  (runit:check-equal? (lt-sc:my-lt 8 3) #f)
  (runit:check-equal? (lt-sc:my-lt 6 6) #f)
  (lt-sc:display-all "up to page 73")
  (runit:check-equal? (lt-sc:raise-power 1 1) 1)
  (runit:check-equal? (lt-sc:raise-power 2 3) 8)
  (runit:check-equal? (lt-sc:raise-power 5 3) 125)
  (runit:check-equal? (lt-sc:my-div 15 4) 3)
  (runit:check-equal? (lt-sc:my-length 
                       '(hotdogs with mustard sauerkraut and pickles))
                      6)
  (runit:check-equal? (lt-sc:my-length 
                       '(ham and cheese on rye))
                      5)
  (runit:check-equal? (lt-sc:pick 4 '(lasagna spaghetti ravioli macaroni meatball))
                      'macaroni)
  (runit:check-equal? (lt-sc:pick 3 '(lasagna spaghetti ravioli macaroni meatball))
                      'ravioli)
  (runit:check-equal? (lt-sc:rempick 3 '(hotdogs with hot mustard))
                      '(hotdogs with mustard))
  (runit:check-equal? (lt-sc:rempick 2 '(hotdogs with hot mustard))
                      '(hotdogs hot mustard))
  (lt-sc:display-all "Up to page 77")
  (runit:check-equal? (lt-sc:no-nums '(5 pears 6 prunes 9 dates))
                      '(pears prunes dates))
  (runit:check-equal? (lt-sc:no-nums '(5 pears prunes 9 dates))
                      '(pears prunes dates))
  (runit:check-equal? (lt-sc:all-nums '(5 pears 6 prunes 9 dates))
                      '(5 6 9))
  (runit:check-equal? (lt-sc:all-nums '(5 pears prunes 9 dates))
                      '(5 9))
  ; I think they want us to use this from now on
  ; instead of =, my-eq or eq?
  (runit:check-equal? (lt-sc:eqan? 4 4) #t)
  (runit:check-equal? (lt-sc:eqan? 4 5) #f)
  (runit:check-equal? (lt-sc:eqan? 4 'a) #f)
  (runit:check-equal? (lt-sc:eqan? 'a 'a) #t)
  (runit:check-equal? (lt-sc:eqan? 'a 'b) #f)

  (runit:check-equal? (lt-sc:occur 'cup '(coffee cup tea cup and hick cup)) 3)
  (runit:check-equal? (lt-sc:occur 'cup '(coffee cup tea cup and hick)) 2)
  (runit:check-equal? (lt-sc:one? 1) #t)
  (runit:check-equal? (lt-sc:one? 2) #f)
  (runit:check-equal? (lt-sc:one? 'a) #f)

  (lt-sc:display-all "using rempick with one?")
  (runit:check-equal? (lt-sc:rempick 3 '(lemon meringue salty pie))
                      '(lemon meringue pie))

  (newline)
  (lt-sc:display-all "Done with chapter 04 tests at " (lt-sc:display-date))

)
