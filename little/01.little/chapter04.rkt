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

  (lt-sc:display-all "The First Commandment (first revision): "
  "When recurring on a list of atoms, lat, ask two questions about it: (null? lat) and else. "
  "When recurring on a number, n, ask two questions about it: (zero? n) and else.")

  ; From text: cons builds lists, addtup builds a number from a tup.
  ; So addtup is like "reduce".
  ; last line of addtup:  [else (my+ (car tup) (addtup (cdr tup)))]))
  ; last line of rember:  [else (cons (car lat) (rember a (cdr lat)))]
  
  (newline)
  (lt-sc:display-all "The Fourth Commandment (first revision): "
   "Always change at least one argument while recurring. "
  "It must be changed to be closer to termination. " 
  "The changing argument must be tested in the termination condition: "
  "when using cdr, test termination with null? and when using sub1 , test termination with zero?.")
  
  ; now multiplication
  (runit:check-equal? (lt-sc:my-x 5 3) 15)
  (runit:check-equal? (lt-sc:my-x 13 4) 52)
  (runit:check-equal? (lt-sc:my-x 12 3) 36)

  (newline)
  (lt-sc:display-all "The Fifth Commandment: " 
                     "When building a value with +, always use 0 for the value of the terminating line, for adding 0 does not change the value of an addition. "
  "When building a value with x (multiplication), always use 1 for the value of the terminating line, for multiplying by 1 does not change the value of a multiplication. "
  "When building a value with cons, always consider '() for the value of the terminating line.")

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
  ; up to page 73


  (newline)
  (lt-sc:display-all "Done with chapter 04 tests at " (lt-sc:display-date))

)
