#lang racket/base

(require (prefix-in rb6:  rnrs/base-6)
         (prefix-in ris6: rnrs/io/simple-6)
         (prefix-in lt-sc: "little-schemer.rkt"))


(module+ test
  (require (prefix-in runit: rackunit))
  (runit:check-true #t)
  (lt-sc:display-all "testing chapter 7 of 'The Little Schemer'")

  ; sets
  (runit:check-equal? (lt-sc:set? '(apple peaches apple plum)) #f)
  (runit:check-equal? (lt-sc:set? '()) #t)
  (runit:check-equal? (lt-sc:set? '(apple 3 pear 4 9  apple 3 4)) #f)
  ; I changed member? to use equal5?

  (runit:check-equal? (lt-sc:makeset '(apple peach pear peach plum apple lemon peach))
                      '(apple peach pear plum lemon))
  ; makeset cons-es the car onto the result of calling makeset on the 
  ; remaining set with the car taken out of the remainder.
  (runit:check-equal? (lt-sc:makeset '(apple 3 pear 4 9 apple 3 4))
                      '(apple 3 pear 4 9))

  ; "subset?" checks if each atom in s1 is in s2
  ; NOT if all of s1 is in s2 in current order
  (runit:check-equal? (lt-sc:subset? '(5 chicken wings)
                                     '(5 hamburgers
                                         2 pieces fried chicken and
                                         light duckling wings))
                      #t)
  (runit:check-equal? (lt-sc:subset? '(4 pounds of horseradish)
                                     '(four pounds chicken and
                                            5 ounces horseradish))
                      #f)

  ; again, checking if each member in s1 is in s2
  ; so I guess sets can be unordered
  (runit:check-equal? (lt-sc:eqset? '(6 large chickens with wings)
                                    '(6 chickens with large wings))
                      #t)
  
    (runit:check-equal? (lt-sc:intersect? '(stewed tomatoes and macaroni)
                                        '(macaroni and cheese))
                      #t)
  (runit:check-equal? (lt-sc:intersect? '(stewed tomatoes und nacaroni)
                                        '(macaroni and cheese))
                      #f)



  (newline)
  (lt-sc:display-all "Done with chapter 07 tests at " (lt-sc:display-date))

)
