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

  (runit:check-equal? (lt-sc:intersect '(stewed tomatoes and macaroni)
                                       '(macaroni and cheese))
                      '(and macaroni))

  (runit:check-equal? (lt-sc:union '(stewed tomatoes and macaroni casserole)
                                   '(macaroni and cheese))
                      '(stewed tomatoes casserole macaroni and cheese))
  ; when s1 was null, we returned s2. I feel a commandment coming up.
  (runit:check-equal? (lt-sc:intersectall '((a b c) (c a d e) (e f g h a b))) 
                      '(a))
  (runit:check-equal? (lt-sc:intersectall '((6 pears and)
                                            (3 peaches and 6 peppers)
                                            (8 pears and 6 plums)
                                            (and 6 prunes with some apples)))
                      '(6 and))

  (lt-sc:display-all "page 117")
  (runit:check-equal? (lt-sc:a-pair? '(pear pair)) #t)
  (runit:check-equal? (lt-sc:a-pair? '(3 7)) #t)
  (runit:check-equal? (lt-sc:a-pair? '((2) (pair))) #t)
  (runit:check-equal? (lt-sc:a-pair? '(full (house))) #t)
  
  ; you make a pair by cons the first onto the cons of the second onto '()
  ; (cons x1 (cons x2 '()))
  ; "rel" is relation, a list of pairs, or a set of pairs
  ; "fun" is function
  (lt-sc:display-all "page 119")
  (runit:check-equal? (lt-sc:fun? '((8 3) (4 2) (7 6) (6 2) (3 4))) #t)
  (runit:check-equal? (lt-sc:fun? '((d 4) (b 0) (b 9) (e 5) (g 4))) #f)
  ; finite function: A list of pairs in which all the first elements of 
  ; each pair are unique.
  (runit:check-equal? (lt-sc:revrel '((8 a) (pumpkin pie) (got sick)))
                      '((a 8) (pie pumpkin) (sick got)))
  (runit:check-equal? (lt-sc:seconds '((8 3) (4 8) (7 6) (6 2) (3 4)))
                      '(3 8 6 2 4))

  ; a full function is one in which the second elements of the pairs form a set
  ; as well as the first elements of the pairs forming a set (I think)
  (runit:check-equal? (lt-sc:fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4))) #f)
  (runit:check-equal? (lt-sc:fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4))) #t)
  (runit:check-equal? (lt-sc:fullfun? '((grape raisin)
                                        (plum prune)
                                        (stewed prune))) 
                      #f)
  (runit:check-equal? (lt-sc:fullfun? '((grape raisin)
                                        (plum prune)
                                        (stewed grape))) 
                      #t)

  (runit:check-equal? (lt-sc:one-to-one? '((chocolate chip) (doughy cookie))) 
                      #t)


  (newline)
  (lt-sc:display-all "Done with chapter 07 tests at " (lt-sc:display-date))

)

