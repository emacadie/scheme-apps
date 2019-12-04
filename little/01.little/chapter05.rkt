#lang racket/base

(require (prefix-in rb6:  rnrs/base-6)
         (prefix-in ris6: rnrs/io/simple-6)
         (prefix-in lt-sc: "little-schemer.rkt"))


(module+ test
  (require (prefix-in runit: rackunit))
  (runit:check-true #t)
  (lt-sc:display-all "testing chapter 5 of 'The Little Schemer'")

  ; We are dealing with layered lists in this chapter. 
  (runit:check-equal? (lt-sc:rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
                      '((coffee) ((tea)) (and (hick))))
  (runit:check-equal? (lt-sc:rember* 'sauce 
                                     '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
                      '(((tomato)) ((bean)) (and ((flying)))))
  ; This is like the tree chapter in Simply Scheme.
  ; We sometimes recur down the car of the list, as well as the cdr.
  ; Looking at SS chapter 18, maybe not:
  ; Trees as lists: "In other words, a tree is a list whose first element is the datum and whose remaining elements are subtrees."
  ; There, we made two functions for each tree: One for the element, one for the subelements
  ; from chapter 18 file again:
  ; Mutual recursion
  ; initialization procedure calls its helper procedure
  ; helper procedure calls itself and the initialization procedure
  ; init procedure takes tree as its arg, checks if a node is a leaf (or some other check)
  ; else it calls the helper procedure with the tree as a list
  ; helper procedure: if the tree is null, return false
  ; else: call init procedure on the car of tree, call itself on cdr of tree
  ; That is the most general pattern

  ; I am not too sure that applies here.
  (lt-sc:display-all "I am not too sure we are really dealing with trees")
  ; or they figured out how to get it into one function.
  ; but we sometimes recurse on the car as well as the cdr
  (runit:check-equal? (lt-sc:lat? '(((tomato sauce))
                                    ((bean) sauce)
                                    (and ((flying)) sauce)))
                      #f)
  (runit:check-equal? (lt-sc:lat? (car '(((tomato sauce))
                                         ((bean) sauce)
                                         (and ((flying)) sauce))))
                      #f)
  (runit:check-equal? (lt-sc:insertR* 'roast 'chuck
                                      '((how much (wood)) 
                                        could ((a (wood) chuck)) (((chuck)))
                                        (if (a) ((wood chuck))) 
                                        could chuck wood))
                      '((how much (wood)) 
                        could ((a (wood) chuck roast)) (((chuck roast)))
                        (if (a) ((wood chuck roast)))
                        could chuck roast wood))

  (lt-sc:display-all "We got the final version of the First Commandment")
  ; if the car the list is not an atom, recur on both car and cdr of list
  ; if the car is an atom and is what we want, cons something to the recur on cdr
  ; else, cons something else to recur on cdr
  ; Is that it? Are we done? Probably not.
  (lt-sc:display-all "We got the final version of the Fourth Commandment")

  (runit:check-equal? (lt-sc:occur* 'banana 
                                    '((banana) (split ((((banana ice)))
                                                       (cream (banana))
                                                       sherbet))
                                               (banana)
                                               (bread)
                                               (banana brandy)))
                      5)
  (runit:check-equal? (lt-sc:subst* 'orange 'banana
                                    '((banana)
                                      (split ((((banana ice)))
                                              (cream (banana))
                                              sherbet))
                                      (banana)
                                      (bread)
                                      (banana brandy)))
                      '((orange)
                        (split ((((orange ice)))
                                (cream (orange))
                                sherbet))
                        (orange)
                        (bread)
                        (orange brandy)))
  (lt-sc:display-all "But is the juice worth the squeeze? Up to page 85")

  (runit:check-equal? (lt-sc:insertR* 'pecker 'chuck
                                      '((how much (wood)) 
                                        could ((a (wood) chuck)) (((chuck)))
                                        (if (a) ((wood chuck))) 
                                        could chuck wood))
                      '((how much (wood)) 
                        could ((a (wood) chuck pecker)) 
                        (((chuck pecker))) 
                        (if (a) ((wood chuck pecker))) 
                        could chuck pecker wood))

  (runit:check-equal? (lt-sc:insertR* 'pecker 'wood
                                      '((how much (wood)) 
                                        could ((a (wood) chuck)) (((chuck)))
                                        (if (a) ((wood chuck))) 
                                        could chuck wood))
                      '((how much (wood pecker)) 
                        could ((a (wood pecker) chuck)) (((chuck))) 
                        (if (a) ((wood pecker chuck))) 
                        could chuck wood pecker))

  (runit:check-equal? (lt-sc:member* 'chips
                                     '((potato) (chips ((with) fish) (chips))))
                      #t)
  (runit:check-equal? (lt-sc:member* 'clips
                                     '((potato) (chips ((with) fish) (chips))))
                      #f)
  (runit:check-equal? (lt-sc:member* 'fish 
                                     '((potato) (chips ((with) fish) (chips))))
                      #t)

  (newline)
  (lt-sc:display-all "Done with chapter 05 tests at " (lt-sc:display-date))

)
