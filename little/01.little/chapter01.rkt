#lang racket/base

(require (prefix-in rb6:  rnrs/base-6)
         (prefix-in ris6: rnrs/io/simple-6)
         (prefix-in lt-sc: "little-schemer.rkt"))

(module+ test
  (require (prefix-in runit: rackunit))
  (runit:check-true #t)
  (lt-sc:display-all "testing chapter 1")
  (runit:check-equal? (lt-sc:atom? 'atom) #t)
  (runit:check-equal? (lt-sc:atom? 'turkey) #t)
  (runit:check-equal? (list? '(atom)) #t)
  (runit:check-equal? (list? '(atom turkey or)) #t)
  ; I don't think I can test "(atom turkey) or" in Racket
  ; but I can do the next one
  (runit:check-equal? (list? '((atom turkey) or)) #t)
  ; Both atoms and lists are s-expressions
  ; but no method to test: no s-expr?
  ; perhaps we will make one
  ; here is a list w/6 s-exprs
  (runit:check-equal? (list? '(how are you doing so far)) #t)
  ; nested list, collection of s-exprs
  (runit:check-equal? (list? '(((how) are) ((you) (doing so)) far)) #t)
  ; how many s-exprs in '(((how) are) ((you) (doing so)) far))
  ; just by counting the closing parens, I get 7
  ; they get three
  (runit:check-equal? (length '(((how) are) ((you) (doing so)) far)) 3)
  ; So I guess they were not going all the way down.
  ; Which is weird because I thought they said this book is about recursion.
  ; Here is the empty list, or null list.
  (runit:check-equal? (rb6:list? '()) #t) ; Now rb6:list? works like list?. Why?
  (runit:check-equal? (list? '()) #t) 
  ; If it is a list, it is not an atom.
  (runit:check-equal? (lt-sc:atom? '()) #f)
  (runit:check-equal? (list? '(() () () ())) #t)
  ; 'a is the first atom of this list
  (runit:check-equal? (car '(a b c)) 'a)
  (runit:check-equal? (car '((a b c) x y z)) '(a b c))
  (lt-sc:display-all "You can't do (car 'hotdog) or (car '()), silly")
  ; (runit:check-equal? (car 'hotdog) #f)
  (lt-sc:display-all "Law of Car: The primitive car is defined only for non-empty lists")
  
  (runit:check-equal? (car '(((hotdogs)) (and) (pickle) relish)) '((hotdogs)))
  (runit:check-equal? (car (car '(((hotdogs)) (and) (pickle) relish))) '(hotdogs))
  (runit:check-equal? (car (car '(((hotdogs)) (and)))) '(hotdogs))
  (runit:check-equal? (cdr '(a b c)) '(b c))
  (runit:check-equal? (cdr '((a b c) x y z)) '(x y z))
  ; you can't do (cdr 'hamburger)
  ; they say it's '(), but that is not what I get
  ; I misread it.
  (runit:check-equal? (cdr '(hamburger)) '())
  (runit:check-equal? (cdr '((x) t r)) '(t r))
  ; Now they ask for (cdr 'hotdogs), now they are asking for cdr of an atom.
  ; Which you cannot do.
  ; or a null list
  (lt-sc:display-all "The Law of Cdr: The primitive cdr is defined only for "
                     "non-empty lists.")
  (lt-sc:display-all "The cdr of any non-empty list is always another list")

  (runit:check-equal? (car (cdr '((b) (x y) ((c))))) '(x y))
  (runit:check-equal? (cdr (cdr '((b) (x y) ((c))))) '(((c))))
  ; (runit:check-equal? (cdr (car '(a (b (c)) d))) '(b (c)))
  ; undefined, the car is an atom
  (runit:check-equal? (cons 'peanut '(butter and jelly)) '(peanut butter and jelly))
  ; cons adds an atom to the front of a list
  (runit:check-equal? (cons '(banana and) '(peanut butter and jelly))
                      '((banana and) peanut butter and jelly))
  ; cons will add a list to the front of a list and keep it as a list.
  ; So cons-ing a list to the front of a list does not give you a flat list
  (runit:check-equal? (cons '((help) this) '(is very ((hard) to learn)))
                      '(((help) this) is very ((hard) to learn)))
  ; arg for car: non-empty list
  ; arg for cdr: non-empty list
  ; args for cons: anything (any s-expr) and a non-empty list
  (runit:check-equal? (cons '(a b (c)) '()) '((a b (c))))
  (runit:check-equal? (cons 'a '()) '(a))
  ; this won't work, b is not a list
  (runit:check-equal? (cons '((a b c)) 'b) '(((a b c)) . b))
  ; Okay, it works in Racket and Chicken
  (runit:check-equal? (cons 'a 'b) '(a . b))
  ; They say that will not work. Maybe Scheme has changed since the book was written.
  (lt-sc:display-all "The Law of Cons: The primitive cons takes two arguments.")
  (lt-sc:display-all "The second argument to cons must be a list")
  (lt-sc:display-all "The result is a list")

  ; If I could, I would insert the clip from Star Trek II of Kirk yelling
  ; "cons! cons!"
  
  (runit:check-equal? (cons 'a (car '((b) c d))) '(a b))
  (runit:check-equal? (cons 'a (cdr '((b) c d))) '(a c d))
  (runit:check-equal? (null? '()) #t)
  (runit:check-equal? (null? '(a b c)) #f) ; not an empty list
  (runit:check-equal? (null? 'spaghetti) #f)
  ; the book says you cannot ask null? of an atom, but in Racket you can.
  ; But there is a footnote: In practice, (null? atom) is false for everything
  ; except the empty list

  (runit:check-equal? (lt-sc:atom? 'Harry) #t)
  (runit:check-equal? (lt-sc:atom? '(Harry had a heap of apples)) #f)

  ; atom? takes one arg, and it is an s-expr
  (runit:check-equal? (lt-sc:atom? (car '(Harry had a heap of apples))) #t)
  (runit:check-equal? (lt-sc:atom? (cdr '(Harry had a heap of apples))) #f)
  ; (cdr '(Harry)) is an empty list, which is not an atom
  (runit:check-equal? (lt-sc:atom? (cdr '(Harry))) #f) 
  (runit:check-equal? (lt-sc:atom? (car (cdr '(swing low sweet cherry oat)))) 
                      #t)
  (runit:check-equal? (lt-sc:atom? (car (cdr '(swing (low sweet) cherry oat)))) 
                      #f)
  (runit:check-equal? 'Harry 'Harry)

  ; In Racket, equal? checks value, eq? checks if same object
  ; The R6RS spec goes into a LOT more detail. 
  ; I think that Racket does it better.
  (runit:check-equal? (equal? 'Harry 'Harry) #t)
  (runit:check-equal? (eq? 'Harry 'Harry) #t)


  (lt-sc:display-all "The Law of Null?:")
  (lt-sc:display-all "The primitive null? is defined only for lists")
  
  (runit:check-equal? (eq? 'margarine 'butter) #f) ; different atoms

  ; eq? takes 2 args, and they are non-numeric atoms
  (runit:check-equal? (eq? '() '(strawberry)) #f)
  (runit:check-equal? (eq? 6 7) #f)
  ; Book says no answer, since they are lists, 
  ; but in practice lists can be args to eq?

  (lt-sc:display-all "The Law of Eq? The primitive eq? takes two arguments")
  (lt-sc:display-all "Each must be a non-numeric atom")

  (runit:check-equal? (eq? (car '(Mary had a little lamb)) 'Mary) #t)
  (runit:check-equal? (eq? (cdr '(soured milk)) 'milk) #f)
  ; book says "No answer"
  ; cdr returns a list, and eq? likes to deal wit atoms
  ; try this:
  (runit:check-equal? (eq? (car (cdr '(soured milk))) 'milk) #t)
  (runit:check-equal? (eq? (car '(beans beans we need jelly beans))
                           (car (cdr '(beans beans we need jelly beans))))
                      #t)
  ; very verbose way to compare first and second atoms in a list

  (newline)
  (lt-sc:display-all "Done with chapter 01 tests at " (lt-sc:display-date))
#|
Can use rb6: prefix:
eq?
equal?
list?
null?
quote
|#
)


