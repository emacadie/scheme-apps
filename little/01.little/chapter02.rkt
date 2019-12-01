#lang racket/base

(require (prefix-in rb6:  rnrs/base-6)
         (prefix-in ris6: rnrs/io/simple-6)
         (prefix-in lt-sc: "little-schemer.rkt"))

(module+ test
  (require (prefix-in runit: rackunit))
  (runit:check-true #t)
  (lt-sc:display-all "testing chapter 2 of 'The Little Schemer'")

  (runit:check-equal? (lt-sc:lat? '(Jack Sprat could eat no chicken fat)) #t)
  ; true, because each s-expr is an atom
  (runit:check-equal? (lt-sc:lat? '((Jack) Sprat could eat no chicken fat)) #f)
  ; false, since (car l) is a list
  ; so lat? is false if you get a list inside a list.
  (runit:check-equal? (lt-sc:lat? '(Jack (Sprat could) eat no chicken fat)) #f)
  (runit:check-equal? (lt-sc:lat? '()) #t) ; no list in the list
  ; I guess they want us to write/derive the function. I skipped ahead.
  (runit:check-equal? (lt-sc:lat? '(bacon and eggs)) #t)
  (runit:check-equal? (lt-sc:lat? '(bacon (and eggs))) #f)

  (runit:check-equal? (or (null? '()) (lt-sc:atom? '(d e f g))) #t)
  (runit:check-equal? (or (null? '(a b c)) (null? '())) #t)
  (runit:check-equal? (or (null? '(a b c)) (null? '(atom))) #f)
  ; or sees if either condition is true
  (runit:check-equal? (lt-sc:member? 'tea '(coffee tea or milk) ) #t)
  (runit:check-equal? (lt-sc:member? 'poached '(fried eggs and scrambled eggs))
                      #f)
  (runit:check-equal? (lt-sc:member? 'meat '(mashed potatoes and meat gravy))
                      #t)
  (lt-sc:display-all "else is a question whose value is always true in a cond, man")
  (lt-sc:display-all "The First Commandment: Always ask null? as ")
  (lt-sc:display-all "the first question in expressing any function.")
  ; they define "member?" with an "or" in the "else", and the recursive call
  ; to "member?" as one of the args to "or".

  (runit:check-equal? (lt-sc:member? 'meat '(meat gravy)) #t)
  (runit:check-equal? (lt-sc:member? 'meat '(and meat gravy)) #t)
  (runit:check-equal? (lt-sc:member? 'meat '(potatoes and meat gravy)) #t)
  (runit:check-equal? (lt-sc:member? 'liver '(bagels and lox)) #f)
  ; done w/chapter 2

  (newline)
  (lt-sc:display-all "Done with chapter 02 tests at " (lt-sc:display-date))

)


