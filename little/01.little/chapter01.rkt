#lang racket/base

(require (prefix-in rb6:  rnrs/base-6)
         (prefix-in ris6: rnrs/io/simple-6)
         (prefix-in lt-sc: "little-schemer.rkt"))




(module+ test
  (require (prefix-in runit: rackunit))
  (runit:check-true #t)
  (runit:check-equal? (rb6:rational-valued? 6/10) #t)
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
)


