#lang racket/base

(require (prefix-in rb6:  rnrs/base-6)
         (prefix-in ris6: rnrs/io/simple-6)
         (prefix-in lt-sc: "little-schemer.rkt"))


(module+ test
  (require (prefix-in runit: rackunit))
  (runit:check-true #t)
  (lt-sc:display-all "testing chapter 6 of 'The Little Schemer'")

  ; Numbers are arithmatic expessions.
  ; So are symbols like 'cookie, apparently.
  ; Definition: an atom, or two arithmatic expressions linked by +, * or pow

  (runit:check-equal? (eq? (quote a) 'a) #t)
  (runit:check-equal? (rb6:eq? 'a 'a) #t)
  ; "numbered?" checks whether an s-expression contains only arithmatic 
  ; expressions: numbers, +, *, power.
  (runit:check-equal? (lt-sc:numbered? 1) #t)
  (runit:check-equal? (lt-sc:numbered? '(2 * sausage)) #f)
  (runit:check-equal? (lt-sc:numbered? '(3 + (4 raise-power 5))) #t)
  (runit:check-equal? (lt-sc:value '13) 13)
  (runit:check-equal? (lt-sc:value '(1 + 3)) 4)
  (runit:check-equal? (lt-sc:value '(1 + (3 raise-power 4))) 82)
  (runit:check-equal? (lt-sc:value 'cookie) '())
  ; value solves the expression given to it
  ; From page 104
  ; Try to write the function value for a new kind of arithmetic expression that is either:
  ; - a number 
  ; - a list of the atom + followed by two arithmetic expressions,
  ; - a list of the atom x followed by two arithmetic expressions, or
  ; - a list of the atom raise-power followed by two arithmetic expressions.
  ; Isn't that how Scheme already works?
  ; Before that, let's check is the new expressions are arithmatic
  ; (by their definition)
  (runit:check-equal? (lt-sc:numbered2? '(+ 1 3)) #t)
  (runit:check-equal? (lt-sc:numbered2? '(+ (* 3 6) (raise-power 8 2))) #t)
  (runit:check-equal? (lt-sc:value2 '(+ (* 3 6) (raise-power 8 2))) 82)
  (runit:check-equal? (lt-sc:value2 '(+ 1 3)) 4)

  ; Not too clear what the point of all of this was.


  (newline)
  (lt-sc:display-all "Done with chapter 06 tests at " (lt-sc:display-date))

)

