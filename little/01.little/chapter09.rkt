#lang racket/base

(require (prefix-in rb6:  rnrs/base-6)
         (prefix-in ris6: rnrs/io/simple-6)
         (prefix-in lt-sc: "little-schemer.rkt"))

(module+ test
  (require (prefix-in runit: rackunit))
  (runit:check-true #t)
  (lt-sc:display-all "testing chapter 9 of 'The Little Schemer'")

  (runit:check-equal? (lt-sc:looking 'caviar '(6 2 4 caviar 5 7 3)) #t)
  (runit:check-equal? (lt-sc:looking 'caviar '(6 2 grits caviar 5 7 3)) #f)
  ; couldn't keep-looking result in an infinite loop if lat is all numbers?
  (runit:check-equal? (lt-sc:keep-looking 'caviar 3 '(6 2 4 caviar 5 7 3)) #t)

  (runit:check-equal? (lt-sc:shift '((a b) c)) '(a (b c)))
  (runit:check-equal? (lt-sc:shift '((a b) (c d))) '(a (b (c d))))
  ; shift takes a pair, and moves the second sub-element of the first list
  ; and adds it to the second list
  ; Is a partial function a function that could never return? 
  ; or never return for some arguments?
  ; page 153
  (runit:check-equal? (lt-sc:weight* '((a b) c)) 7)
  (runit:check-equal? (lt-sc:weight* '(a (b c))) 5)

  ; page 154: Partial function does not return a value for all arguments
  ; Isn't that a sign of a badly designed or implemented function?
  ; total function: returns a value for any/all arguments
  ; Is that the point of this chapter? Try to always return a value?
  (runit:check-equal? (lt-sc:shuffle '(a (b c))) '(a (b c)))
  (runit:check-equal? (lt-sc:shuffle '(a b)) '(a b))
  ; starting page 155
  (runit:check-equal? (lt-sc:ackermann 1 0) 2)
  (runit:check-equal? (lt-sc:ackermann 1 1) 3)
  (runit:check-equal? (lt-sc:ackermann 2 2) 7)
  ; the following does not give an answer
  ; (runit:check-equal? (lt-sc:ackermann 4 3) 7)
  ; starting page 170
  (lt-sc:display-all "done with chapter 9")

  ; from https://mvanier.livejournal.com/2700.html
  (define Yx
    (lambda (f)
      ((lambda (x)
         (f (lambda (y) ((x x) y))))
       (lambda (x)
         (f (lambda (y) ((x x) y)))))))

  (define factorialx
     (Yx (lambda (func)
           (lambda (num)
              (if (= num 0)
                  1
                  (* num (func (- num 1))))))))
  ; Not sure I see the point of the Y-Combinator.
  ; Maybe I will just be grateful that Scheme can do recursion.
  (newline)
  (lt-sc:display-all "Done with chapter 09 tests at " (lt-sc:display-date))
)

