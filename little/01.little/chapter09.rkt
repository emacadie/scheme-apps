#lang racket/base

(require (prefix-in rb6:  rnrs/base-6)
         (prefix-in ris6: rnrs/io/simple-6)
         (prefix-in lt-sc: "little-schemer.rkt"))

; Into the Y-Combinator rabbit-hole starting on page 160
; Try to define length without "define" function.
; How could we make "length" recursive?
(define (eternity x)
  (eternity x))

; gets length of empty list
(define length-0 ; just go with this for now
  (lambda (l)
    (cond [(null? l) 0]
          [else (eternity (cdr l))]))
)



; function to determine length of list w/1 or fewer items
(define length-1-or-less
(lambda (l)
  (cond [(null? l) 0]
        [else (lt-sc:my-add1 (length-0 (cdr l)))]))
)
; now, replace length-0 with its definition in length-1-or-less
(lambda (l)
  (cond [(null? l) 0]
        [else (lt-sc:my-add1 ((lambda (l)
                       (cond [(null? l) 0]
                             [else (eternity (cdr l))])) (cdr l)))]))

; Now, a function to get the length of a list w/2 or fewer items

(define length-2-or-less
  (lambda (l)
    (cond [(null? l) 0]
          [else (lt-sc:my-add1 (length-1-or-less (cdr l)))])
))

; for some reason, theirs do not work in racket
; which might make this harder to follow
(lambda (l)
  (cond [(null? 0)]
        [else (lt-sc:my-add1 
               (lambda (l)
                 (cond [(null? l) 0]
                       [else (lt-sc:my-add1 ((lambda (l)
                                      (cond [(null? l) 0]
                                            [else (eternity (cdr l))])) (cdr l)))]))
               (cdr l))]))

; a function that looks like length, again
((lambda (lengthx)
   (lambda (l)
     (cond [(null? l) 0]
           [else (lt-sc:my-add1 (lengthx (cdr l)))]))) 
 eternity)

; now define length-1-or-less
(define length-1-or-less-02
((lambda (f)
   (lambda (l)
     (lt-sc:display-all "length-1-or-less-02 with f")
     (cond [(null? l) 0]
           [else (lt-sc:my-add1 (f (cdr l)))])))
 ((lambda (g)
    (lambda (l)
      (lt-sc:display-all "length-1-or-less-02 with g")
      (cond [(null? l) 0]
            [else (lt-sc:my-add1 (g (cdr l)))])))
  eternity))
)

; now length-2-or-less-02
(define length-2-or-less-02
  ((lambda (lengthx)
     ; putting display-all here does not help
     ; and lengthx is a procedure, so don't bother printing that
     (lambda (l)
       (lt-sc:display-all "In lambda 01 for length-2-or-less-02 w/ l: " l)
       (cond ((null? l) 0)
             (else (lt-sc:my-add1 (lengthx (cdr l)))))))
   ((lambda (lengthx)
      (lambda (l)
        (lt-sc:display-all "In lambda 02 for length-2-or-less-02 w/ l: " l)
        (cond ((null? l) 0)
              (else (lt-sc:my-add1 (lengthx (cdr l)))))))
    ((lambda (lengthx)
       (lambda (l)
         (lt-sc:display-all "In lambda 03 for length-2-or-less-02 w/ l: " l)
         (cond ((null? l) 0)
               (else (lt-sc:my-add1 (lengthx (cdr l)))))))
     eternity)))
)
; I guess Racket starts at the first lambda
; and just gives (cdr l) to the next lambda below until it runs out

; page 163:
; Remove the repetition by 
; "Name the function that takes length as an argument 
; and that returns a function that looks like length."
; They call it "mk-length
(define mk-length-0
((lambda (mk-length)
   (lt-sc:display-all "starting mk-length-0") ; does not print
   (mk-length eternity))
 (lambda (lengthx)
   (lt-sc:display-all "in mk-length-0 lengthx") ; does not print
   (lambda (l)
     (lt-sc:display-all "in mk-length-0 lengthx lambda") ; prints
     (cond [(null? l) 0]
           [else (lt-sc:my-add1 (lengthx (cdr l)))]))))
)
; you can call this on an empty list

; mk-length for a list <= 1 items
((lambda (mk-length)
   (mk-length
    (mk-length eternity)))
 (lambda (length)
   (lambda (l)
     (cond [(null? l) 0]
           [else (lt-sc:my-add1 (length (cdr l)))])))) 

; mk-length for a list <= 2 items
((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length eternity))))
 (lambda (length)
   (lambda (l)
     (cond [(null? l) 0]
           [else (lt-sc:my-add1 (length (cdr l)))]))))

; mk-length for a list <= 3 items
(define mk-length-3-or-less
  ((lambda (mk-length)
     (mk-length ; list w/3 items?
       (mk-length ; list w/2 items?
         (mk-length ; list w/1 item?
           (mk-length eternity))))) ; empty list?
   (lambda (lengthx)
     (lambda (l)
       (lt-sc:display-all "in mk-length-3-or-less lengthx lambda with l " l) ; prints
       (cond [(null? l) 0]
             [else (lt-sc:my-add1 (lengthx (cdr l)))]))))
)
; this is very inefficient

; mk-length-0 without a call to eternity - page 165
(define mk-length-0-02
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond [(null? l) 0]
           [else (lt-sc:my-add1 (mk-length (cdr l)))]))))
)
; Page 166: the first argument to mk-length is mk-length

; length for <= 1 again
(define mk-length-1-02
  ((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (lambda (l)
       (cond [(null? l) 0]
             [else (lt-sc:my-add1 ((mk-length eternity)
                          (cdr l)))]))))
)
; note for page 166: (mk-length-1-02 '(apples)) result: 1

; this works, not clear why
; From book: It keeps adding recursive uses by passing
; mk-length to itself, just as it is about to expire.
(define new-mk-length
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond [(null? l) 0]
           [else (lt-sc:my-add1 ((mk-length mk-length) (cdr l)))]))))
)
#|
(new-mk-length '(a b c d e))
in lambda of new-mk-length w/ l: (a b c d e)
in lambda of new-mk-length w/ l: (b c d e)
in lambda of new-mk-length w/ l: (c d e)
in lambda of new-mk-length w/ l: (d e)
in lambda of new-mk-length w/ l: (e)
in lambda of new-mk-length w/ l: ()
5
|#

; now they re-write that: 
; We could extract this new application of mk-length to itself & call it length
; I think I will call it lengthx so I know it's not the function "length"
; so they go from this:
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond [(null? l) 0]
           [else (lt-sc:my-add1 ((mk-length mk-length) (cdr l)))]))))
; to this:
#|
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (lengthx)
      (lambda (l)
        (cond [(null? l) 0]
              [else (lt-sc:my-add1 (lengthx (cdr l)))])))
    (mk-length mk-length))))
; For me, this never stops
|#
; (mk-length-p171-01 '(a b c d e))
(define mk-length-p171-01
  ((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (lambda (l)
       (lt-sc:display-all "In mk-length-p171-01 w/l: " l)
       (cond [(null? l) 0]
             [else (lt-sc:my-add1 ((lambda(x)
                            ((mk-length mk-length) x))
                          (cdr l)))]))))
)
; (mk-length-p171-02 '(a b c d e))
(define mk-length-p171-02
  ((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     ((lambda (length)
        (lambda (l)
          (cond [(null? l) 0]
                [else (lt-sc:my-add1 (length (cdr l)))])))
      (lambda (x)
        ((mk-length mk-length) x)))))
)

; extract the function in the box that looks like length and give it a name
; (p172-01 '(a b c d e))
(define p172-01
((lambda (le) ; this part makes length
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
            ((mk-length mk-length) x))))))
 (lambda (lengthx) ; this part looks like length
   (lambda (l)
     (cond [(null? l) 0]
           [else (lt-sc:my-add1 (lengthx (cdr l)))]))))
)

; that top part is the applicative-order Y combinator
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))
; I'm still not sure I get it

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

  ; The crazy begins here
  (runit:check-equal? (length-2-or-less '(a b)) 2)
  (runit:check-equal? (length-2-or-less '(a)) 1)
  (runit:check-equal? (length-2-or-less '()) 0)
  (runit:check-equal? (length-1-or-less-02 '(a)) 1)
  (lt-sc:display-all "About to call length-1-or-less-02 '()")
  (runit:check-equal? (length-1-or-less-02 '()) 0)

  (newline)
  (lt-sc:display-all "Calling length-2-or-less-02 '(a b)")
  (runit:check-equal? (length-2-or-less-02 '(a b)) 2)
  (lt-sc:display-all "Calling (length-2-or-less-02 '(a))")
  (runit:check-equal? (length-2-or-less-02 '(a)) 1)
  (lt-sc:display-all "Calling (length-2-or-less-02 '())")
  (runit:check-equal? (length-2-or-less-02 '()) 0)

  (newline)
  (runit:check-equal? (mk-length-3-or-less '(a b c)) 3)
  (newline)
  (runit:check-equal? (mk-length-3-or-less '(a b)) 2)
  (newline)
  (runit:check-equal? (mk-length-3-or-less '(a)) 1)
  (newline)
  (runit:check-equal? (mk-length-3-or-less '()) 0)

  ; this will never return
  ;(runit:check-equal? (length-1-or-less-02 '(a c)) 2)
  (lt-sc:display-all "about to try new-mk-length")
  (runit:check-equal? (new-mk-length '(a b c d e)) 5)
  (runit:check-equal? (new-mk-length '(a b c d)) 4)
  (runit:check-equal? (new-mk-length '(a b c)) 3)
  (runit:check-equal? (new-mk-length '(a b)) 2)
  (runit:check-equal? (new-mk-length '(a)) 1)
  (runit:check-equal? (new-mk-length '()) 0)

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
  #|
  M Vanier states:  
  "The Y combinator allows us to define recursive functions in 
  computer languages that do not have built-in support for 
  recursive functions, but that do support first-class functions."
  The book gets to that point by trying to make "length" without using "define"
  |#
  (newline)
  (lt-sc:display-all "Done with chapter 09 tests at " (lt-sc:display-date))
)

