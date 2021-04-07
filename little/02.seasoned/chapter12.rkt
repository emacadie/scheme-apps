#lang racket/base

(require (prefix-in rb6:  rnrs/base-6)
         (prefix-in ris6: rnrs/io/simple-6)
         (prefix-in ss-sc: "seasoned-schemer.rkt"))

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

(define multirember-Y
  (lambda (a lat)
    ((Y (lambda (mr)
          (lambda (lat)
            (cond [(null? lat) '()]
                  [(eq? a (car lat)) (mr (cdr lat))]
                  [else (cons (car lat) (mr (cdr lat)))]))))
     lat)))

(define length-Y 
  (Y (lambda (length)
       (lambda (l)
         (cond [(null? l) 0]
               [else (add1 (length-Y (cdr l)))])))))

(define multirember-letrec
  (lambda (a lat)
    (ss-sc:display-all "In multirember-letrec")
    ((letrec 
         ((mr (lambda (lat)
                (cond [(null? lat) '()]
                      [(eq? a (car lat)) (mr (cdr lat))]
                      [else (cons (car lat) (mr (cdr lat)))]))))
       mr)
     lat)))

; I don't think I ever quite got letrec.
; I got the exercises done in prior Scheme tutorials, but my answers were more lines
; I guess it is useful if you want to define a function once.
; from page 22
(define multirember-letrec-22
  (lambda (a lat)
    (ss-sc:display-all "In multirember-letrec-22")
    (letrec 
         ((mr (lambda (lat)
                (cond [(null? lat) '()]
                      [(eq? a (car lat)) (mr (cdr lat))]
                      [else (cons (car lat) (mr (cdr lat)))]))))
      (mr lat))))

; The Twelfth Commandment: Use (letrec ...) to remove arguments that do not change for recursive applications.
; It just seems like a lot of extra work when you can just make a cond in a recursive function.

;; define multirember-f which takes a function like rember-f
;; This is a VERY convoluted way to do filter
;; Cannot do something like rb6:even? on this, only comparing values
(define multirember-f
  (lambda (test?)
    ; (ss-sc:display-all "In multirember-f")
    (lambda (a lat)
      (cond [(null? lat) '()]
            [(test? (car lat) a) ((multirember-f test?)  a (cdr lat))]
            [else (cons (car lat) ((multirember-f test?) a (cdr lat)))]))))
; in my own words:
;; multirember-f takes a test, an element and a list, and recursively calls
;; itself moving through the list, removing each element if 
;; the element in the list is true for the test
;; their words: it accepts a test function and returns a new function
;; the new function takes an atom and a list, and removes any atom in list
;; for which the test is true
;; Okay, I get that. But it just feels like an extra step. 
;; I am not clear what I am supposed to get out of this.
;; Maybe if you get already get recursion all of this seems wasteful. maybe???

(define multirember-f-letrec
  (lambda (test?)
    (letrec
        ([m-f
          (lambda (a lat)
            (cond [(null? lat) '()]
                  [(test? (car lat) a) (m-f a (cdr lat))]
                  [else (cons (car lat) (m-f a (cdr lat)))]))])

     m-f)))

(define member-letrec?
  (lambda (a lat)
    (letrec
        ([yes? (lambda (l)
                 (cond [(null? l) #f]
                       [(eq? (car l) a) #t]
                       [else (yes? (cdr l))]))])
      (yes? lat))))

; from Little Schemer
(define (union s1 s2)
  (cond [(null? s1) s2]
        [(ss-sc:member? (car s1) s2) (union (cdr s1) s2)]
        [else (cons (car s1) (union (cdr s1) s2))]))

(define (union-letrec s1 s2)
  (letrec
      ([U (lambda (set-1)
            (cond [(null? set-1) s2]
                  [(ss-sc:member? (car set-1) s2) (U (cdr set-1))]
                  [else (cons (car set-1) (U (cdr set-1)))]))])
    (U s1)))

;; This might be one place where all the parens really IS a bad idea
;; in my own words how new version of union works
; returns a function that performs union on a set w/another set
; U knows about s2 since U is defined w/letrec, it knows what union-letrec knows
; The example in the Scheme docs shows 2 inline functions that call each other
; Is that the best use of letrec?
; They do not seem to be doing that here.

; union w/letrec and internal member
; since letrec can be used to define multiple functions
; is this sort of like anonymous functions?
(define (union-letrec-2 s1 set2)
  (letrec
      ([U (lambda (set-1)
            (cond [(null? set-1) set2]
                  [(M? set2 (car set-1)) (U (cdr set-1))]
                  [else (cons (car set-1) (U (cdr set-1)))]))]
       [M? (lambda (lat a)
             (cond [(null? lat) #f]
                   [(eq? (car lat) a) #t]
                   [else (M? (cdr lat) a)]))])
    (U s1)))


;; The Thirteenth Commandment: Use (letrec ...) to hide and protect functions

(define (union-letrec-3 s1 set2)
  (letrec
      ([U (lambda (set-1)
            (cond [(null? set-1) set2]
                  [(M? set2 (car set-1)) (U (cdr set-1))]
                  [else (cons (car set-1) (U (cdr set-1)))]))]
       [M? (lambda (lat a)
             (letrec 
                 ([N? (lambda (lat)
                        (cond [(null? lat) #f]
                              [(eq? (car lat) a) #t]
                              [else (N? (cdr lat))]))])
               (N? lat)))])
    (U s1)))
; this is a LOT of parens to keep track of
; I had to go to https://github.com/pkrumins/the-seasoned-schemer/blob/master/12-take-cover.ss
; to get mine to work
; The call to the internal function is just before close of letrec

(define two-in-a-row-page33 
  (lambda (lat)
    (letrec
        ([W (lambda (a lat)
              (cond [(null? lat) #f]
                    [else (or (ss-sc:equal5? (car lat) a) 
                              (W (car lat) (cdr lat)))]))])
      (cond [(null? lat) #f]
            [else (W (car lat) (cdr lat))]))))

(define two-in-a-row-page34
  (letrec
      ((W (lambda (a lat)
            (cond [(null? lat) #f]
                  [else (or (ss-sc:equal5? (car lat) a) 
                            (W (car lat) (cdr lat)))]))))
    (lambda (lat)
      (cond [(null? lat) #f]
            [else (W (car lat) (cdr lat))]))))


; now theirs
; sonssf: sum of numbers seen so far
(define (sum-of-prefixes-b sonssf tup)
  (ss-sc:display-all "sum-of-prefixes-b with sonssf: " sonssf ", and tup: " tup)
  (cond [(null? tup) '()]
        [else (cons (+ sonssf (car tup))
                    (sum-of-prefixes-b (+ sonssf (car tup)) 
                                       (cdr tup)))]))

(define (sum-of-prefixes-34 tup)
  (letrec ([SOP (lambda (sonssf tup-1)
                  (cond [(null? tup-1) '()]
                      [else (cons (+ sonssf (car tup-1))
                                  (SOP (+ sonssf (car tup-1)) 
                                       (cdr tup-1)))]))])
    (SOP 0 tup))) ;; this goes right inside end of letrec


; So I guess letrec can be useful if you want to make a function tail-recursive
; and don't want to make a helper
;; recursive
(define (my-sum-of-prefixes tup)
  (letrec ([SOP2 (lambda (in-tup ttl out-tup)
                   (cond [(null? in-tup) (reverse out-tup)]
                         [else (SOP2 (cdr in-tup) 
                                     (+ (car in-tup) ttl)
                                     (cons (+ (car in-tup) ttl) 
                                           out-tup))]))])
    (SOP2 (cdr tup) (car tup) (list (car tup)))))

(module+ test
  (require (prefix-in runit: rackunit))
  (runit:check-true #t)
  (ss-sc:display-all "testing chapter 12 of 'The Seasoned Schemer'")

  (runit:check-equal? (ss-sc:multirember 'tuna 
                                         '(shrimp salad tuna salad and tuna)) 
                      '(shrimp salad salad and))

  (runit:check-equal? (multirember-Y 'tuna 
                                     '(shrimp salad tuna salad and tuna)) 
                      '(shrimp salad salad and))
  (runit:check-equal? (multirember-letrec 'tuna 
                                          '(shrimp salad tuna salad and tuna)) 
                      '(shrimp salad salad and))
  (runit:check-equal? (multirember-letrec-22 'tuna 
                                             '(shrimp salad tuna salad and tuna)) 
                      '(shrimp salad salad and))
  (runit:check-equal? ((multirember-f rb6:=) 5  '(1 5 2 5 3 4 5 6 7)) 
                      '(1 2 3 4 6 7))
  (runit:check-equal? (union '(tomatoes and macaroni casserole)
                             '(macaroni and cheese))
                      '(tomatoes casserole macaroni and cheese))
  (ss-sc:display-all "About to try union-letrec")
  (runit:check-equal? (union-letrec '(tomatoes and macaroni casserole)
                                     '(macaroni and cheese) )
                      '(tomatoes casserole macaroni and cheese))
  (runit:check-equal? (union-letrec-2 '(tomatoes and macaroni casserole)
                                      '(macaroni and cheese) )
                      '(tomatoes casserole macaroni and cheese))
  (runit:check-equal? (union-letrec-3 '(tomatoes and macaroni casserole)
                                      '(macaroni and cheese) )
                      '(tomatoes casserole macaroni and cheese))

  (runit:check-equal? (two-in-a-row-page33 '(wash adams jeff monroe)) 
                      #f)
  (runit:check-equal? (two-in-a-row-page33 '(wash adams jeff jeff monroe)) 
                      #t)
  (runit:check-equal? (two-in-a-row-page34 '(wash adams jeff monroe)) 
                      #f)
  (runit:check-equal? (two-in-a-row-page34 '(wash adams jeff jeff monroe)) 
                      #t)
  (runit:check-equal? (sum-of-prefixes-34 '(2 1 9 17 0))
                      '(2 3 12 29 29))
  (runit:check-equal? (sum-of-prefixes-34 '(1 1 1 1 1))
                      '(1 2 3 4 5))
  (runit:check-equal? (my-sum-of-prefixes '(2 1 9 17 0))
                      '(2 3 12 29 29))
  (runit:check-equal? (my-sum-of-prefixes '(1 1 1 1 1))
                      '(1 2 3 4 5))
    
  (newline)
  (ss-sc:display-all "Done with chapter 12 tests at " (ss-sc:display-date))
)
