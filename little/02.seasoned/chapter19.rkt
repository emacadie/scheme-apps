#lang racket/base

(require (prefix-in rb6:  rnrs/base-6)
         (prefix-in ris6: rnrs/io/simple-6)
         ; mentioned at https://docs.racket-lang.org/r6rs/r6rs-lib-std/r6rs-lib-Z-H-18.html#node_idx_1276
         ; (prefix-in r6mp: rnrs/mutable-pairs-6)
         
         (prefix-in ss-sc: "seasoned-schemer.rkt")
         (prefix-in uuid: uuid))

(define toppings 'cheese)

(define (deepB m)
  (cond [(zero? m) (let/cc jump
                     (set! toppings jump)
                     'pizza)]
        [else (cons (deepB (ss-sc:my-sub1 m)) 
                    '())]))

; The Twentieth Commandment: When thinking about a value created w/ (letcc ...),
; write down the function that is equivalent but does not forget.
; Then, when you use it, remember to forget.

; collectors again, from chapter 8
(define (deepCollector m k)
  (cond [(zero? m) (k 'pizza)]
        [else (deepCollector (ss-sc:my-sub1 m)
                             (lambda (x)
                               (k (cons x '()))))]))

(define (deepCollectorB m k)
  (cond [(zero? m) (let ()
                     (set! toppings k)
                     (k 'pizza))]
        [else (deepCollectorB (ss-sc:my-sub1 m)
                              (lambda (x)
                                (k (cons x '()))))]))

;; page 166: two-in-a-row* is the same as two-in-a-row,
;; except it can look at a list that is not flat,
;; and the atoms do not have to be on the same level to be flagged as consecutive

(define leave '0)

(define (walk l)
  (ss-sc:display-all "calling walk " l)
  (cond [(null? l) '()]
        [(ss-sc:atom? (car l)) (leave (car l))]
        [else
         (begin
           (walk (car l))
           (walk (cdr l)))]))

(define (start-it l)
  (let/cc here
    (set! leave here)
    (ss-sc:display-all "in let/cc, setting leave to " here)
    (walk l)))

; so you can have the continuation in a different function

(define fill (lambda (x) x))
; (define fill 0)

(define (waddle l)
  (cond [(null? l) '()]
        [(ss-sc:atom? (car l))
         (begin
           (let/cc rest
             (set! fill rest)
             (leave (car l)))
           (waddle (car l)))]
        [else (begin
                (waddle (car l))
                (waddle (cdr l)))]))

(define (start-it2 l)
  (let/cc here
    (set! leave here)
    (waddle l)))

(define (get-next x)
  (let/cc here-again
    (set! leave here-again)
    (fill 'go)))

(module+ test
  (require (prefix-in runit: rackunit))
  (runit:check-true #t)
  
  (ss-sc:display-all "----------------------------------------------\n"
                     "testing chapter 19 of 'The Seasoned Schemer'")
  ; (runit:check-equal? (leftmost '(((a) b) (c d))) 'a)
  (runit:check-equal? (deepB 6) '((((((pizza)))))))
  ; (ss-sc:display-all "(toppings 'cake): " (toppings 'cake))
  ; (ss-sc:display-all "toppings: " toppings)
  ; (ss-sc:display-all "here is (cons (toppings 'cake) '()): " (cons (toppings 'cake) '()))
  ; changing toppings changes the return value of deepB, 
  ; even if deepB is called before altering toppings
  (runit:check-equal? (deepCollector 0 (lambda (x) x)) 'pizza)
  (runit:check-equal? (deepCollector 6 (lambda (x) x)) '((((((pizza)))))) )

  (runit:check-equal? (deepCollectorB 4 (lambda (x) x)) '((((pizza)))))
  (ss-sc:display-all "(cons (toppings 'cake) (toppings 'cake)): "
                     (cons (toppings 'cake) (toppings 'cake)))
  (ss-sc:display-all "(start-it '((potato) (chips (chips (with))) fish)): "
                     (start-it '((potato) (chips (chips (with))) fish)))
  

  (ss-sc:display-all "'(start-it2 '((donuts) (cheerios (cheerios (spaghettios))) donuts)): "
                  (start-it2 '((donuts) 
                               (cheerios (cheerios (spaghettios))) 
                               donuts)) )

  (ss-sc:display-all "(get-next 'go): " (get-next 'go))
  (newline)
  (ss-sc:display-all "Here is a UUID w/gensym: " (gensym (uuid:uuid-string)))
  (ss-sc:display-all "Done with chapter 19 tests at " (ss-sc:display-date)
                     "\n---------------------------------------------------"))

