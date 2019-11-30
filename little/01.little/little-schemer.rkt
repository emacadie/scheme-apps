#lang racket/base

(require (prefix-in rb6:  rnrs/base-6)
         (prefix-in ris6: rnrs/io/simple-6)
         (prefix-in srfi-19: srfi/19)
         ; (prefix-in rd:   racket/date)
)


(provide atom?
         display-all
         display-date
         firsts
         insertL
         insertR
         lat?
         member?
         my-rember
         rember
         subst
         subst2)

; in preface
(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(define (display-all . vs)
  (for-each ris6:display vs)
  (ris6:newline))

(define (display-date)
  (let ([the-date (seconds->date (current-seconds))] )
    (srfi-19:date->string the-date "~Y-~m-~d ~H:~M:~S")))

; defined in chapter 2, after it is used
; lat means list of atoms, I think
(define (lat? l)
  (cond [(null? l) #t]
        [(atom? (car l)) (lat? (cdr l))]
        [else #f]))

; Mine is tail-recursive.
(define (member? the-atom the-list)
  (cond [(null? the-list) #f]
        [(eq? the-atom (car the-list)) #t]
        [else (member? the-atom (cdr the-list))]))

; Chapter Three
(define (rember-helper a lat out-lat)
  (display-all "rember-helper called with " a ", " lat ", " out-lat)
  (cond [(null? lat) out-lat]
        [(eq? a (car lat)) (cons out-lat (cdr lat))]
        [else (rember-helper a (cdr lat) (cons (car lat) out-lat))]))

(define (my-rember a lat)
  (rember-helper a lat '()))

(define (rember a lat)
  (cond
    [(null? lat) (quote ())]
    [(eq? (car lat) a) (cdr lat)]
    [else (cons (car lat) (rember a (cdr lat)))])) 

(define (firsts l)
  (cond [(null? l) '()]
        [else (cons (car (car l)) (firsts (cdr l)))]))

(define (insertR new old lat)
  (cond [(null? lat) '()]
        [(eq? (car lat) old) (cons old 
                                   (cons new  (cdr lat)))]
        [else (cons (car lat) (insertR new old (cdr lat)))]))
; Yo dawg, I heard you like cons-ing lists, so I put a cons in your cons
; so you can add a list to your list.
; I really hate the way they put another "cond" inside their "else" clause.

(define (insertL new old lat)
  (cond [(null? lat) '()]
        [(eq? (car lat) old) (cons new lat)  ]
        [else (cons (car lat) (insertL new old (cdr lat)))]))

; replace old with new
(define (subst new old lat)
  (cond [(null? lat) '()]
        [(eq? (car lat) old) (cons new (cdr lat))]
        [else (cons (car lat) (subst new old (cdr lat)))]))

; replace either o1 or o2 with new
; now we get "or"
(define (subst2 new o1 o2 lat)
  (cond [(null? lat) '()]
        [(or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat))]
        [else (cons (car lat) (subst2 new o1 o2 (cdr lat)))]))

; (display-all (rd:date->string the-date "~Y-~m-~d ~H:~M:~S"))

(module+ test
  (require (prefix-in runit: rackunit))
  (runit:check-true #t)
  (runit:check-equal? (rb6:rational-valued? 6/10) #t)
  (display-all "testing " "testing")
  (runit:check-equal? (atom? (quote ())) #f)
  (runit:check-equal? (atom? (rb6:quote ())) #f)
  (runit:check-equal? (atom? '()) #f))


