#lang racket/base

(require (prefix-in rb6:  rnrs/base-6)
         (prefix-in ris6: rnrs/io/simple-6)
         (prefix-in srfi-19: srfi/19)
         ; (prefix-in rd:   racket/date)
)


(provide atom?
         display-all
         display-date
         lat?
         member?)

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

; (display-all (rd:date->string the-date "~Y-~m-~d ~H:~M:~S"))

(module+ test
  (require (prefix-in runit: rackunit))
  (runit:check-true #t)
  (runit:check-equal? (rb6:rational-valued? 6/10) #t)
  (display-all "testing " "testing")
  (runit:check-equal? (atom? (quote ())) #f)
  (runit:check-equal? (atom? (rb6:quote ())) #f)
  (runit:check-equal? (atom? '()) #f))


