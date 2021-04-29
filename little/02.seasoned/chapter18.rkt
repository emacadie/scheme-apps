#lang racket/base

(require (prefix-in rb6:  rnrs/base-6)
         (prefix-in ris6: rnrs/io/simple-6)
         ; mentioned at https://docs.racket-lang.org/r6rs/r6rs-lib-std/r6rs-lib-Z-H-18.html#node_idx_1276
         (prefix-in r6mp: rnrs/mutable-pairs-6)
         ; (prefix-in rlib: r6rs/lib) 
         (prefix-in ss-sc: "seasoned-schemer.rkt")
         (prefix-in uuid: uuid))

(define (kons a b)
  (cons a b))

(define (kdr a)
  (cdr a))

(define (kar a)
  (car a))

(define (lots m)
  (cond [(zero? m) '()]
        [else (kons 'egg (lots (ss-sc:my-sub1 m)))]))

(define (lenkth l)
  (cond [(null? l) 0]
        [else (ss-sc:my-add1 (lenkth (kdr l)))]))

(define konsC
  (let ([N 0])
    (lambda (x y)
      (set! N (ss-sc:my-add1 N))
      (cons x y))))

(define (add-at-end l)
  (cond [(null? (kdr l)) (konsC (kar l) (kons 'egg '()))]
        [else (konsC (kar l) (add-at-end (kdr l)))]))

(define counter (lambda() 0))
(define set-counter (lambda () 0))
(define consC
  (let ((N 0))
    (set! counter (lambda() N))
    (set! set-counter
      (lambda (x) (set! N x)))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))

(define kounter 0)
(define set-kounter 0)
;
(define konsC2
  (let ((N 0))
    (set! kounter (lambda () N))
    (set! set-kounter
          (lambda (x) (set! N x)))
    (lambda (x y)
      (set! N (+ N 1))
      (kons x y))))

(define consC135
  (let ([N 0])
    (set! kounter (lambda () N))
    (set! set-kounter (lambda (x)
                        (set! N x)))
    (lambda (x y)
      (set! N (ss-sc:my-add1 N))
      (cons x y))))


; I can't get this to work; skipping
(define (add-at-end-too l)
  (letrec ([A (lambda (ls)
                (cond [(null? (kdr ls)) (r6mp:set-cdr! ls (kons 'egg '()))]
                      [else (A (kdr ls))]))])
    (A l)
    l))



(module+ test
  (require (prefix-in runit: rackunit))
  (runit:check-true #t)
  
  (ss-sc:display-all "----------------------------------------------\n"
                     "testing chapter 18 of 'The Seasoned Schemer'")
  ; (runit:check-equal? (leftmost '(((a) b) (c d))) 'a)
  (runit:check-equal? (lots 3) '(egg egg egg))
  (runit:check-equal? (lots 5) '(egg egg egg egg egg))
  (runit:check-equal? (lots 12) '(egg egg egg egg egg egg
                                  egg egg egg egg egg egg))

  (runit:check-equal? (lenkth (lots 3)) 3)
  (runit:check-equal? (lenkth (lots 5)) 5)
  (runit:check-equal? (lenkth (lots 12)) 12)

  (runit:check-equal? (add-at-end (lots 3)) '(egg egg egg egg))

  (ss-sc:display-all "Here is kounter: " (kounter) )
  (runit:check-equal? (add-at-end-too (lots 3)) '(egg egg egg egg))

  (newline)
  (ss-sc:display-all "Here is a UUID w/gensym: " (gensym (uuid:uuid-string)))
  (ss-sc:display-all "Done with chapter 18 tests at " (ss-sc:display-date)
                     "\n---------------------------------------------------"))

