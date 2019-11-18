#lang racket/base

(require (prefix-in rb6:  rnrs/base-6)
         (prefix-in ris6: rnrs/io/simple-6)
         
)


(provide atom?
         display-all)

(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

(define (display-all . vs)
  (for-each ris6:display vs)
  (ris6:newline))

(module+ test
  (require (prefix-in runit: rackunit))
  (runit:check-true #t)
  (runit:check-equal? (rb6:rational-valued? 6/10) #t)
  (display-all "testing " "testing")
  (runit:check-equal? (atom? (quote ())) #f)
  (runit:check-equal? (atom? (rb6:quote ())) #f)
  (runit:check-equal? (atom? '()) #f))


