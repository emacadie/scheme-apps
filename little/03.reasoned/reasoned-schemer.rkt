#lang racket/base

(require (prefix-in rb6:  rnrs/base-6)
         (prefix-in ris6: rnrs/io/simple-6)
         ; (prefix-in rd:   racket/date)
         (prefix-in srfi-19: srfi/19))


(provide display-all
         display-date
)

(define (display-all . vs)
  (for-each ris6:display vs)
  (ris6:newline))

(define (display-date)
  (let ([the-date (seconds->date (current-seconds))] )
    (srfi-19:date->string the-date "~Y-~m-~d ~H:~M:~S")))

