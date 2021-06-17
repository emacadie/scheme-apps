#lang racket/base

(require (prefix-in ck-mk: cKanren/miniKanren)
         (prefix-in rs-sc: "reasoned-schemer.rkt")
         (prefix-in uuid: uuid)
)

(module+ test
  (require (prefix-in runit: rackunit))
  (runit:check-true #t)
  (rs-sc:display-all "----------------------------------------------\n"
                     "testing chapter 1 of 'The Reasoned Schemer'")
  (runit:check-equal? (ck-mk:run* (q) ck-mk:fail) '())
  ;; From page 4: (≡ v w) is read “unify v with w” and ≡ is written == 
  (runit:check-equal? (ck-mk:run* (q) (ck-mk:== #t q)) '(#t))
  (runit:check-equal? (ck-mk:run* (q) ck-mk:fail (ck-mk:== #t q))
                      '())
  ;; In the book, they get just #t for this one
  (runit:check-equal? (ck-mk:run* (q) ck-mk:succeed (ck-mk:== #t q))
                      '(#t))

  (rs-sc:display-all "Here is a UUID w/gensym: " (gensym (uuid:uuid-string)))
  (rs-sc:display-all "Done with chapter 1 tests at " (rs-sc:display-date)
                     "\n---------------------------------------------------")
)


