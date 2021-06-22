#lang racket/base

(require (prefix-in ck-mk: cKanren/miniKanren)
         (prefix-in rs-sc: "reasoned-schemer.rkt")
         (prefix-in uuid: uuid)
         ; (prefix-in r-mk: Racket-miniKanren/miniKanren/mk)
         ; (prefix-in mk: minikanren)
         ; (prefix-in r-match: racket/match)
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
  ;; Maybe that is a typo; some repos say they have a few typos
  (runit:check-equal? (ck-mk:run* (q) ck-mk:succeed (ck-mk:== #t q))
                      '(#t))

  (runit:check-equal? (ck-mk:run* (r) ck-mk:succeed (ck-mk:== 'corn r)) '(corn))
  ;; So ck-mk:run* has three args: a list, a return value, and a call to 
  ;; ck-mk:== (the equal sign w/three lines), the second arg to which is the
  ;; same as one of the elements in the list sent to ck-mk:run*
  ;; ck-mk:run* returns whatever value in the first arg that would make
  ;; the third arg equal to the second arg
  ;; This sounds like "Newt Ginrich's Family Values":
  ;; "Using kids from your first wife to convince everyone your second wife
  ;; is lying about your third wife"
  (runit:check-equal? (ck-mk:run* (r) ck-mk:fail (ck-mk:== 'corn r)) '())
  ;; page 5: they say this is equal to '(#f)
  ;; "#s succeeds and run* returns a non-empty list if its goals succeed"
  ;; per comments on https://www.monolune.com/using-racket-for-the-reasoned-schemer/
  ;; things changed from first to second edition
  ;; Anyway, leave it for now
  (runit:check-equal? (ck-mk:run* (q) ck-mk:succeed (ck-mk:== #f q)) '())

  ;; this works, but it only has two args. When will they explain what run* does?
  (runit:check-equal? (ck-mk:run* (x) 
                                  (let ([x #f])
                                    (ck-mk:== #t x)))
                      '())

  ;; (fresh (x ..) g...) binds fresh variables to x ... and succeeds
  ;; if goals g ... succeed. (ck-mk:== v x) succeeds when x is fresh
  (runit:check-equal? (ck-mk:run* (q)
                                  (ck-mk:fresh (x)
                                               (ck-mk:== #t x)
                                               (ck-mk:== #t q)))
                      '(#t))
  ;; In the intro, they said "fresh" is like lambda

  (runit:check-equal? (ck-mk:run* (q)
                                  (ck-mk:fresh (x)
                                               (ck-mk:== x #t)
                                               (ck-mk:== q #t)))
                      '(#t))

  ;; x is a fresh variable, like the one made with "reify-name" (which is in chapter 9)
  ;; the "x" in (ck-mk:== #t x) is the one from the "fresh" expression
  ;; not the one from "run" or the "lambda"
  (runit:check-equal? (ck-mk:run* (x)
                                  (let ([x #f])
                                    (ck-mk:fresh (x)
                                           (ck-mk:== #t x))))
                      '(_.0))

  (runit:check-equal? (ck-mk:run* (x)
                                  ck-mk:succeed)
                      '(_.0))

  ;; The variables are fresh: shown by underscore followed by a numeric subscript
  ;; I cannot make subscripts here, so I just use numbers
  ;; such a variable has been reified
  (runit:check-equal? (ck-mk:run* (r)
                                  (ck-mk:fresh (x y)
                                               (ck-mk:== (cons x (cons y '())) 
                                                         r)))
                      '((_.0 _.1)))
  ;; up to page 16
  (rs-sc:display-all "Here is a UUID w/gensym: " (gensym (uuid:uuid-string)))
  (rs-sc:display-all "Done with chapter 1 tests at " (rs-sc:display-date)
                     "\n---------------------------------------------------")
)


