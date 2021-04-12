#lang racket/base

(require (prefix-in rb6:  rnrs/base-6)
         (prefix-in ris6: rnrs/io/simple-6)
         (prefix-in ss-sc: "seasoned-schemer.rkt"))

; intersect is like union
; union has every element in each
; intersect has the elements in common
(define (union s1 s2)
  (cond [(null? s1) s2]
        [(ss-sc:member? (car s1) s2) (union (cdr s1) s2)]
        [else (cons (car s1) (union (cdr s1) s2))]))

; tail recursive
(define (intersect s1 s2 outp)
  (cond [(null? s1) (reverse outp)]
        [(ss-sc:member? (car s1) s2) (intersect (cdr s1) 
                                                s2 
                                                (cons (car s1) outp))]
        [else (intersect (cdr s1) s2 outp)]))

(define (intersect-letrec s1 s2)
  (letrec 
      ([IL (lambda (s-1 outp)
             (cond [(null? s-1) (reverse outp)]
                   [(ss-sc:member? (car s-1) s2) (IL (cdr s-1) 
                                                     (cons (car s-1) outp))]
                   [else (IL (cdr s-1) outp)]))])
    (IL s1 '())))

(define (intersectall l-set)
  (cond [(null? l-set) '()] ; do not assume list of sets is not empty 
        [(null? (cdr l-set)) (car l-set)]
        [else (intersect-letrec (car l-set) 
                                (intersectall (cdr l-set)))]))

; hide old intersectall in a new one
(define (intersectall-letrec l-set)
  (letrec ([A (lambda (lset)
                (cond [(null? (cdr lset)) (car lset)]
                      [else (intersect-letrec (car lset) 
                                              (A (cdr lset)))]))])
    (cond [(null? l-set) '()]
          [else (A l-set)])))

; Is letcc the same as let/cc? 
; defined on page 41
(define intersectall-41
  (lambda (lset)
    (ss-sc:display-all "-- In intersectall-41 with lset: " lset)
    (let/cc hop
      (letrec
          ([A (lambda (lset)
                (ss-sc:display-all "In A with lset: " lset)
                (cond [(null? (car lset)) 
                       (begin
                         (ss-sc:display-all "car lset is null, calling hop '()")
                         (hop '()))]
                      [(null? (cdr lset)) 
                       (begin
                         (ss-sc:display-all "cdr lset is null, returning car lset")
                         (car lset)
                         )]
                      [else 
                       (begin
                         (ss-sc:display-all "In else of A, calling intersect-letrec with " (car lset) " and (A " (cdr lset) ")")
                         (intersect-letrec (car lset) (A (cdr lset))))]))])
        (cond [(null? lset) 
               (begin
                 (ss-sc:display-all "In (null? lset), returning '()")
                 '())]
              [else 
               (begin
                 (ss-sc:display-all "In else, calling (A lset)")
                 (A lset))])))))

; Alonzo Church version
(define intersectall-ac
  (lambda (lset)
    (call/cc
     (lambda (hop)
       (letrec
          ([A (lambda (lset)
                 (cond [(null? (car lset)) (hop '())]
                       [(null? (cdr lset)) (car lset)]
                       [else (intersect-letrec (car lset) (A (cdr lset)))]))])
        (cond [(null? lset) '()]
              [else (A lset)]))))))

;; The Fourteenth Commandment: Use (letcc ... ) to return values abruptly and promptly

;; looks correct, but does not work
(define intersectall-49
  (lambda (lset)
    (let/cc hop
      (letrec
          ([A (lambda (lset)
                (cond [(null? (car lset)) (hop '())]
                      [(null? (cdr lset)) (car lset)]
                      [else (I (car lset) (A (cdr lset)))]))]
           [I (lambda (s1 s2)
                (letrec
                    ([J (lambda (s1)
                          (cond [(null? s1) '()]
                                [(ss-sc:member? (car s1) s2) 
                                 (cons (car s1) (J (cdr s1)))]
                                [else (J (cdr s1))]))])
                  (cond [(null? s2) '()]
                        [else (J s1)])))])
        (cond [(null? lset) '()]
              [else (A lset)])))))

;; from https://github.com/pkrumins/the-seasoned-schemer/blob/master/13-hop-skip-and-jump.ss - has conditions of J reversed and it works
(define intersectall-ap
  (lambda (lset)
    (call-with-current-continuation
      (lambda (hop)
        (letrec
            ([A (lambda (lset)
                  (cond [(null? (car lset)) (hop '())]
                        [(null? (cdr lset)) (car lset)]
                        [else (I (car lset) (A (cdr lset)))]))]
             [I (lambda (s1 s2)
                (letrec
                    ([J (lambda (s1)
                          (cond [(null? s1) '()]
                                [(ss-sc:member? (car s1) s2) (cons (car s1) (J (cdr s1)))]
                                [else (J (cdr s1))]))])
                  (cond [(null? s2) (hop '())]
                        [else (J s1)])))])
          (cond [(null? lset) '()]
                [else (A lset)]))))))

; rember w/letrec
(define (rember a lat)
  (cond [(null? lat) '()]
        ; [(eq? (car lat) a) (cdr lat)]
        [(ss-sc:equal5? (car lat) a) (cdr lat)] ; from chapter 5
        [else (cons (car lat) (rember a (cdr lat)))]))

(define (rember-letrec a lat)
  (letrec ([R (lambda (lat2)
                  (cond [(null? lat2) '()]       
                        [(ss-sc:equal5? (car lat2) a) (cdr lat2)] ; from chapter 5
                        [else (cons (car lat2) (R (cdr lat2)))]))])
    (R lat))) 
;; need to remember to have func call just before closing parens in letrec
;; and to remember to put function within the "lambda"

;; rember-beyond-first takes an atom and a list of atoms
;; and removes the first occurance of the atom from the list
;; as well as removing everything from the list after the first occurance 

(define (rember-beyond-first a lat)
  (letrec ([R (lambda (lat)
                  (cond [(null? lat) '()]       
                        [(ss-sc:equal5? (car lat) a) '()] ; from chapter 5
                        [else (cons (car lat) (R (cdr lat)))]))])
    (R lat)))

;; rember-upto-last takes an atom and a list of atoms
;; and removes everything in the list before the last instance of the atom
;; as well as the atom
; we could do this by reversing, but I have a feeling we will use let/cc
; or with a second list

(define (rember-upto-last a lat)
  (let/cc skip
      (letrec
          ([R (lambda (lat)
                (cond [(null? lat) '()]
                      [(ss-sc:equal5? (car lat) a) 
                       (skip (R (cdr lat)))
                       ; (R (cdr lat))
                       ]
                      [else (cons (car lat) (R (cdr lat)))]))])
          (R lat))))

; in tail-recursive version, 
; (skip (R (cdr lat))) can be replaced with (R (cdr lat))
; and it still works
; in non-tail-recursive, it breaks
; recursive, and prints
(define (rember-upto-last-r a lat)
  (ss-sc:display-all "----- in rember-upto-last-r with a: " a)
  (let/cc skip
    (ss-sc:display-all "at skip point")
      (letrec
          ([R (lambda (lat outp)
                (ss-sc:display-all "Calling R w/lat: " lat 
                                   " and outp: " outp)
                (cond [(null? lat) 
                       (begin
                         (ss-sc:display-all "lat is null, returning " 
                                            (reverse outp))
                         (reverse outp))]
                      [(ss-sc:equal5? (car lat) a) 
                       (begin
                         (ss-sc:display-all "car lat is a, calling skip")
                         (skip (R (cdr lat) '()))
                         ;(R (cdr lat) '())
)
                       ]
                      [else (begin
                              (ss-sc:display-all "In else, calling R w/ "
                                                 (cdr lat) " and consing "
                                                 (car lat) " onto " outp
                                                 )
                              (R (cdr lat) (cons (car lat) outp)))]))])
        (R lat '()))))

; prints
(define (rember-upto-last-p a lat)
  (ss-sc:display-all "---------------- Calling rember-upto-last-p w/a: " a)
  (let/cc skip
      (letrec
          ([R (lambda (lat)
                (ss-sc:display-all "* In (R " lat ")")
                (cond [(null? lat) 
                       (begin
                         (ss-sc:display-all "lat is null")
                         '())]
                      [(ss-sc:equal5? (car lat) a) 
                       (begin
                         (ss-sc:display-all "car lat is a: " a
                                            " calling skip (R ("
                                            (cdr lat) "))"
                                            )
                         (skip (R (cdr lat)))
                         ; (R (cdr lat))
                         )]
                      [else 
                       (begin
                         (ss-sc:display-all "In the else, "
                                                 "calling (cons "
                                                 (car lat) " (R "
                                                 (cdr lat) "))"
                                                 )
                         (cons (car lat) (R (cdr lat))))]))])
          (R lat))))
; going to the skip seems to reset it and start over with whatever values
; are "current" at that time
; from book: we eliminate all the pending "cons" calls when we skip
; maybe that's why we could alter the "skip" in the tail-recursive versions
; there were no "cons" calls waiting for data to return


(module+ test
  (require (prefix-in runit: rackunit))
  (runit:check-true #t)
  (ss-sc:display-all "testing chapter 13 of 'The Seasoned Schemer'")
  (runit:check-equal? (intersect '(tomatoes and macaroni) 
                                 '(macaroni and cheese) '())
                      '(and macaroni))
  (runit:check-equal? (intersect-letrec '(tomatoes and macaroni) 
                                        '(macaroni and cheese))
                      '(and macaroni))
  (runit:check-equal? (intersectall '((a b c) 
                                      (c a d e) 
                                      (e f g h a b))) 
                      '(a))
  (runit:check-equal? (intersectall '((6 pears and)
                                      (3 peaches and 6 peppers)
                                      (8 pears and 6 plums)
                                      (and 6 prunes with some apples)))
                      '(6 and))

  (runit:check-equal? (intersectall-letrec '((a b c) 
                                             (c a d e) 
                                             (e f g h a b))) 
                      '(a))
  (runit:check-equal? (intersectall-letrec '((6 pears and)
                                             (3 peaches and 6 peppers)
                                             (8 pears and 6 plums)
                                             (and 6 prunes with some apples)))
                      '(6 and))

  ;; (runit:check-equal? (intersectall-41 '((a b c) 
  ;;                                        (c a d e) 
  ;;                                        (e f g h a b))) 
  ;;                     '(a))
  ;; (runit:check-equal? (intersectall-41 '((6 pears and)
  ;;                                        (3 peaches and 6 peppers)
  ;;                                        (8 pears and 6 plums)
  ;;                                        (and 6 prunes with some apples)))
  ;;                     '(6 and))

  ;; (runit:check-equal? (intersectall-41 '((3 mangoes and) 
  ;;                                        () 
  ;;                                        (3 diet hamburgers))) 
  ;;                     '())

  ;; (runit:check-equal? (intersectall-41 '((3 steaks and) 
  ;;                                        (no food and) 
  ;;                                        (three baked potatoes)
  ;;                                        (3 diet hamburgers))) 
  ;;                     '())

  (runit:check-equal? (intersectall-49 '((3 steaks and) 
                                         (no food and) 
                                         (three baked potatoes)
                                         (3 diet hamburgers))) 
                      '())

  (runit:check-equal? (intersectall-ap '((3 steaks and) 
                                         (no food and) 
                                         (three baked potatoes)
                                         (3 diet hamburgers))) 
                      '())

  (runit:check-equal? (rember-letrec 'mint '(lamb chops and mint jelly)) 
                      '(lamb chops and jelly))
  (runit:check-equal? (rember-letrec 'mint '(lamb chops and mint flavored mint jelly)) 
                      '(lamb chops and flavored mint jelly))
  (runit:check-equal? (rember-letrec 'toast '(bacon lettuce and tomato))
                      '(bacon lettuce and tomato))
  (runit:check-equal? (rember-letrec 'cup '(coffee cup tea cup and hick cup))
                      '(coffee tea cup and hick cup))
  (runit:check-equal? (rember-letrec 'and '(bacon lettuce and tomato))
                      '(bacon lettuce tomato))
  (runit:check-equal? (rember-letrec 'sauce '(soy sauce and tomato sauce))
                      '(soy and tomato sauce))

  (runit:check-equal? (rember-beyond-first 'roots
                                           '(noodles 
                                             spaghetti 
                                             spatzle
                                             bean-thread
                                             roots
                                             potatoes
                                             yam
                                             others
                                             rice))
                      '(noodles
                        spaghetti
                        spatzle
                        bean-thread))

  (runit:check-equal? (rember-beyond-first 'others
                                           '(noodles 
                                             spaghetti 
                                             spatzle
                                             bean-thread
                                             roots
                                             potatoes
                                             yam
                                             others
                                             rice))
                      '(noodles 
                        spaghetti 
                        spatzle
                        bean-thread
                        roots
                        potatoes
                        yam))

  

  (runit:check-equal? (rember-beyond-first 'sweetthing
                                           '(noodles 
                                             spaghetti 
                                             spatzle
                                             bean-thread
                                             roots
                                             potatoes
                                             yam
                                             others
                                             rice))
                      '(noodles 
                        spaghetti 
                        spatzle
                        bean-thread
                        roots
                        potatoes
                        yam
                        others
                        rice))

  (runit:check-equal? (rember-beyond-first 'desserts
                                           '(cookies
                                             chocolate mints
                                             caramel delight ginger snaps
                                             desserts
                                             chocolate moose
                                             vanilla ice cream
                                             German chocolate cake
                                             more desserts
                                             gingerbread man))
                      '(cookies
                        chocolate mints
                        caramel delight ginger snaps))

  (runit:check-equal? (rember-upto-last-p 'roots
                                        '(noodles 
                                          spaghetti 
                                          spatzle
                                          bean-thread
                                          roots
                                          potatoes
                                          yam
                                          others
                                          rice))
                      '(potatoes
                        yam
                        others
                        rice))

  (runit:check-equal? (rember-upto-last 'sweetthing
                                        '(noodles 
                                          spaghetti 
                                          spatzle
                                          bean-thread
                                          roots
                                          potatoes
                                          yam
                                          others
                                          rice))
                      '(noodles 
                        spaghetti 
                        spatzle
                        bean-thread
                        roots
                        potatoes
                        yam
                        others
                        rice))

  (runit:check-equal? (rember-upto-last 'cookies
                                        '(cookies
                                          chocolate mints
                                          caramel delight ginger snaps
                                          desserts
                                          chocolate moose
                                          vanilla ice cream
                                          German chocolate cake
                                          more cookies
                                          gingerbread man))
                      '(gingerbread man))

    (runit:check-equal? (rember-upto-last-p 'cookies
                                            '(cookies
                                              chocolate mints
                                              caramel delight ginger snaps
                                              desserts
                                              cookies
                                              chocolate moose
                                              vanilla ice cream
                                              German chocolate cake
                                              more cookies
                                              gingerbread man))
                      '(gingerbread man))



  (newline)
  (ss-sc:display-all "Done with chapter 13 tests at " (ss-sc:display-date))
)

