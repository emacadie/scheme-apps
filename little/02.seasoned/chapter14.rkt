#lang racket/base

(require (prefix-in rb6:  rnrs/base-6)
         (prefix-in ris6: rnrs/io/simple-6)
         (prefix-in ss-sc: "seasoned-schemer.rkt"))

;; page 56
(define (leftmost l)
  (cond [(null? l) '()]
        [(ss-sc:atom? (car l)) (car l)]
        [else (cond [(ss-sc:atom? (leftmost (car l))) (leftmost (car l))]
                    [else (leftmost (cdr l))])]))

; with "let" to reduce repetition
(define (leftmost-66 l)
  (cond [(null? l) '()]
        [(ss-sc:atom? (car l)) (car l)]
        [else (let ([a (leftmost (car l))])
                (cond [(ss-sc:atom? a) a]
                      [else (leftmost (cdr l))]))]))

(define (rember1* a l)
  (cond [(null? l) '()]
        [(ss-sc:atom? (car l))
         (cond [(ss-sc:equal5? (car l) a) (cdr l)]
               [else (cons (car l) (rember1* a (cdr l)))])]
        [else 
         (cond [(ss-sc:eqlist5? (rember1* a (car l)) (car l)) 
                (cons (car l) (rember1* a (cdr l)))]
               [else (cons (rember1* a (car l)) (cdr l))])]))

(define (rember1-letrec a l)
  (letrec 
      ([R (lambda (l)
            (cond [(null? l) '()]
                  [(ss-sc:atom? (car l))
                   (cond [(ss-sc:equal5? (car l) a) (cdr l)]
                         [else (cons (car l) (R (cdr l)))])]
                  [else 
                   (cond [(ss-sc:eqlist5? (R (car l)) (car l)) 
                          (cons (car l) (R (cdr l)))]
                         [else (cons (R (car l)) (cdr l))])]))])
    (R l)))

; we still have repetition
; rember1* will remove an atom from a list
; even if it is in a sublist

; with let
(define (rember1-68 a l)
  (letrec 
      ([R (lambda (l)
            ; (ss-sc:display-all "Calling R w/l: " l)
            (cond [(null? l) '()]
                  [(ss-sc:atom? (car l))
                   (cond [(ss-sc:equal5? (car l) a) (cdr l)]
                         [else (cons (car l) (R (cdr l)))])]
                  [else 
                   (cond [(ss-sc:eqlist5? (R (car l)) (car l)) 
                          (cons (car l) (R (cdr l)))]
                         [else (cons (R (car l)) (cdr l))])])
            ;; (let ([Rcdr (R (cdr l))]
            ;;       [Rcar (R (car l))])
            ;;   )
)])
    (R l)))

; from the other guy's github
; Did I just put the "let" in the wrong place? Mine is around whole "cond"
;; (define (rember1-68 a l)
;;   (letrec 
;;       ([R (lambda (l)
;;             (cond [(null? l) '()]
;;                   [(ss-sc:atom? (car l))
;;                    (cond [(ss-sc:equal5? (car l) a) (cdr l)]
;;                          [else (cons (car l) (R (cdr l)))])]
;;                   [else
;;                    (let ([av (R (car l))]
;;                          [cv (R (cdr l))])
;;                      (cond [(ss-sc:eqlist5? (car l) av) 
;;                             (cons (car l) cv)]
;;                            [else (cons av (cdr l))]))]))])
;;     (R l)))

(define rember1*-letrec-let
  (lambda (a l)
    (letrec
      ((R (lambda (l)
            (cond
              [(null? l) '()]
              [(ss-sc:atom? (car l))
               (cond [(eq? (car l) a) (cdr l)]
                     [else (cons (car l) (R (cdr l)))])]
              [else
               (let ([av (R (car l))])
                  (cond [(equal? (car l) av) (cons (car l) (R (cdr l)))] 
                        ; if the list w/'a' removed didn't change,then recurse
                        [else (cons av (cdr l))]))]))))  ; otherwise remove 'a'
      (R l))))

; Fifteenth Commandment (preliminary version)
; Use (let ...) to name values of repeated expressions

(define (depth l)
  (cond [(null? l) 1]
        [(ss-sc:atom? (car l)) (depth (cdr l))]
        [else (cond [(> (depth (cdr l)) 
                        (ss-sc:my-add1 (depth (car l)))) 
                     (depth (cdr l))]
                    [else (ss-sc:my-add1 (depth (car l)))])]))

;; (define (depth-let l)
;;   (let ([dcdr (depth-let (cdr l))]
;;         [dcar (depth-let (car l))])
;;     (cond [(null? l) 1]
;;         [(ss-sc:atom? (car l)) (depth-let (cdr l))]
;;         [else (cond [(> (depth-let (cdr l)) 
;;                         (ss-sc:my-add1 (depth-let (car l)))) 
;;                      (depth-let (cdr l))]
;;                     [else (ss-sc:my-add1 (depth-let (car l)))])])))

; for some reason, Racket does not allow using "let" surrounding the outer cond
;; only in the inner cond
; they explain it on page 71:
; it fails because sometimes l will come in as an empty list
; The expressions in the let are evaluated every time, 
; so it will blow up on (cdr l)
; I guess I just thought it was magic
(define (depth-let l)
  (cond [(null? l) 1]
        [(ss-sc:atom? (car l)) (depth-let (cdr l))]
        [else 
         (let ([dcdr (depth-let (cdr l))]
               [adcar (ss-sc:my-add1 (depth-let (car l)))])
           (cond [(> dcdr adcar) dcdr]
                 [else adcar]))]))

(define (depth-let-73 l)
  (cond [(null? l) 1]
        [else 
         (let ([dcdr (depth-let-73 (cdr l))])
           (cond [(ss-sc:atom? (car l)) dcdr]
                 [else 
                  (let ([adcar (ss-sc:my-add1 (depth-let-73 (car l)))])
                    (cond [(> dcdr adcar) dcdr]
                          [else adcar]))]))]))

;; The Fifteenth Commandment: Use (let ...) to name the values of repeated
;; expressions in a function definition if they may be evaluated twice
;; for one and the same use of the function

; the cleanest depth yet
(define (depth-75 l)
  (cond [(null? l) 1]
        [(ss-sc:atom? (car l)) (depth-75 (cdr l))]
        [else (max (ss-sc:my-add1 (depth-75 (car l)))
                   (depth-75 (cdr l)))]))

; let's improve scramble w/ let
; for reference
(define (scramble tup)
  (letrec 
      ([P 
        (lambda (tup rev-pre)
          (cond [(null? tup) '()]
                [else
                 (cons (ss-sc:pick (car tup) 
                                   (cons (car tup) 
                                         rev-pre))
                       (P (cdr tup) 
                          (cons (car tup) 
                                rev-pre)))]))])
    (P tup '())))

(define (scramble-let tup)
  (letrec 
      ([P 
        (lambda (tup rev-pre)
          (cond [(null? tup) '()]
                [else
                 (let ([conscartup (cons (car tup) rev-pre)])
                   (cons (ss-sc:pick (car tup) conscartup)
                       (P (cdr tup) conscartup)))]))])
    (P tup '())))

; redefing leftmost w/let-cc
(define (leftmost-78 l)
  (let/cc skip (lm l skip)))

(define (lm l out)
  (cond [(null? l) '()]
        [(ss-sc:atom? (car l)) (out (car l))] ; is this where the skip happens?
        [else (let () ; could also use begin
                (lm (car l) out)
                (lm (cdr l) out))]))

(define (leftmost-letrec l)
  (letrec ([lm (lambda (l out)
                 (cond [(null? l) '()]
                       [(ss-sc:atom? (car l)) (out (car l))] 
                       [else (let () ; could also use begin
                               (lm (car l) out)
                               (lm (cdr l) out))]))])
    (let/cc skip (lm l skip))))

(define (leftmost-letcc l)
  ; "out" above never really changes, it is always "skip"
  ; and then we can remove it from definition of lm by putting letrec in let/cc
  ; since we already have "skip"
  (let/cc skip 
    (letrec ([lm (lambda (l)
                 (cond [(null? l) '()]
                       [(ss-sc:atom? (car l)) (skip (car l))] 
                       [else (let () ; could also use begin
                               (lm (car l))
                               (lm (cdr l)))]))])
      (lm l))))

;; Their explanation (roughly)
;; Sets up a North Pole w/skip
;; calls lm
;; lm looks at each atom in list l from left to right
;; when it finds an atom if uses skip to return it "abruptly and promptly"
; up to page 83

(module+ test
  (require (prefix-in runit: rackunit))
  (runit:check-true #t)
  (ss-sc:display-all "----------------------------------------------\n"
                     "testing chapter 14 of 'The Seasoned Schemer'")
  (runit:check-equal? (leftmost '(((a) b) (c d)))
                      'a)
  (runit:check-equal? (leftmost '(((a) ()) () (e)))
                      'a)
  (runit:check-equal? (leftmost '(((() a) ())))
                      'a)

  (runit:check-equal? (leftmost-66 '(((a) b) (c d)))
                      'a)
  (runit:check-equal? (leftmost-66 '(((a) ()) () (e)))
                      'a)
  (runit:check-equal? (leftmost-66 '(((() a) ())))
                      'a)
  (runit:check-equal? (rember1* 'salad '((Swedish rye) 
                                         (French (mustard salad turkey))
                                         salad))
                      '((Swedish rye) 
                        (French (mustard turkey))
                        salad))

  (runit:check-equal? (rember1-letrec 'salad '((Swedish rye) 
                                               (French (mustard salad turkey))
                                               salad))
                      '((Swedish rye) 
                        (French (mustard turkey))
                        salad))

  (runit:check-equal? (rember1-68 'salad '((Swedish rye) 
                                               (French (mustard salad turkey))
                                               salad))
                      '((Swedish rye) 
                        (French (mustard turkey))
                        salad))

  (runit:check-equal? (depth '((pickled) peppers (peppers pickled)))
                      2)
  (runit:check-equal? (depth '(margarine 
                               ((bitter butter) 
                                (makes)
                                (batter (bitter))) butter))
                      4)
  (runit:check-equal? (depth '(c (b (a b) a) a))
                      3)

  (runit:check-equal? (depth-let '((pickled) peppers (peppers pickled)))
                      2)
  (runit:check-equal? (depth-let '(margarine 
                               ((bitter butter) 
                                (makes)
                                (batter (bitter))) butter))
                      4)
  (runit:check-equal? (depth-let '(c (b (a b) a) a))
                      3)

  (runit:check-equal? (depth-let-73 '((pickled) peppers (peppers pickled)))
                      2)
  (runit:check-equal? (depth-let-73 '(margarine 
                               ((bitter butter) 
                                (makes)
                                (batter (bitter))) butter))
                      4)
  (runit:check-equal? (depth-let-73 '(c (b (a b) a) a))
                      3)

  (runit:check-equal? (depth-75 '((pickled) peppers (peppers pickled)))
                      2)
  (runit:check-equal? (depth-75 '(margarine 
                               ((bitter butter) 
                                (makes)
                                (batter (bitter))) butter))
                      4)
  (runit:check-equal? (depth-75 '(c (b (a b) a) a))
                      3)

  (runit:check-equal? (scramble '(1 1 1 3 4 2 1 1 9 2))
                      '(1 1 1 1 1 4 1 1 1 9))
  (runit:check-equal? (scramble '(1 2 3 4 5 6 7 8 9))
                      '(1 1 1 1 1 1 1 1 1))
  (runit:check-equal? (scramble '(1 2 3 1 2 3 4 1 8 2 10))
                      '(1 1 1 1 1 1 1 1 2 8 2))
  (runit:check-equal? (scramble-let '(1 1 1 3 4 2 1 1 9 2))
                      '(1 1 1 1 1 4 1 1 1 9))
  (runit:check-equal? (scramble-let '(1 2 3 4 5 6 7 8 9))
                      '(1 1 1 1 1 1 1 1 1))
  (runit:check-equal? (scramble-let '(1 2 3 1 2 3 4 1 8 2 10))
                      '(1 1 1 1 1 1 1 1 2 8 2))

  (runit:check-equal? (leftmost-78 '(((a) b) (c d)))
                      'a)
  (runit:check-equal? (leftmost-78 '(((a) ()) () (e)))
                      'a)
  (runit:check-equal? (leftmost-78 '(((() a) ())))
                      'a)

  (runit:check-equal? (leftmost-letrec '(((a) b) (c d)))
                      'a)
  (runit:check-equal? (leftmost-letrec '(((a) ()) () (e)))
                      'a)
  (runit:check-equal? (leftmost-letrec '(((() a) ())))
                      'a)

  (runit:check-equal? (leftmost-letcc '(((a) b) (c d)))
                      'a)
  (runit:check-equal? (leftmost-letcc '(((a) ()) () (e)))
                      'a)
  (runit:check-equal? (leftmost-letcc '(((() a) ())))
                      'a)

  (newline)
  (ss-sc:display-all "Done with chapter 14 tests at " (ss-sc:display-date)
                     "\n---------------------------------------------------")
)

