#lang racket/base

(require (prefix-in rb6:  rnrs/base-6)
         (prefix-in ris6: rnrs/io/simple-6)
         (prefix-in ss-sc: "seasoned-schemer.rkt")
         (prefix-in uuid: uuid))

(define (find117 n Ns Rs)
  (letrec ([A (lambda (ns rs)
                (cond [(null? ns) #f] 
                      [(ss-sc:equal5? (car ns) n) (car rs)]
                      [else (A (cdr ns) (cdr rs))]))])
    (A Ns Rs)))

; first version
(define (deep127 m)
  (cond [(rb6:zero? m) 'pizza]
        [else (cons (deep127 (ss-sc:my-sub1 m)) '())]))

(define deepM127
  (let ([Rs '()]
        [Ns '()])
    (letrec ([D (lambda (m)
                  (if (rb6:zero? m)
                      'pizza
                      (cons (D (ss-sc:my-sub1 m)) '())))])
      (lambda (n)
        (let ([exists (find117 n Ns Rs)])
        (if (ss-sc:atom? exists)
            (let ([result (D n)])
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists))))))

; D should refer to deepM instead of itself
(define deepM127b
  (let ([Rs '()]
        [Ns '()])
    (letrec ([D (lambda (m)
                  (if (rb6:zero? m)
                      'pizza
                      (cons (deepM127b (ss-sc:my-sub1 m)) '())))])
      (lambda (n)
        (let ([exists (find117 n Ns Rs)])
        (if (ss-sc:atom? exists)
            (let ([result (D n)])
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists))))))

; since D does not refer to itself, no need for letrec
(define deepM128
  (let ([Rs '()]
        [Ns '()])
    (let ([D (lambda (m)
               (if (rb6:zero? m)
                   'pizza
                   (cons (deepM128 (ss-sc:my-sub1 m)) '())))])
      (lambda (n)
        (let ([exists (find117 n Ns Rs)])
        (if (ss-sc:atom? exists)
            (let ([result (D n)])
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists))))))

; we only need one let, since Ns and Rs are not in definition of D
(define deepM128b
  (let ([Rs '()]
        [Ns '()]
        [D (lambda (m)
             (if (rb6:zero? m)
                 'pizza
                 (cons (deepM128b (ss-sc:my-sub1 m)) '())))])
    (lambda (n)
      (let ([exists (find117 n Ns Rs)])
        (if (ss-sc:atom? exists)
            (let ([result (D n)])
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

; but we only call D once, so why not replace it in the let where it is called?
; Because this is honestly easier to read, but that's just me
(define deepM129
  (let ([Rs '()]
        [Ns '()])
    (lambda (n)
      (let ([exists (find117 n Ns Rs)])
        (if (ss-sc:atom? exists)
            (let ([result ((lambda (m)
             (if (rb6:zero? m)
                 'pizza
                 (cons (deepM129 (ss-sc:my-sub1 m)) '()))) 
                           n)])
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

(define deepM129b
  (let ([Rs '()]
        [Ns '()])
    (lambda (n)
      (let ([exists (find117 n Ns Rs)])
        (if (ss-sc:atom? exists)
            (let ([result (let ([m n])
                            (if (rb6:zero? m)
                                'pizza
                                (cons (deepM129b (ss-sc:my-sub1 m)) '())))])
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

; let's get rid of one of those "let" calls
(define deepM130
  (let ([Rs '()]
        [Ns '()])
    (lambda (n)
      (let ([exists (find117 n Ns Rs)])
        (if (ss-sc:atom? exists)
            (let ([result (if (rb6:zero? n)
                                'pizza
                                (cons (deepM130 (ss-sc:my-sub1 n)) '()))])
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

(define consC
  (let ([N 0])
    (lambda (x y)
      (set! N (ss-sc:my-add1 N))
      (cons x y))))

(define (consC2 x y)
  (let ([N 0])
    (set! N (ss-sc:my-add1 N))
    (cons x y)))

(define counter 0) ; needs initial value

(define consC132 
  (let ([N 0])
    (set! counter (lambda () N))
    (lambda (x y)
      (set! N (ss-sc:my-add1 N))
      (cons x y))))

;; (define consC132
;;   (let ([N 0])
;;     (set! counter (lambda() N))
;;     (lambda (x y)
;;       (set! N (ss-sc:my-add1 N))
;;       (cons x y))))
        

(define (deep132 m)
  (cond [(rb6:zero? m) 'pizza]
        [else (consC132 (deep132 (ss-sc:my-sub1 m)) '())]))

(define Ns '())
(define Rs '())

(define supercounter
  (lambda (f)
    (letrec ([S (lambda (n)
                  (if (rb6:zero? n)
                      (f n)
                      (let ()
                        (f n)
                        (S (ss-sc:my-sub1 n)))))])
      (S 1000)
      (counter))))

(define set-counter 0)
(define consC135
  (let ([N 0])
    (set! counter (lambda () N))
    (set! set-counter (lambda (x)
                        (set! N x)))
    (lambda (x y)
      (set! N (ss-sc:my-add1 N))
      (cons x y))))

; deep is not redefined on page 135, but consC was
(define (deep135 m)
  (cond [(rb6:zero? m) 'pizza]
        [else (consC135 (deep135 (ss-sc:my-sub1 m)) '())]))

(define deepM136
  (let ([Rs '()]
        [Ns '()])
    (lambda (n)
      (let ([exists (find117 n Ns Rs)])
        (if (ss-sc:atom? exists)
            (let ([result (if (rb6:zero? n)
                                'pizza
                                (consC135 (deepM136 (ss-sc:my-sub1 n)) '()))])
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

; left off page 139
(define (rember1 a l)
  (letrec ([R (lambda (l oh)
                (cond [(null? l) (oh 'no)]
                      [(ss-sc:atom? (car l))
                       (if (ss-sc:equal5? (car l) a)
                           (cdr l)
                           (cons (car l) (R (cdr l) oh)))]
                      [else (let ([new-car 
                                   (let/cc oh
                                     (R (car l) oh))])
                              (if (ss-sc:atom? new-car)
                                  (cons (car l) (R (cdr l) oh))
                                  (cons new-car (cdr l))))]))]) ; R
    (let ([new-l (let/cc oh (R l oh))])
      (if (ss-sc:atom? new-l)
          l
          new-l))))

; with our new cons
(define (rember1C a l)
  (letrec ([R (lambda (l oh)
                (cond [(null? l) (oh 'no)]
                      [(ss-sc:atom? (car l))
                       (if (ss-sc:equal5? (car l) a)
                           (cdr l)
                           (consC135 (car l) (R (cdr l) oh)))]
                      [else (let ([new-car 
                                   (let/cc oh
                                     (R (car l) oh))])
                              (if (ss-sc:atom? new-car)
                                  (consC135 (car l) (R (cdr l) oh))
                                  (consC135 new-car (cdr l))))]))]) ; R
    (let ([new-l (let/cc oh (R l oh))])
      (if (ss-sc:atom? new-l)
          l
          new-l))))

; first version of rember
(define (rember1-140 a l)
  (letrec 
      ([R (lambda (l)
            (cond [(null? l) '()]
                  [(ss-sc:atom? (car l))
                   (if (ss-sc:equal5? (car l) a)
                       (cdr l)
                       (cons (car l) (R (cdr l))))]
                  [else (let ([av (R (car l))])
                          (if (ss-sc:eqlist5? (car l) av)
                              (cons (car l) (R (cdr l)))
                              (cons av (cdr l))))]))])
      (R l)))

; with consC135
(define (rember1-140C2 a l)
  (letrec 
      ([R (lambda (l)
            (cond [(null? l) '()]
                  [(ss-sc:atom? (car l))
                   (if (ss-sc:equal5? (car l) a)
                       (cdr l)
                       (consC135 (car l) (R (cdr l))))]
                  [else (let ([av (R (car l))])
                          (if (ss-sc:eqlist5? (car l) av)
                              (consC135 (car l) (R (cdr l)))
                              (consC135 av (cdr l))))]))])
      (R l)))

(module+ test
  (require (prefix-in runit: rackunit))
  (runit:check-true #t)
  
  (ss-sc:display-all "----------------------------------------------\n"
                     "testing chapter 17 of 'The Seasoned Schemer'")
  ; (runit:check-equal? (leftmost '(((a) b) (c d))) 'a)
  (runit:check-equal? (deep132 5) '(((((pizza))))))
  ; (runit:check-equal? (counter) 5)
  (runit:check-equal? (deep132 7) '(((((((pizza))))))))
  ; (runit:check-equal? (counter) 12)

  ; (runit:check-equal? (supercounter deep132) 500512)
  (runit:check-equal? (supercounter deep135) 500500)
  (runit:check-equal? (deepM136 5) '(((((pizza))))))
  (ss-sc:display-all "Here is (counter): " (counter))
  (set-counter 0)
  (runit:check-equal? (deepM136 5) '(((((pizza))))))
  (runit:check-equal? (deepM136 5) '(((((pizza))))))
  (ss-sc:display-all "Here is (counter): " (counter))
  (runit:check-equal? (deepM136 7) '(((((((pizza))))))))
  (ss-sc:display-all "Here is (counter): " (counter)) 
  (runit:check-equal? (supercounter deepM136) 995)

  (runit:check-equal? (rember1 'salad
                               '((Swedish rye) 
                                 (French (mustard salad turkey)) salad))
                      '((Swedish rye) (French (mustard turkey)) salad))

  (runit:check-equal? (rember1 'meat
                               '((pasta meat) 
                                 pasta (noodles meat sauce) meat tomatoes))
                      '((pasta) pasta (noodles meat sauce) meat tomatoes))
  
  (runit:check-equal? (rember1 'a '((foo bar) baz))
                      '((foo bar) baz))

  (runit:check-equal? (rember1C 'salad
                               '((Swedish rye) 
                                 (French (mustard salad turkey)) salad))
                      '((Swedish rye) (French (mustard turkey)) salad))

  (runit:check-equal? (rember1C 'meat
                               '((pasta meat) 
                                 pasta (noodles meat sauce) meat tomatoes))
                      '((pasta) pasta (noodles meat sauce) meat tomatoes))
  
  (runit:check-equal? (rember1C 'a '((foo bar) baz))
                      '((foo bar) baz))
  (runit:check-equal? (rember1C 'noodles '((food) more (food)))
                      '((food) more (food)))

  (runit:check-equal? (rember1-140 'salad
                               '((Swedish rye) 
                                 (French (mustard salad turkey)) salad))
                      '((Swedish rye) (French (mustard turkey)) salad))

  (runit:check-equal? (rember1-140 'meat
                               '((pasta meat) 
                                 pasta (noodles meat sauce) meat tomatoes))
                      '((pasta) pasta (noodles meat sauce) meat tomatoes))
  
  (runit:check-equal? (rember1-140 'a '((foo bar) baz))
                      '((foo bar) baz))
  (runit:check-equal? (rember1-140 'noodles '((food) more (food)))
                      '((food) more (food)))

  (runit:check-equal? (rember1-140C2 'salad
                               '((Swedish rye) 
                                 (French (mustard salad turkey)) salad))
                      '((Swedish rye) (French (mustard turkey)) salad))

  (runit:check-equal? (rember1-140C2 'meat
                               '((pasta meat) 
                                 pasta (noodles meat sauce) meat tomatoes))
                      '((pasta) pasta (noodles meat sauce) meat tomatoes))
  
  (runit:check-equal? (rember1-140C2 'a '((foo bar) baz))
                      '((foo bar) baz))
  (runit:check-equal? (rember1-140C2 'noodles '((food) more (food)))
                      '((food) more (food)))

  (ss-sc:display-all "Here is (set-counter 0)" (set-counter 0))
  (ss-sc:display-all "Here is (counter): " (counter))

  (newline)
  (ss-sc:display-all "Here is a UUID w/gensym: " (gensym (uuid:uuid-string)))
  (ss-sc:display-all "Done with chapter 17 tests at " (ss-sc:display-date)
                     "\n---------------------------------------------------"))

