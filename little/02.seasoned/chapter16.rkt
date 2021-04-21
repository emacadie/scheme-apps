#lang racket/base

(require (prefix-in rb6:  rnrs/base-6)
         (prefix-in ris6: rnrs/io/simple-6)
         (prefix-in ss-sc: "seasoned-schemer.rkt")
         (prefix-in uuid: uuid))

(define (sweet-tooth food)
  (cons food (cons 'cake '())))

(define last 'angelfood)

(define (sweet-toothL food)
  (set! last food)
  (cons food (cons 'cake '())))

; to keep a list of ingredients
(define ingredients '())
(define (sweet-toothR food)
  (set! ingredients (cons food ingredients))
  (cons food (cons 'cake '())))

; first version
(define (deep110 m)
  (cond [(rb6:zero? m) 'pizza]
        [else (cons (deep110 (ss-sc:my-sub1 m)) '())]))

(define Ns '())
(define Rs '())
(define (deepR n)
  (let ([result (deep110 n)])
    (set! Rs (cons result Rs))
    (set! Ns (cons n Ns))
    result))

; The Nineteenth Commandment: Use (set! ... ) to remember valuable things
; between two distinct uses of a function
(define (find n Ns Rs)
  (letrec ([A (lambda (ns rs)
                (cond [(ss-sc:equal5? (car ns) n) (car rs)]
                      [else (A (cdr ns) (cdr rs))]))])
    (A Ns Rs)))

; first version
(define (deepM113 n)
  (if (ss-sc:member? n Ns)
      (find n Ns Rs)
      (deepR n)))

(define (removeDupsInNsPage114)
  ; (ss-sc:display-all "Here is Ns: " Ns ", and (cdr Ns): " (cdr Ns))
  (set! Ns (cdr Ns)))

(define (removeDupsInRsPage114)
  ; (ss-sc:display-all "Here is Rs: " Rs ", and (cdr Rs): " (cdr Rs))
  (set! Rs (cdr Rs)))

; page 114
(define (deepM114 n)
  (if (ss-sc:member? n Ns)
      (find n Ns Rs)
      (let ([result (deep110 n)])
        (set! Rs (cons result Rs))
        (set! Ns (cons n Ns))
        result)))

; page 115
(define (deep115 m)
  (cond [(rb6:zero? m) 'pizza]
        [else (cons (deepM115 (ss-sc:my-sub1 m)) '())]))
; page 115 - since deepM is now using deep115
(define (deepM115 n)
  (if (ss-sc:member? n Ns)
      (find n Ns Rs)
      (let ([result (deep115 n)])
        (set! Rs (cons result Rs))
        (set! Ns (cons n Ns))
        result)))


; page 116
;; (define (deep m)
;;   (cond [(rb6:zero? m) 'pizza]
;;         [else (cons (deepM (ss-sc:my-sub1 m)) '())]))
; sixteenth commandment
; page 116
(define (deepM116 n)
  (let ([Rs '()]
        [Ns '()])
    (if (ss-sc:member? n Ns)
      (find n Ns Rs)
      (let ([result (deep115 n)])
        (set! Rs (cons result Rs))
        (set! Ns (cons n Ns))
        result))))

(define (find117 n Ns Rs)
  (letrec ([A (lambda (ns rs)
                (cond [(null? ns) #f] 
                      [(ss-sc:equal5? (car ns) n) (car rs)]
                      [else (A (cdr ns) (cdr rs))]))])
    (A Ns Rs)))

; page 118
(define (deepM118 n)
  (let ([Rs '()]
        [Ns '()]
        [exists (find117 n Ns Rs)])
    (if (ss-sc:atom? exists)
      (let ([result (deep115 n)])
        (set! Rs (cons result Rs))
        (set! Ns (cons n Ns))
        result)
      exists
)))

; does it matter where the let for the call to find117 goes?
;; (define (deepM118 n)
;;   (let ([Rs '()]
;;         [Ns '()])
;;     (if (ss-sc:atom? (find117 n Ns Rs))
;;       (let ([result (deep115 n)])
;;         (set! Rs (cons result Rs))
;;         (set! Ns (cons n Ns))
;;         result)
;;       (find117 n Ns Rs)
;; )))

;; up to page 118
(define (my-length lat)
  (cond [(null? lat) 0]
        [else (ss-sc:my-add1 (my-length (cdr lat)))]))

;; The Seventeenth Commandment (final version): 
;; Use (set! x ...) for (let ((x ...)) ... )
;; only if there is at least one (lambda ... ) between it and the "let"
;; or if the new value for x is a function that refers to x

(define length119
  (let ([h (lambda (l) 0)])
    (set! h
          (lambda (l)
            (cond [(null? l) 0]
                  [else (ss-sc:my-add1 (h (cdr l)))])))
    h))
; if we take out the second lambda block, 
; we have something that could be reused to construct 
; a one-arg recursive function
(define L
  (lambda (lengthq) ; fake function name
    (lambda (l) ; arg
      (cond [(null? l) 0]
            [else (ss-sc:my-add1 (lengthq (cdr l)))]))))

(define length122
  (let ([h (lambda (l) 0)])
    (set! h
          (L (lambda (arg) (h arg))))
    h))

(define Y!
  (lambda (L)
    (let ([h (lambda (l) '())])
      (set! h
            (L (lambda (arg) (h arg))))
      h)))

(define Y-bang
  (lambda (f)
    (letrec 
        ([h (f (lambda (arg) (h arg)))])
      h)))

; either works
; (define length123 (Y! L))
(define length123 (Y-bang L))

(define D
  (lambda (depth-s) ; fake function name
    (lambda (s) ; arg
      (cond [(null? s) 1]
            [(ss-sc:atom? (car s)) (depth-s (cdr s))]
            [else (max (ss-sc:my-add1 (depth-s (car s)))
                       (depth-s (cdr s)))]))))

(define depth-star (Y-bang D))

(define biz
  (let ([x 0])
    (lambda (f)
      (set! x (ss-sc:my-add1 x))
      (lambda (a)
        (if (ss-sc:equal5? a x)
            0
            (f a))))))

(module+ test
  (require (prefix-in runit: rackunit))
  (runit:check-true #t)
  
  (ss-sc:display-all "----------------------------------------------\n"
                     "testing chapter 16 of 'The Seasoned Schemer'")
  ; (runit:check-equal? (leftmost '(((a) b) (c d))) 'a)
  (runit:check-equal? (sweet-tooth 'chocolate) '(chocolate cake))
  (runit:check-equal? (sweet-tooth 'fruit) '(fruit cake))
  (runit:check-equal? last 'angelfood)
  (runit:check-equal? (sweet-toothL 'chocolate) '(chocolate cake))
  (runit:check-equal? last 'chocolate)
  (runit:check-equal? (sweet-toothL 'fruit) '(fruit cake))
  (runit:check-equal? last 'fruit)
  (runit:check-equal? (sweet-toothR 'chocolate) '(chocolate cake))
  (runit:check-equal? ingredients '(chocolate))
  (runit:check-equal? (sweet-toothR 'fruit) '(fruit cake))
  (runit:check-equal? (sweet-toothR 'cheese) '(cheese cake))
  (runit:check-equal? ingredients '(cheese fruit chocolate))
  (runit:check-equal? (sweet-toothR 'carrot) '(carrot cake))
  (runit:check-equal? ingredients '(carrot cheese fruit chocolate))
  (runit:check-equal? (deep110 3) '(((pizza))))
  (runit:check-equal? (deep110 7) '(((((((pizza))))))))
  (runit:check-equal? (deepR 3) '(((pizza))))
  (runit:check-equal? Ns '(3))
  (runit:check-equal? Rs '((((pizza)))))
  (runit:check-equal? (deepR 5) '(((((pizza))))))
  (runit:check-equal? Ns '(5 3))
  (runit:check-equal? Rs '((((((pizza))))) (((pizza)))))
  (runit:check-equal? (find 3 Ns Rs) '(((pizza))))
  (runit:check-equal? (find 5 Ns Rs) '(((((pizza))))))
  (runit:check-equal? (deepR 3) '(((pizza))))

  ; we have not done (deepR 7)
  ; (runit:check-equal? (find 7 Ns Rs) '())
  ; gives the below:
; car: contract violation
;   expected: pair?
;   given: '()
; Context (plain; to see better errortrace context, re-run with C-u prefix):
;   /home/ericm/github/scheme-apps/little/02.seasoned/chapter16.rkt:39:14 A
;   (submod /home/ericm/github/scheme-apps/little/02.seasoned/chapter16.rkt test):1:0 [running body]
  ; Sort of like checking for an exception in JUnit:
  (runit:check-exn
   	exn:fail:contract?
    (lambda () (find 7 Ns Rs)
      (error "contract violation")))
  ; as we saw above, (find 3 Ns Rs) does not throw an exception
  (runit:check-not-exn
    (lambda () (find 3 Ns Rs)))

  
  (runit:check-equal? Ns '(3 5 3))
  ; remove duplicates
  (removeDupsInNsPage114)
  (removeDupsInRsPage114)
  (runit:check-equal? Ns '(5 3))
  (runit:check-equal? Rs '( (((((pizza))))) (((pizza))) ))
  
  ; page 115
  (runit:check-equal? (deepM114 6) '((((((pizza)))))))
;;   (runit:check-equal? Rs '( ((((((pizza))))))  
;;                             (((((pizza))))) 
;;                             (((pizza))) ))

  (runit:check-equal? (deepM115 9) '(((((((((pizza))))))))))
;;   (runit:check-equal? Rs '( (((((((((pizza)))))))))  
;;                             ((((((((pizza))))))))  
;;                             (((((((pizza)))))))  
;;                             ((((((pizza))))))  
;;                             (((((pizza))))) 
;;                             (((pizza))) ))

  ; page 116
  (runit:check-equal? (deepM116 16) '((((((((((((((((pizza)))))))))))))))))
  (ss-sc:display-all "Ns: " Ns)
  ; (ss-sc:display-all "Rs: " Rs)
  (runit:check-equal? (find117 3 '() '()) #f)
;;   (runit:check-equal? Ns '(15 14 13 12 11 10 9 8 7 6 5 3))
;;   (runit:check-equal? Rs '( ((((((((((((((((pizza))))))))))))))) 
;;                              ((((((((((((((pizza)))))))))))))) 
;;                              (((((((((((((pizza))))))))))))) 
;;                              ((((((((((((pizza)))))))))))) 
;;                              (((((((((((pizza))))))))))) 
;;                              ((((((((((pizza)))))))))) 
;;                              (((((((((pizza))))))))) 
;;                              ((((((((pizza)))))))) 
;;                              (((((((pizza))))))) 
;;                              ((((((pizza)))))) 
;;                              (((((pizza))))) 
;;                              (((pizza))))
;; ))
  
  (runit:check-equal? (length119 '(1 2 3 4 5)) 5)
  (runit:check-equal? (length119 '(1 2 3 4 5 6)) 6)

  (runit:check-equal? (length122 '(1 2 3 4 5)) 5)
  (runit:check-equal? (length122 '(1 2 3 4 5 6)) 6)

  (runit:check-equal? (length123 '(1 2 3 4 5)) 5)
  (runit:check-equal? (length123 '(1 2 3 4 5 6)) 6)

   (runit:check-equal? (depth-star '((pickled) peppers (peppers pickled)))
                      2)
   (runit:check-equal? (depth-star '(margarine 
                                ((bitter butter) 
                                 (makes)
                                 (batter (bitter))) butter))
                      4)
   (runit:check-equal? (depth-star '(c (b (a b) a) a))
                      3)

  (newline)
  (ss-sc:display-all "Here is a UUID w/gensym: " (gensym (uuid:uuid-string)))
  (ss-sc:display-all "Done with chapter 16 tests at " (ss-sc:display-date)
                     "\n---------------------------------------------------"))

