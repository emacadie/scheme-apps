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
;; (define (deep m)
;;   (cond [(rb6:zero? m) 'pizza]
;;         [else (cons (deep (ss-sc:my-sub1 m)) '())]))

(define Ns '())
(define Rs '())
(define (deepR n)
  (let ([result (deep n)])
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
;; (define (deepM n)
;;   (if (ss-sc:member? n Ns)
;;       (find n Ns Rs)
;;       (deepR n)))

(define (removeDupsInNsPage114)
  ; (ss-sc:display-all "Here is Ns: " Ns ", and (cdr Ns): " (cdr Ns))
  (set! Ns (cdr Ns)))

(define (removeDupsInRsPage114)
  ; (ss-sc:display-all "Here is Rs: " Rs ", and (cdr Rs): " (cdr Rs))
  (set! Rs (cdr Rs)))

; page 114
;; (define (deepM n)
;;   (if (ss-sc:member? n Ns)
;;       (find n Ns Rs)
;;       (let ([result (deep n)])
;;         (set! Rs (cons result Rs))
;;         (set! Ns (cons n Ns))
;;         result)))

; page 115
;; (define (deepM115 n)
;;   (if (ss-sc:member? n Ns)
;;       (find n Ns Rs)
;;       (let ([result (deep115 n)])
;;         (set! Rs (cons result Rs))
;;         (set! Ns (cons n Ns))
;;         result)))

; page 115
(define (deep m)
  (cond [(rb6:zero? m) 'pizza]
        [else (cons (deepM (ss-sc:my-sub1 m)) '())]))

; page 116
;; (define (deep116 m)
;;   (cond [(rb6:zero? m) 'pizza]
;;         [else (cons (deepM116 (ss-sc:my-sub1 m)) '())]))
; sixteenth commandment
(define (deepM116 n)
  (let ([Rs '()]
        [Ns '()])
    (if (ss-sc:member? n Ns)
      (find n Ns Rs)
      (let ([result (deep116 n)])
        (set! Rs (cons result Rs))
        (set! Ns (cons n Ns))
        result))))

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
  (runit:check-equal? (deep 3) '(((pizza))))
  (runit:check-equal? (deep 7) '(((((((pizza))))))))
  (runit:check-equal? (deepR 3) '(((pizza))))
  (runit:check-equal? Ns '(3))
  (runit:check-equal? Rs '((((pizza)))))
  (runit:check-equal? (deepR 5) '(((((pizza))))))
  (runit:check-equal? Ns '(5 3))
  (runit:check-equal? Rs '((((((pizza))))) (((pizza)))))
  (runit:check-equal? (find 3 Ns Rs) '(((pizza))))
  (runit:check-equal? (find 5 Ns Rs) '(((((pizza))))))
  

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

  (runit:check-equal? (deepR 3) '(((pizza))))
  (runit:check-equal? Ns '(3 5 3))
  ; remove duplicates
  (removeDupsInNsPage114)
  (removeDupsInRsPage114)
  (runit:check-equal? Ns '(5 3))
  (runit:check-equal? Rs '( (((((pizza))))) (((pizza))) ))
  
  (runit:check-equal? (deepM114 6) '((((((pizza)))))))
  (runit:check-equal? Rs '( ((((((pizza))))))  
                            (((((pizza))))) 
                            (((pizza))) ))

  (runit:check-equal? (deepM115 9) '(((((((((pizza))))))))))
  (runit:check-equal? Rs '( (((((((((pizza)))))))))  
                            ((((((((pizza))))))))  
                            (((((((pizza)))))))  
                            ((((((pizza))))))  
                            (((((pizza))))) 
                            (((pizza))) ))

  (runit:check-equal? (deepM116 16) '((((((((((((((((pizza)))))))))))))))))
  (runit:check-equal? Ns '(15 14 13 12 11 10 9 8 7 6 5 3))
  (runit:check-equal? Rs '( ((((((((((((((((pizza))))))))))))))) 
                             ((((((((((((((pizza)))))))))))))) 
                             (((((((((((((pizza))))))))))))) 
                             ((((((((((((pizza)))))))))))) 
                             (((((((((((pizza))))))))))) 
                             ((((((((((pizza)))))))))) 
                             (((((((((pizza))))))))) 
                             ((((((((pizza)))))))) 
                             (((((((pizza))))))) 
                             ((((((pizza)))))) 
                             (((((pizza))))) 
                             (((pizza))))
))

  (newline)
  (ss-sc:display-all "Here is a UUID w/gensym: " (gensym (uuid:uuid-string)))
  (ss-sc:display-all "Done with chapter 16 tests at " (ss-sc:display-date)
                     "\n---------------------------------------------------"))

