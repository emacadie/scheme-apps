#lang simply-scheme

; Chapter 19 Higher Order Functions

(require "more-simply.rkt")
(require (prefix-in ch17: "chapter17.rkt"))


(butfirst '(This is chapter 19 Higher Order Functions))

;; Chapter 19 Higher Order Functions

;; from chapter 14, tail-recursive
;; (every-something-r (lambda (x) (* 2 x)) '(1 2 3) '())
(define (my-map func sent)
  (my-map-r func sent '()))
(define (my-map-r func sent outp)
  (cond [(null? sent) (reverse outp)]
        [else (my-map-r func 
                        (cdr sent) 
                        (cons (func (car sent)) outp))]))

#| 
(define (map fn lst)
  (if (null? lst)
      '()
      (cons (fn (car lst)) (map fn (cdr lst)))))
|#

(define (every2 func sent outp)
  (cond [(null? sent) outp]
        [(empty? outp) (every2 func 
                               (cdr sent) 
                               (list (func (car sent))))]
        [else (begin
                (printf "in else, with car sent: ~a \n" (car sent))
                (every2 func 
                        (cdr sent) 
                        (ch17:my-append outp 
                                        (func (car sent)))))]))

; from chapter 14 - tail recursive
(define (my-keep predicate sent)
  (keep-r predicate sent '()))
(define (keep-r predicate sent outp)
  (cond [(empty? sent) outp] 
        [(and (predicate (car sent)) (empty? outp)) (keep-r predicate 
                                                            (cdr sent)
                                                            (list (car sent)))]
	    [(predicate (car sent)) (keep-r predicate 
                                        (cdr sent) 
                                        (ch17:my-append outp 
                                                        (car sent)))]
	    [else (keep-r predicate 
                      (cdr sent) 
                      outp)]))

; from chapter 14, tail-recursive
(define (my-reduce func start sent)
  (cond [(empty? sent) start]
        [else (my-reduce func
                         (func start (car sent))
                         (cdr sent) )]))


;; 19.2  Write keep. Don't forget that keep has to return a sentence if 
;; its second argument is a sentence, and a word if its second argument is a word.

;; (Hint: it might be useful to write a combine procedure 
;; that uses either word or sentence depending on the types of its arguments.) 
;; from chapter 14
(define (keep-ch19 the-pred sent)
  (cond [(word? sent) (reduce word (keep-ch19-work the-pred sent))]
        [else (keep-ch19-work the-pred sent)]))

(define (keep-ch19-work the-pred sent)
  (cond [(empty? sent) '()]
        [(the-pred (first sent)) (se (first sent) 
                                     (keep-ch19-work the-pred 
                                                     (bf sent)))]
        [else (keep-ch19-work the-pred 
                              (bf sent))]))

(module+ test
  (require rackunit)
  (check-true #t)
  (define (check-three-things-equal? result their-append-rsl my-append-rsl)
  (unless (and (check-equal? result their-append-rsl)
               (check-equal? result my-append-rsl))
    (fail-check)))
  (check-three-things-equal? (my-map (lambda (x) (* 2 x)) '(1 2 3) ) 
                             (map (lambda (x) (* 2 x)) '(1 2 3) ) 
                             '(2 4 6))
  (check-three-things-equal? (my-map (lambda (n) (expt n n)) '(1 2 3 4 5))
                             (map (lambda (n) (expt n n)) '(1 2 3 4 5))
                             '(1 4 27 256 3125))
  (check-three-things-equal? (my-keep even? '(1 2 3 4 5 6)) 
                             (filter even? '(1 2 3 4 5 6))
                             '(2 4 6))
  (check-three-things-equal? (my-reduce max 0 '(23 54 45 85 65)) 
                             (reduce max '(23 54 45 85 65))
                             85)
  (check-three-things-equal? (my-reduce * 1 '(1 2 3 4 5))
                             (reduce * '(1 2 3 4 5))
                             120)

  (check-equal? 'ei (keep-ch19 vowel? 'qerti))
  (check-equal? '(e u) (keep-ch19 vowel? '(q w e r t u)))

) ;; end module+ test 
  ; (printf ": ~a \n"  )
  ; (check-equal?  "Error for: ")

